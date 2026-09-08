package org.podval.xml

import zio.blocks.schema.*
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset
import zio.blocks.typeid.TypeId
import scala.annotation.switch
import scala.collection.mutable
import scala.reflect.ClassTag

/** Record codec, field layout, and register load/store. */
private[xml] trait XmlCodecRecord:
  this: XmlCodecDeriver =>

  protected val recursiveRecordCache: ThreadLocal[java.util.HashMap[TypeId[?], Array[FieldInfo]]] =
    new ThreadLocal[java.util.HashMap[TypeId[?], Array[FieldInfo]]]:
      override def initialValue: java.util.HashMap[TypeId[?], Array[FieldInfo]] = new java.util.HashMap

  protected final class RecordCodec[A](
    typeId: TypeId[A],
    modifiers: Seq[Modifier.Reflect],
    fieldInfos: Array[FieldInfo],
    constructor: Constructor[A],
    deconstructor: Deconstructor[A],
    xmlTag: Option[XmlTag[Any]]
  ) extends XmlCodec[A]:
    private val recordName: String = configuredElementName(typeId.name, Seq.empty, modifiers)
    private val namespace: Option[(String, String)] = namespaceOf(modifiers)
    // Recursive records cache this array before every slot is filled; look up after derivation.
    private lazy val tagField: Option[FieldInfo] = fieldInfos.find(_.kind == FieldKind.Tag)

    override def elementName: String = xmlTag.flatMap(_.names.headOption).getOrElse(recordName)
    override def caseNames: Seq[String] = xmlTag.fold(Seq.empty)(_.names)
    override def isRecordLike: Boolean = true

    override def elementNameOf(value: A): String =
      (xmlTag, tagField) match
        case (Some(tag), Some(info)) =>
          val regs: Registers = Registers(deconstructor.usedRegisters)
          deconstructor.deconstruct(regs, 0, value)
          tag.toName(load(regs, info.offset, info.typeTag))
        case _ => recordName

    override def unsafeDecode[E: XmlAst](element: E): A =
      val ast: XmlAst[E] = summon[XmlAst[E]]
      val attrs: mutable.LinkedHashMap[XmlName, String] =
        mutable.LinkedHashMap.from(element.getAttributes)
      val nodes: ast.Nodes = ast.getChildren(element)
      val available: mutable.BitSet = mutable.BitSet.empty
      nodes.zipWithIndex.foreach: (node, idx) =>
        if node.asElement.isDefined then available += idx
      val regs: Registers = Registers(constructor.usedRegisters)
      fieldInfos.foreach: info =>
        try
          info.kind match
            case FieldKind.Tag =>
              val name: XmlName = element.getName
              xmlTag.flatMap(tag => tag.fromName(name.localName).orElse(tag.fromName(name.qName))) match
                case Some(k) => store(regs, info.offset, info.typeTag, k)
                case None => throw XmlError(s"Unknown element: ${name.qName}")
            case FieldKind.Text =>
              val text: String = characterData(element)
              val value: Any =
                if info.optional then
                  if text.isEmpty then None else Some(info.codec.unsafeDecodeText(text))
                else if text.isEmpty then info.defaultValue.getOrElse(throw XmlError("Missing text content"))
                else info.codec.unsafeDecodeText(text)
              store(regs, info.offset, info.typeTag, value)
            case FieldKind.Attribute(attrName) =>
              val wanted: XmlName = XmlName.parse(attrName, isAttribute = true)
              attrs.collectFirst:
                case (key, value) if key.sameAs(wanted) || key.matches(attrName) => (key, value)
              match
                case Some((_, raw)) =>
                  attrs.filterInPlace((n, _) => !(n.sameAs(wanted) || n.matches(attrName)))
                  val decoded: Any = info.codec.unsafeDecodeText(raw)
                  store(regs, info.offset, info.typeTag, if info.optional then Some(decoded) else decoded)
                case None =>
                  if info.optional then store(regs, info.offset, info.typeTag, None)
                  else info.defaultValue match
                    case Some(dv) => store(regs, info.offset, info.typeTag, dv)
                    case None => throw XmlError(s"Missing required attribute: $attrName")
            case FieldKind.Child =>
              val matched: Seq[(E, Int)] = nodes.zipWithIndex.flatMap: (node, nodeIdx) =>
                if !available.contains(nodeIdx) then None
                else node.asElement.filter(el => info.itemNames.exists(el.getName.matches)).map(_ -> nodeIdx)
              if info.sequence then
                val decodedItems: Seq[Any] = matched.map: (el, nodeIdx) =>
                  available -= nodeIdx
                  info.codec.unsafeDecode(el)
                val seqValue: Any = buildSeq(info, decodedItems)
                val stored: Any =
                  if info.optional then
                    if decodedItems.isEmpty then None else Some(seqValue)
                  else seqValue
                store(regs, info.offset, info.typeTag, stored)
              else if info.optional then
                matched.headOption match
                  case Some((el, nodeIdx)) =>
                    available -= nodeIdx
                    store(regs, info.offset, info.typeTag, Some(info.codec.unsafeDecode(el)))
                  case None => store(regs, info.offset, info.typeTag, None)
              else
                matched.headOption match
                  case Some((el, nodeIdx)) =>
                    available -= nodeIdx
                    store(regs, info.offset, info.typeTag, info.codec.unsafeDecode(el))
                  case None =>
                    info.defaultValue match
                      case Some(dv) => store(regs, info.offset, info.typeTag, dv)
                      case None => throw XmlError(s"Missing required element: ${info.itemNames.mkString("|")}")
        catch
          case e: XmlError => throw XmlError(s"${info.fieldName}: ${e.getMessage}")

      val leftoverAttrs: Seq[String] =
        attrs.keys.iterator.filterNot(_.isXmlnsDeclaration).map(_.qName).toSeq
      if leftoverAttrs.nonEmpty then throw XmlError(s"Unparsed attributes: ${leftoverAttrs.mkString(", ")}")
      val leftoverElements: Seq[String] = nodes.zipWithIndex.flatMap: (node, nodeIdx) =>
        if available.contains(nodeIdx) then node.asElement.map(_.localName) else None
      if leftoverElements.nonEmpty then throw XmlError(s"Unparsed elements: ${leftoverElements.mkString(", ")}")
      val leftoverText: Boolean = nodes.zipWithIndex.exists: (node, nodeIdx) =>
        !available.contains(nodeIdx) &&
          node.asElement.isEmpty &&
          node.asAtom.exists(_.trim.nonEmpty) &&
          !fieldInfos.exists(_.kind == FieldKind.Text)
      if leftoverText then throw XmlError("Unparsed character content")
      constructor.construct(regs, 0)

    override def encodeNamed[E: XmlAst](name: String, value: A): E =
      val ast: XmlAst[E] = summon[XmlAst[E]]
      val regs: Registers = Registers(deconstructor.usedRegisters)
      deconstructor.deconstruct(regs, 0, value)
      val attributes: mutable.ArrayBuffer[(XmlName, String)] = mutable.ArrayBuffer.empty
      val children: mutable.ArrayBuffer[ast.Node] = mutable.ArrayBuffer.empty
      fieldInfos.foreach: info =>
        info.kind match
          case FieldKind.Tag => ()
          case FieldKind.Text =>
            val loaded: Any = load(regs, info.offset, info.typeTag)
            val textOpt: Option[String] =
              if info.optional then loaded.asInstanceOf[Option[Any]].map(info.codec.encodeText)
              else Some(info.codec.encodeText(loaded))
            textOpt.filter(_.nonEmpty).foreach(text => children += ast.text(text))
          case FieldKind.Attribute(attrName) =>
            val loaded: Any = load(regs, info.offset, info.typeTag)
            val raw: Option[String] =
              if info.optional then loaded.asInstanceOf[Option[Any]].map(info.codec.encodeText)
              else Some(info.codec.encodeText(loaded))
            raw.foreach: value =>
              attributes += XmlName.parse(attrName, isAttribute = true) -> value
          case FieldKind.Child =>
            val loaded: Any = load(regs, info.offset, info.typeTag)
            def appendItem(item: Any): Unit =
              val encoded: E =
                if info.codec.caseNames.nonEmpty then info.codec.encode(item)
                else info.codec.encodeNamed(info.itemNames.head, item)
              children += encoded
            if info.sequence then
              val items: Iterator[Any] =
                if info.optional then
                  loaded.asInstanceOf[Option[Any]] match
                    case Some(seq) => deconstructSeq(info, seq)
                    case None => Iterator.empty
                else deconstructSeq(info, loaded)
              items.foreach(appendItem)
            else if info.optional then
              loaded.asInstanceOf[Option[Any]].foreach(appendItem)
            else appendItem(loaded)
      val nsAttrs: Seq[(XmlName, String)] = namespace match
        case Some((uri, prefix)) if prefix.nonEmpty => Seq(XmlName.xmlnsAttribute(Some(prefix), uri))
        case Some((uri, _)) => Seq(XmlName.xmlnsAttribute(None, uri))
        case None => Seq.empty
      val parsedName: XmlName = XmlName.parseQName(name)
      val expandedName: XmlName = namespace match
        case Some((uri, prefix)) if prefix.nonEmpty && parsedName.prefix.isEmpty =>
          XmlName(parsedName.localName, Some(XmlNamespace(uri, Some(prefix))))
        case Some((uri, _)) if parsedName.prefix.isEmpty =>
          XmlName(parsedName.localName, Some(XmlNamespace(uri, None)))
        case Some((uri, _)) => parsedName.copy(namespace = XmlNamespace.of(parsedName.prefix, Some(uri)))
        case None =>
          XmlName.parseDeclared(name, nsAttrs ++ attributes.toSeq, isAttribute = false)
      ast.element(expandedName, nsAttrs ++ attributes.toSeq, children.toSeq)

  protected def fieldInfo[F[_, _], A](recordTypeId: TypeId[A], field: Term[F, A, ?], offset: RegisterOffset)(using
    F: HasBinding[F],
    D: HasInstance[F]
  ): FieldInfo =
    val reflect: Reflect[F, ?] = field.value
    val optional: Boolean = reflect.isOption
    val innerReflect: Reflect[F, ?] =
      if optional then reflect.optionInnerType.getOrElse(reflect) else reflect
    val sequence: Boolean = innerReflect.isSequence
    val itemReflect: Reflect[F, ?] =
      if sequence then innerReflect.asSequenceUnknown.get.sequence.element else innerReflect
    val codec: XmlCodec[Any] = D.instance(itemReflect.metadata).force.asInstanceOf[XmlCodec[Any]]
    val kind: FieldKind =
      if tagBinding(recordTypeId).exists(_._1 == field.name) then FieldKind.Tag
      else configValue(field.modifiers, XmlCodec.Attribute) match
        case Some(attr) => FieldKind.Attribute(if attr.isEmpty then field.name else attr)
        case None if configValue(field.modifiers, XmlCodec.Text).isDefined => FieldKind.Text
        case None => FieldKind.Child
    val itemName: String =
      configValue(field.modifiers, XmlCodec.Element).filter(_.nonEmpty)
        .orElse(renameOf(field.modifiers))
        .getOrElse:
          if codec.caseNames.nonEmpty then field.name
          else if codec.isRecordLike then codec.elementName
          else field.name
    val aliases: Seq[String] = field.modifiers.collect { case Modifier.alias(name) => name }
    val itemNames: Seq[String] =
      if codec.caseNames.nonEmpty then codec.caseNames else (itemName +: aliases).distinct
    val seqParts: Option[SeqParts] =
      if !sequence then None
      else
        val seqReflect = innerReflect.asSequenceUnknown.get.sequence
        val ctor = seqReflect.seqConstructor
        val dector = seqReflect.seqDeconstructor
        val classTag: ClassTag[Any] = seqReflect.elemClassTag.asInstanceOf[ClassTag[Any]]
        Some(SeqParts(
          fromItems = items =>
            val builder = ctor.newBuilder[Any](items.size)(using classTag)
            items.foreach(item => ctor.add(builder, item))
            ctor.result(builder),
          toItems = value => dector.asInstanceOf[SeqDeconstructor[Seq]].deconstruct(value.asInstanceOf[Seq[Any]])
        ))
    FieldInfo(
      fieldName = field.name,
      kind = kind,
      optional = optional,
      sequence = sequence,
      codec = codec,
      itemNames = itemNames,
      defaultValue = reflect.getDefaultValue,
      offset = offset,
      typeTag = typeTagOf(reflect),
      seqParts = seqParts
    )

  protected def buildSeq(info: FieldInfo, items: Seq[Any]): Any =
    info.seqParts.get.fromItems(items)

  protected def deconstructSeq(info: FieldInfo, value: Any): Iterator[Any] =
    info.seqParts.get.toItems(value)

  protected def characterData[E: XmlAst](element: E): String =
    val ast: XmlAst[E] = summon[XmlAst[E]]
    ast.getChildren(element).flatMap(node => ast.asAtom(node)).mkString.trim

  protected def configValue(modifiers: Seq[Modifier], key: String): Option[String] =
    modifiers.collectFirst { case Modifier.config(`key`, value) => value }

  protected def renameOf(modifiers: Seq[Modifier.Term]): Option[String] =
    modifiers.collectFirst { case Modifier.rename(name) => name }

  protected def configuredElementName(
    defaultName: String,
    termModifiers: Seq[Modifier.Term],
    reflectModifiers: Seq[Modifier.Reflect]
  ): String =
    configValue(termModifiers, XmlCodec.Element).filter(_.nonEmpty)
      .orElse(renameOf(termModifiers))
      .orElse(configValue(reflectModifiers, XmlCodec.Element).filter(_.nonEmpty))
      .getOrElse(defaultName)

  protected def namespaceOf(modifiers: Seq[Modifier.Reflect]): Option[(String, String)] =
    configValue(modifiers, XmlCodec.NamespaceUri).map: uri =>
      (uri, configValue(modifiers, XmlCodec.NamespacePrefix).getOrElse(""))

  protected def typeTagOf[F[_, _], A](reflect: Reflect[F, A]): Int =
    reflect.asPrimitive.map(_.primitiveType) match
      case Some(_: PrimitiveType.Unit.type) => 9
      case Some(_: PrimitiveType.Boolean) => 5
      case Some(_: PrimitiveType.Byte) => 6
      case Some(_: PrimitiveType.Char) => 7
      case Some(_: PrimitiveType.Short) => 8
      case Some(_: PrimitiveType.Float) => 3
      case Some(_: PrimitiveType.Int) => 1
      case Some(_: PrimitiveType.Double) => 4
      case Some(_: PrimitiveType.Long) => 2
      case _ => 0

  protected def registerOffset[F[_, _], A](reflect: Reflect[F, A]): RegisterOffset =
    reflect.asPrimitive.map(_.primitiveType) match
      case Some(_: PrimitiveType.Unit.type) => 0L
      case Some(_: PrimitiveType.Boolean) => RegisterOffset.incrementBooleansAndBytes(0L)
      case Some(_: PrimitiveType.Byte) => RegisterOffset.incrementBooleansAndBytes(0L)
      case Some(_: PrimitiveType.Char) => RegisterOffset.incrementCharsAndShorts(0L)
      case Some(_: PrimitiveType.Short) => RegisterOffset.incrementCharsAndShorts(0L)
      case Some(_: PrimitiveType.Float) => RegisterOffset.incrementFloatsAndInts(0L)
      case Some(_: PrimitiveType.Int) => RegisterOffset.incrementFloatsAndInts(0L)
      case Some(_: PrimitiveType.Double) => RegisterOffset.incrementDoublesAndLongs(0L)
      case Some(_: PrimitiveType.Long) => RegisterOffset.incrementDoublesAndLongs(0L)
      case _ => RegisterOffset.incrementObjects(0L)

  protected def store(regs: Registers, offset: RegisterOffset, tag: Int, value: Any): Unit =
    (tag: @switch) match
      case 1 => regs.setInt(offset, value.asInstanceOf[Int])
      case 2 => regs.setLong(offset, value.asInstanceOf[Long])
      case 3 => regs.setFloat(offset, value.asInstanceOf[Float])
      case 4 => regs.setDouble(offset, value.asInstanceOf[Double])
      case 5 => regs.setBoolean(offset, value.asInstanceOf[Boolean])
      case 6 => regs.setByte(offset, value.asInstanceOf[Byte])
      case 7 => regs.setChar(offset, value.asInstanceOf[Char])
      case 8 => regs.setShort(offset, value.asInstanceOf[Short])
      case 9 => ()
      case _ => regs.setObject(offset, value.asInstanceOf[AnyRef])

  protected def load(regs: Registers, offset: RegisterOffset, tag: Int): Any =
    (tag: @switch) match
      case 1 => regs.getInt(offset)
      case 2 => regs.getLong(offset)
      case 3 => regs.getFloat(offset)
      case 4 => regs.getDouble(offset)
      case 5 => regs.getBoolean(offset)
      case 6 => regs.getByte(offset)
      case 7 => regs.getChar(offset)
      case 8 => regs.getShort(offset)
      case 9 => ()
      case _ => regs.getObject(offset)

  protected enum FieldKind derives CanEqual:
    case Attribute(name: String)
    case Text
    case Child
    case Tag

  protected final class FieldInfo(
    val fieldName: String,
    val kind: FieldKind,
    val optional: Boolean,
    val sequence: Boolean,
    val codec: XmlCodec[Any],
    val itemNames: Seq[String],
    val defaultValue: Option[Any],
    val offset: RegisterOffset,
    val typeTag: Int,
    val seqParts: Option[SeqParts]
  )

  protected final class SeqParts(
    val fromItems: Seq[Any] => Any,
    val toItems: Any => Iterator[Any]
  )
