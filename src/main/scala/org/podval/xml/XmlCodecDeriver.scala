package org.podval.xml

import org.podval.metadata.Name
import zio.blocks.chunk.Chunk
import zio.blocks.docs.Doc
import zio.blocks.schema.*
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset
import zio.blocks.schema.derive.{BindingInstance, Deriver, InstanceOverride, InstanceOverrideByType}
import zio.blocks.typeid.TypeId
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

object XmlCodecDeriver extends XmlCodecDeriver:
  private val taggedCodecs: ConcurrentHashMap[TypeId[?], Lazy[XmlCodec[?]]] = ConcurrentHashMap()

  private[xml] def register[A](typeId: TypeId[A], codec: XmlCodec[A]): Unit =
    taggedCodecs.put(typeId, Lazy(codec))

  private[xml] def registerTagged[A](typeId: TypeId[A], codec: XmlCodec[A]): Unit =
    register(typeId, codec)

  def tagged[A, K](tagField: String, tag: XmlTag[K])(using typeId: TypeId[A]): XmlCodecDeriver =
    val target: String = typeId.fullName
    val xmlTag: XmlTag[Any] = tag.erased
    new XmlCodecDeriver:
      override protected def tagBinding(id: TypeId[?]): Option[(String, XmlTag[Any])] =
        Option.when(id.fullName == target)((tagField, xmlTag))

class XmlCodecDeriver extends Deriver[XmlCodec], XmlCodecRecord:
  protected def tagBinding(typeId: TypeId[?]): Option[(String, XmlTag[Any])] = None

  override def derivePrimitive[A](
    primitiveType: PrimitiveType[A],
    typeId: TypeId[A],
    binding: Binding.Primitive[A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  ): Lazy[XmlCodec[A]] =
    if binding.isInstanceOf[Binding[?, ?]] then Lazy(primitiveCodec(primitiveType))
    else binding.asInstanceOf[BindingInstance[XmlCodec, ?, A]].instance

  override def deriveRecord[F[_, _], A](
    fields: IndexedSeq[Term[F, A, ?]],
    typeId: TypeId[A],
    binding: Binding.Record[A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[A]] =
    if !binding.isInstanceOf[Binding[?, ?]] then binding.asInstanceOf[BindingInstance[XmlCodec, ?, A]].instance
    else Lazy:
      val recordBinding: Binding.Record[A] = binding
      val isRecursive: Boolean = fields.exists(_.value.isInstanceOf[Reflect.Deferred[F, ?]])
      var fieldInfos: Array[FieldInfo] =
        if isRecursive then recursiveRecordCache.get.get(typeId) else null
      val deriveCodecs: Boolean = fieldInfos eq null
      if deriveCodecs then
        fieldInfos = new Array[FieldInfo](fields.length)
        if isRecursive then recursiveRecordCache.get.put(typeId, fieldInfos)
        var offset: RegisterOffset = 0L
        fields.zipWithIndex.foreach: (field, idx) =>
          fieldInfos(idx) = fieldInfo(typeId, field, offset)
          offset = RegisterOffset.add(registerOffset(field.value), offset)
      new RecordCodec[A](
        typeId = typeId,
        modifiers = modifiers,
        fieldInfos = fieldInfos,
        constructor = recordBinding.constructor,
        deconstructor = recordBinding.deconstructor,
        xmlTag = tagBinding(typeId).map(_._2)
      )

  override def deriveVariant[F[_, _], A](
    cases: IndexedSeq[Term[F, A, ?]],
    typeId: TypeId[A],
    binding: Binding.Variant[A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[A]] =
    if !binding.isInstanceOf[Binding[?, ?]] then binding.asInstanceOf[BindingInstance[XmlCodec, ?, A]].instance
    else if typeId.isOption then
      val inner: Reflect[F, ?] = cases(1).value.asRecord.get.fields(0).value
      D.instance(inner.metadata).map: codec =>
        val innerCodec: XmlCodec[Any] = codec.asInstanceOf[XmlCodec[Any]]
        new XmlCodec[Option[Any]]:
          override def elementName: String = innerCodec.elementName
          override def isRecordLike: Boolean = innerCodec.isRecordLike
          override def caseNames: Seq[String] = innerCodec.caseNames
          override def isEnumeration: Boolean = innerCodec.isEnumeration
          override def unsafeDecode(element: Xml.Element): Option[Any] =
            Some(innerCodec.unsafeDecode(element))
          override def encodeNamed(name: String, value: Option[Any]): Xml.Element = value match
            case Some(innerValue) => innerCodec.encodeNamed(name, innerValue)
            case None => throw XmlError("Cannot encode None as an element")
          override def unsafeDecodeText(text: String): Option[Any] =
            Some(innerCodec.unsafeDecodeText(text))
          override def encodeText(value: Option[Any]): String = value match
            case Some(innerValue) => innerCodec.encodeText(innerValue)
            case None => throw XmlError("Cannot encode None as text")
        .asInstanceOf[XmlCodec[A]]
    else Lazy:
      val caseCodecs: IndexedSeq[(String, XmlCodec[A], Option[A])] = cases.map: caseTerm =>
        val name: String = configuredElementName(caseTerm.name, caseTerm.modifiers, caseTerm.value.modifiers)
        val codec: XmlCodec[A] = D.instance(caseTerm.value.metadata).force.asInstanceOf[XmlCodec[A]]
        val empty: Option[A] = caseTerm.value.asRecord.filter(_.fields.isEmpty).map: record =>
          val ctor: Constructor[?] = F.record(record.recordBinding).constructor
          ctor.construct(Registers(ctor.usedRegisters), 0).asInstanceOf[A]
        (name, codec, empty)
      val enumeration: Boolean = caseCodecs.forall(_._3.isDefined)
      val discriminator: Discriminator[A] = binding.discriminator
      def caseByName(name: XmlName): Option[(String, XmlCodec[A], Option[A])] =
        caseCodecs.find((caseName, _, _) => name.matches(caseName))
      new XmlCodec[A]:
        override def elementName: String = configuredElementName(typeId.name, Seq.empty, modifiers)
        override def isRecordLike: Boolean = true
        override def isEnumeration: Boolean = enumeration
        override def caseNames: Seq[String] = caseCodecs.map(_._1)
        override def unsafeDecode(element: Xml.Element): A =
          val name: XmlName = element.getName
          caseByName(name) match
            case Some((_, codec, empty)) =>
              empty.getOrElse(codec.unsafeDecode(element))
            case None if enumeration =>
              unsafeDecodeText(characterData(element))
            case None => throw XmlError(s"Unknown variant case: ${name.qName}")
        override def encodeNamed(name: String, value: A): Xml.Element =
          val idx: Int = discriminator.discriminate(value)
          val (caseName, codec, _) = caseCodecs(idx)
          if enumeration then Xml.element(name, Seq.empty, Seq(Xml.text(caseName)))
          else codec.encodeNamed(caseName, value)
        override def encode(value: A): Xml.Element =
          val idx: Int = discriminator.discriminate(value)
          val (caseName, codec, _) = caseCodecs(idx)
          if enumeration then Xml.element(caseName)
          else codec.encodeNamed(caseName, value)
        override def unsafeDecodeText(text: String): A =
          if !enumeration then throw XmlError("Variant does not decode from text")
          caseCodecs.find((caseName, _, _) => caseName == text.trim) match
            case Some((_, _, Some(value))) => value
            case _ => throw XmlError(s"Unknown enumeration value: $text")
        override def encodeText(value: A): String =
          if !enumeration then throw XmlError("Variant does not encode as text")
          caseCodecs(discriminator.discriminate(value))._1

  override def deriveSequence[F[_, _], C[_], A](
    element: Reflect[F, A],
    typeId: TypeId[C[A]],
    binding: Binding.Seq[C, A],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[C[A]],
    examples: Seq[C[A]]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[C[A]]] =
    if !binding.isInstanceOf[Binding[?, ?]] then binding.asInstanceOf[BindingInstance[XmlCodec, ?, C[A]]].instance
    else
      val seqBinding: Binding.Seq[C, A] = binding
      val itemClassTag: ClassTag[A] = element.typeId.classTag.asInstanceOf[ClassTag[A]]
      D.instance(element.metadata).map: codec =>
        val itemCodec: XmlCodec[A] = codec
        new XmlCodec[C[A]]:
          override def elementName: String = typeId.name
          override def unsafeDecode(root: Xml.Element): C[A] =
            val children: Seq[Xml.Element] = root.childElements
            val names: Seq[String] =
              if itemCodec.caseNames.nonEmpty then itemCodec.caseNames else Seq(itemCodec.elementName)
            val matched: Seq[Xml.Element] =
              if itemCodec.isRecordLike then children.filter(child => child.getName.matchesAny(names))
              else children
            val builder = seqBinding.constructor.newBuilder[A](matched.size)(using itemClassTag)
            matched.foreach: child =>
              seqBinding.constructor.add(builder, itemCodec.unsafeDecode(child))
            seqBinding.constructor.result(builder)
          override def encodeNamed(name: String, value: C[A]): Xml.Element =
            val items: Iterator[A] = seqBinding.deconstructor.deconstruct(value)
            val children: Seq[XmlNode] = items.map: item =>
              if itemCodec.caseNames.nonEmpty then itemCodec.encode(item)
              else itemCodec.encodeNamed(itemCodec.elementName, item)
            .toSeq
            Xml.element(name, Seq.empty, children)
          override def encode(value: C[A]): Xml.Element = encodeNamed(elementName, value)

  override def deriveMap[F[_, _], M[_, _], K, V](
    key: Reflect[F, K],
    value: Reflect[F, V],
    typeId: TypeId[M[K, V]],
    binding: Binding.Map[M, K, V],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[M[K, V]],
    examples: Seq[M[K, V]]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[M[K, V]]] =
    Lazy(unsupported(s"Map ${typeId.name}"))

  override def deriveDynamic[F[_, _]](
    binding: Binding.Dynamic,
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[DynamicValue],
    examples: Seq[DynamicValue]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[DynamicValue]] =
    Lazy(unsupported("DynamicValue"))

  override def deriveWrapper[F[_, _], A, B](
    wrapped: Reflect[F, B],
    typeId: TypeId[A],
    binding: Binding.Wrapper[A, B],
    doc: Doc,
    modifiers: Seq[Modifier.Reflect],
    defaultValue: Option[A],
    examples: Seq[A]
  )(using F: HasBinding[F], D: HasInstance[F]): Lazy[XmlCodec[A]] =
    if !binding.isInstanceOf[Binding[?, ?]] then binding.asInstanceOf[BindingInstance[XmlCodec, ?, A]].instance
    else
      val wrapperBinding: Binding.Wrapper[A, B] = binding
      D.instance(wrapped.metadata).map: codec =>
        val inner: XmlCodec[B] = codec
        new XmlCodec[A]:
          override def elementName: String = configuredElementName(typeId.name, Seq.empty, modifiers)
          override def isRecordLike: Boolean = inner.isRecordLike
          override def caseNames: Seq[String] = inner.caseNames
          override def isEnumeration: Boolean = inner.isEnumeration
          override def unsafeDecode(element: Xml.Element): A = wrapperBinding.wrap(inner.unsafeDecode(element))
          override def encodeNamed(name: String, value: A): Xml.Element =
            inner.encodeNamed(name, wrapperBinding.unwrap(value))
          override def unsafeDecodeText(text: String): A = wrapperBinding.wrap(inner.unsafeDecodeText(text))
          override def encodeText(value: A): String = inner.encodeText(wrapperBinding.unwrap(value))

  override def instanceOverrides: IndexedSeq[InstanceOverride] =
    recursiveRecordCache.remove()
    // Parent `Schema.derived` inlines `Schema[Name]` before `Name`'s given runs.
    // Register here, before this snapshot is taken, so `Seq[Name]` uses `<name>`.
    XmlCodecDeriver.register(TypeId.of[Name], Name.codec)
    val tagged: List[InstanceOverride] =
      XmlCodecDeriver.taggedCodecs.asScala.toList.map: (id, codec) =>
        InstanceOverrideByType(id.asInstanceOf[TypeId[Any]], codec.asInstanceOf[Lazy[XmlCodec[Any]]])
    Chunk(
      InstanceOverrideByType(TypeId.of[XmlNode.Element], Lazy(XmlCodec.elementCodec))
    ) ++ Chunk.from(tagged)

  private def primitiveCodec[A](primitiveType: PrimitiveType[A]): XmlCodec[A] =
    primitiveType match
      case _: PrimitiveType.Unit.type => textCodec("unit", _ => (), _ => "")
      case _: PrimitiveType.Boolean =>
        textCodec[Boolean](
          "boolean",
          XmlAst.parseBoolean,
          value => if value then "true" else "false"
        ).asInstanceOf[XmlCodec[A]]
      case _: PrimitiveType.Byte => textCodec("byte", _.toByte, _.toString)
      case _: PrimitiveType.Short => textCodec("short", _.toShort, _.toString)
      case _: PrimitiveType.Int => textCodec("int", _.toInt, _.toString)
      case _: PrimitiveType.Long => textCodec("long", _.toLong, _.toString)
      case _: PrimitiveType.Float => textCodec("float", _.toFloat, _.toString)
      case _: PrimitiveType.Double => textCodec("double", _.toDouble, _.toString)
      case _: PrimitiveType.Char =>
        textCodec[Char](
          "char",
          text =>
            if text.length == 1 then text.head
            else throw XmlError(s"Expected a single character: $text"),
          _.toString
        ).asInstanceOf[XmlCodec[A]]
      case _: PrimitiveType.String => textCodec("string", identity, identity)
      case _: PrimitiveType.BigInt => textCodec("bigInt", BigInt(_), _.toString)
      case _: PrimitiveType.BigDecimal => textCodec("bigDecimal", BigDecimal(_), _.toString)
      case _ => textCodec(primitiveType.typeId.name, parse => throw XmlError(s"Unsupported primitive: $parse"), _.toString)

  private def textCodec[A](name: String, parse: String => A, format: A => String): XmlCodec[A] =
    new XmlCodec[A]:
      override def elementName: String = name
      override def unsafeDecode(element: Xml.Element): A = unsafeDecodeText(characterData(element))
      override def encodeNamed(elementName: String, value: A): Xml.Element =
        val text: String = format(value)
        Xml.element(
          elementName,
          Seq.empty,
          if text.isEmpty then Seq.empty else Seq(Xml.text(text))
        )
      override def unsafeDecodeText(text: String): A =
        try parse(text)
        catch
          case e: XmlError => throw e
          case e: NumberFormatException => throw XmlError(s"Invalid $name: $text", e)
          case e: IllegalArgumentException => throw XmlError(s"Invalid $name: $text", e)
      override def encodeText(value: A): String = format(value)

  private def unsupported[A](what: String): XmlCodec[A] = new XmlCodec[A]:
    override def elementName: String = what
    override def unsafeDecode(element: Xml.Element): A = throw XmlError(s"$what is not supported")
    override def encodeNamed(name: String, value: A): Xml.Element = throw XmlError(s"$what is not supported")
