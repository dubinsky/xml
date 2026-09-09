package org.podval.metadata

import org.podval.xml.XmlParser

trait HasName(nameOverride: Option[String]):
  final def name: String = nameOverride.getOrElse(defaultName)

  protected def defaultName: String

object HasName:

  trait Enum extends HasName:
    final override protected def defaultName: String = this.toString

  trait NonEnum extends HasName:
    final override protected def defaultName: String = XmlParser.className(this.getClass)

  def mapByName[K <: HasName, M](
    keys: Seq[K],
    metadatas: Seq[M],
    hasName: (M, String) => Boolean
  ): Map[K, M] =
    val pairs: Seq[(K, M)] = metadatas.map(metadata =>
      find(
        keys,
        metadata,
        hasName
      ) -> metadata
    )
    checkNoDuplicateKeys(pairs.map(_._1))
    val result: Map[K, M] = pairs.toMap
    checkNoUnmatchedKeys(keys.toSet -- result.keySet)
    result

  def bind[K, M](
    keys: Seq[K],
    metadatas: Seq[M],
    getKey: M => K
  ): Map[K, M] =
    val pairs: Seq[(K, M)] = metadatas.map(metadata => getKey(metadata) -> metadata)
    checkNoDuplicateKeys(pairs.map(_._1))
    val result: Map[K, M] = pairs.toMap
    val keySet: Set[K] = keys.toSet
    checkNoUnmatchedKeys(keySet -- result.keySet)
    val extra: Set[K] = result.keySet -- keySet
    require(extra.isEmpty, s"Extra keys: $extra")
    result

  def find[K <: HasName](
    keys: Seq[K],
    names: Names
  ): K = find(
    keys = keys,
    metadata = names,
    hasName = (names: Names, name: String) => names.hasName(name)
  )

  private def find[K <: HasName, M](
    keys: Seq[K],
    metadata: M,
    hasName: (M, String) => Boolean
  ): K =
    val result: Seq[K] = keys.filter(key => hasName(metadata, key.name))
    require(result.nonEmpty, s"Unmatched metadata: $metadata")
    require(result.length == 1, s"Metadata matched multiple keys: $metadata")
    result.head

  private def checkNoUnmatchedKeys[K](unmatchedKeys: Set[K]): Unit =
    require(unmatchedKeys.isEmpty, s"Unmatched keys: $unmatchedKeys")

  private def checkNoDuplicateKeys[K](keys: Seq[K]): Unit =
    val duplicates: Set[K] = keys.groupBy(identity).filter((_, ts) => ts.length > 1).keySet
    require(duplicates.isEmpty, s"Duplicate keys: $duplicates")
