package hydra.lib

object maps:
  /** Alter a value at a key using a function. */
  def alter[V, K](f: Option[V] => Option[V])(k: K)(m: Map[K, V]): Map[K, V] =
    f(m.get(k)) match
      case Some(v) => m.updated(k, v)
      case None => m.removed(k)

  /** Map a function over the keys and values of a map. */
  def bimap[K1, K2, V1, V2](fk: K1 => K2)(fv: V1 => V2)(m: Map[K1, V1]): Map[K2, V2] = m.map((k, v) => (fk(k), fv(v)))

  /** Remove a key from a map. */
  def delete[K, V](k: K)(m: Map[K, V]): Map[K, V] = m.removed(k)

  /** Get the values of a map. */
  def elems[K, V](m: Map[K, V]): Seq[V] = m.values.toSeq

  /** Create an empty map. */
  def empty[K, V]: Map[K, V] = Map.empty

  /** Filter a map based on values. */
  def filter[V, K](p: V => Boolean)(m: Map[K, V]): Map[K, V] = m.filter((_, v) => p(v))

  /** Filter a map based on key-value pairs. */
  def filterWithKey[K, V](p: K => V => Boolean)(m: Map[K, V]): Map[K, V] = m.filter((k, v) => p(k)(v))

  /** Lookup a value with a default. */
  def findWithDefault[V, K](d: V)(k: K)(m: Map[K, V]): V = m.getOrElse(k, d)

  /** Create a map from a list of key-value pairs. */
  def fromList[K, V](pairs: Seq[(K, V)]): Map[K, V] = pairs.toMap

  /** Insert a key-value pair into a map. */
  def insert[K, V](k: K)(v: V)(m: Map[K, V]): Map[K, V] = m.updated(k, v)

  /** Get the keys of a map. */
  def keys[K, V](m: Map[K, V]): Seq[K] = m.keys.toSeq.sortWith((a, b) => equality.lt(a)(b))

  /** Lookup a value in a map. */
  def lookup[K, V](k: K)(m: Map[K, V]): Option[V] = m.get(k)

  /** Map a function over a map. */
  def map[V1, V2, K](f: V1 => V2)(m: Map[K, V1]): Map[K, V2] = m.map((k, v) => (k, f(v)))

  /** Map a function over the keys of a map. */
  def mapKeys[K1, K2, V](f: K1 => K2)(m: Map[K1, V]): Map[K2, V] = m.map((k, v) => (f(k), v))

  /** Check if a key is present in a map. */
  def member[K, V](k: K)(m: Map[K, V]): Boolean = m.contains(k)

  /** Check if a map is empty. */
  def `null`[K, V](m: Map[K, V]): Boolean = m.isEmpty

  /** Create a map with a single key-value pair. */
  def singleton[K, V](k: K)(v: V): Map[K, V] = Map(k -> v)

  /** Get the size of a map. */
  def size[K, V](m: Map[K, V]): Int = m.size

  /** Convert a map to a list of key-value pairs. */
  def toList[K, V](m: Map[K, V]): Seq[(K, V)] = m.toSeq

  /** Union two maps, with the first taking precedence. */
  def union[K, V](m1: Map[K, V])(m2: Map[K, V]): Map[K, V] = m2 ++ m1
