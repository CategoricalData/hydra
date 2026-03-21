package hydra.lib

object maps:
  // Type parameter order matches Hydra's type application order
  def alter[V, K](f: Option[V] => Option[V])(k: K)(m: Map[K, V]): Map[K, V] =
    f(m.get(k)) match
      case Some(v) => m.updated(k, v)
      case None => m.removed(k)
  def bimap[K1, K2, V1, V2](fk: K1 => K2)(fv: V1 => V2)(m: Map[K1, V1]): Map[K2, V2] = m.map((k, v) => (fk(k), fv(v)))
  def delete[K, V](k: K)(m: Map[K, V]): Map[K, V] = m.removed(k)
  def elems[K, V](m: Map[K, V]): Seq[V] = m.values.toSeq
  def empty[K, V]: Map[K, V] = Map.empty
  def filter[V, K](p: V => Boolean)(m: Map[K, V]): Map[K, V] = m.filter((_, v) => p(v))
  def filterWithKey[K, V](p: K => V => Boolean)(m: Map[K, V]): Map[K, V] = m.filter((k, v) => p(k)(v))
  def findWithDefault[V, K](d: V)(k: K)(m: Map[K, V]): V = m.getOrElse(k, d)
  def fromList[K, V](pairs: Seq[(K, V)]): Map[K, V] = pairs.toMap
  def insert[K, V](k: K)(v: V)(m: Map[K, V]): Map[K, V] = m.updated(k, v)
  def keys[K, V](m: Map[K, V]): Seq[K] = m.keys.toSeq
  def lookup[K, V](k: K)(m: Map[K, V]): Option[V] = m.get(k)
  def map[V1, V2, K](f: V1 => V2)(m: Map[K, V1]): Map[K, V2] = m.map((k, v) => (k, f(v)))
  def mapKeys[K1, K2, V](f: K1 => K2)(m: Map[K1, V]): Map[K2, V] = m.map((k, v) => (f(k), v))
  def member[K, V](k: K)(m: Map[K, V]): Boolean = m.contains(k)
  def `null`[K, V](m: Map[K, V]): Boolean = m.isEmpty
  def singleton[K, V](k: K)(v: V): Map[K, V] = Map(k -> v)
  def size[K, V](m: Map[K, V]): Int = m.size
  def toList[K, V](m: Map[K, V]): Seq[(K, V)] = m.toSeq
  def union[K, V](m1: Map[K, V])(m2: Map[K, V]): Map[K, V] = m2 ++ m1
