// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A map/dictionary literal. Serializes as {:a 1 :b 2} in Clojure, as an alist '((a . 1) (b . 2)) in other dialects
 */
public class MapLiteral implements Serializable, Comparable<MapLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.MapLiteral");

  public static final hydra.core.Name ENTRIES = new hydra.core.Name("entries");

  /**
   * The key-value pairs
   */
  public final java.util.List<hydra.ext.lisp.syntax.MapEntry> entries;

  public MapLiteral (java.util.List<hydra.ext.lisp.syntax.MapEntry> entries) {
    this.entries = entries;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapLiteral)) {
      return false;
    }
    MapLiteral o = (MapLiteral) other;
    return java.util.Objects.equals(
      this.entries,
      o.entries);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(entries);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MapLiteral other) {
    return hydra.util.Comparing.compare(
      entries,
      other.entries);
  }
}
