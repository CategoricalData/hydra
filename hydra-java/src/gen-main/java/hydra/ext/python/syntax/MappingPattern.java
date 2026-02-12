// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class MappingPattern implements Serializable, Comparable<MappingPattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.MappingPattern");
  
  public static final hydra.core.Name FIELD_NAME_ITEMS = new hydra.core.Name("items");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE_STAR = new hydra.core.Name("doubleStar");
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.ItemsPattern> items;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.DoubleStarPattern> doubleStar;
  
  public MappingPattern (hydra.util.Maybe<hydra.ext.python.syntax.ItemsPattern> items, hydra.util.Maybe<hydra.ext.python.syntax.DoubleStarPattern> doubleStar) {
    this.items = items;
    this.doubleStar = doubleStar;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MappingPattern)) {
      return false;
    }
    MappingPattern o = (MappingPattern) other;
    return java.util.Objects.equals(
      this.items,
      o.items) && java.util.Objects.equals(
      this.doubleStar,
      o.doubleStar);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(items) + 3 * java.util.Objects.hashCode(doubleStar);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MappingPattern other) {
    int cmp = 0;
    cmp = Integer.compare(
      items.hashCode(),
      other.items.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      doubleStar.hashCode(),
      other.doubleStar.hashCode());
  }
  
  public MappingPattern withItems(hydra.util.Maybe<hydra.ext.python.syntax.ItemsPattern> items) {
    return new MappingPattern(items, doubleStar);
  }
  
  public MappingPattern withDoubleStar(hydra.util.Maybe<hydra.ext.python.syntax.DoubleStarPattern> doubleStar) {
    return new MappingPattern(items, doubleStar);
  }
}
