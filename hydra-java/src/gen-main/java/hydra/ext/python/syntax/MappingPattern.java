// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class MappingPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.MappingPattern");
  
  public static final hydra.core.Name FIELD_NAME_ITEMS = new hydra.core.Name("items");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE_STAR = new hydra.core.Name("doubleStar");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.ItemsPattern> items;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.DoubleStarPattern> doubleStar;
  
  public MappingPattern (hydra.util.Opt<hydra.ext.python.syntax.ItemsPattern> items, hydra.util.Opt<hydra.ext.python.syntax.DoubleStarPattern> doubleStar) {
    java.util.Objects.requireNonNull((items));
    java.util.Objects.requireNonNull((doubleStar));
    this.items = items;
    this.doubleStar = doubleStar;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MappingPattern)) {
      return false;
    }
    MappingPattern o = (MappingPattern) (other);
    return items.equals(o.items) && doubleStar.equals(o.doubleStar);
  }
  
  @Override
  public int hashCode() {
    return 2 * items.hashCode() + 3 * doubleStar.hashCode();
  }
  
  public MappingPattern withItems(hydra.util.Opt<hydra.ext.python.syntax.ItemsPattern> items) {
    java.util.Objects.requireNonNull((items));
    return new MappingPattern(items, doubleStar);
  }
  
  public MappingPattern withDoubleStar(hydra.util.Opt<hydra.ext.python.syntax.DoubleStarPattern> doubleStar) {
    java.util.Objects.requireNonNull((doubleStar));
    return new MappingPattern(items, doubleStar);
  }
}