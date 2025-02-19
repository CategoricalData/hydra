// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class ValuePair implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.graphson.syntax.ValuePair");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_SECOND = new hydra.core.Name("second");
  
  public final hydra.pg.graphson.syntax.Value first;
  
  public final hydra.pg.graphson.syntax.Value second;
  
  public ValuePair (hydra.pg.graphson.syntax.Value first, hydra.pg.graphson.syntax.Value second) {
    java.util.Objects.requireNonNull((first));
    java.util.Objects.requireNonNull((second));
    this.first = first;
    this.second = second;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValuePair)) {
      return false;
    }
    ValuePair o = (ValuePair) (other);
    return first.equals(o.first) && second.equals(o.second);
  }
  
  @Override
  public int hashCode() {
    return 2 * first.hashCode() + 3 * second.hashCode();
  }
  
  public ValuePair withFirst(hydra.pg.graphson.syntax.Value first) {
    java.util.Objects.requireNonNull((first));
    return new ValuePair(first, second);
  }
  
  public ValuePair withSecond(hydra.pg.graphson.syntax.Value second) {
    java.util.Objects.requireNonNull((second));
    return new ValuePair(first, second);
  }
}