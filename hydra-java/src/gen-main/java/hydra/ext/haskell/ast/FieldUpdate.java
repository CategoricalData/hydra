// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A field name and value
 */
public class FieldUpdate implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.FieldUpdate");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final hydra.ext.haskell.ast.Expression value;
  
  public FieldUpdate (hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Expression value) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((value));
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldUpdate)) {
      return false;
    }
    FieldUpdate o = (FieldUpdate) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public FieldUpdate withName(hydra.ext.haskell.ast.Name name) {
    java.util.Objects.requireNonNull((name));
    return new FieldUpdate(name, value);
  }
  
  public FieldUpdate withValue(hydra.ext.haskell.ast.Expression value) {
    java.util.Objects.requireNonNull((value));
    return new FieldUpdate(name, value);
  }
}