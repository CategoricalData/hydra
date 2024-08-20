// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class Assertion_Class implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.Assertion.Class");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPES = new hydra.core.Name("types");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final java.util.List<hydra.ext.haskell.ast.Type> types;
  
  public Assertion_Class (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.Type> types) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((types));
    this.name = name;
    this.types = types;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Assertion_Class)) {
      return false;
    }
    Assertion_Class o = (Assertion_Class) (other);
    return name.equals(o.name) && types.equals(o.types);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * types.hashCode();
  }
  
  public Assertion_Class withName(hydra.ext.haskell.ast.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Assertion_Class(name, types);
  }
  
  public Assertion_Class withTypes(java.util.List<hydra.ext.haskell.ast.Type> types) {
    java.util.Objects.requireNonNull((types));
    return new Assertion_Class(name, types);
  }
}
