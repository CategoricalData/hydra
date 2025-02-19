// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class Argument implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.Argument");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> name;
  
  public final hydra.ext.csharp.syntax.ArgumentValue value;
  
  public Argument (hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> name, hydra.ext.csharp.syntax.ArgumentValue value) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((value));
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Argument)) {
      return false;
    }
    Argument o = (Argument) (other);
    return name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * value.hashCode();
  }
  
  public Argument withName(hydra.util.Opt<hydra.ext.csharp.syntax.Identifier> name) {
    java.util.Objects.requireNonNull((name));
    return new Argument(name, value);
  }
  
  public Argument withValue(hydra.ext.csharp.syntax.ArgumentValue value) {
    java.util.Objects.requireNonNull((value));
    return new Argument(name, value);
  }
}