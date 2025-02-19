// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class StarTypeParameter implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.StarTypeParameter");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.StarExpression> default_;
  
  public StarTypeParameter (hydra.ext.python.syntax.Name name, hydra.util.Opt<hydra.ext.python.syntax.StarExpression> default_) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((default_));
    this.name = name;
    this.default_ = default_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StarTypeParameter)) {
      return false;
    }
    StarTypeParameter o = (StarTypeParameter) (other);
    return name.equals(o.name) && default_.equals(o.default_);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * default_.hashCode();
  }
  
  public StarTypeParameter withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new StarTypeParameter(name, default_);
  }
  
  public StarTypeParameter withDefault(hydra.util.Opt<hydra.ext.python.syntax.StarExpression> default_) {
    java.util.Objects.requireNonNull((default_));
    return new StarTypeParameter(name, default_);
  }
}