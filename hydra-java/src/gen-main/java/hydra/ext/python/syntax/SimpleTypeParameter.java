// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class SimpleTypeParameter implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SimpleTypeParameter");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_BOUND = new hydra.core.Name("bound");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Expression> bound;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Expression> default_;
  
  public SimpleTypeParameter (hydra.ext.python.syntax.Name name, hydra.util.Opt<hydra.ext.python.syntax.Expression> bound, hydra.util.Opt<hydra.ext.python.syntax.Expression> default_) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((bound));
    java.util.Objects.requireNonNull((default_));
    this.name = name;
    this.bound = bound;
    this.default_ = default_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleTypeParameter)) {
      return false;
    }
    SimpleTypeParameter o = (SimpleTypeParameter) (other);
    return name.equals(o.name) && bound.equals(o.bound) && default_.equals(o.default_);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * bound.hashCode() + 5 * default_.hashCode();
  }
  
  public SimpleTypeParameter withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new SimpleTypeParameter(name, bound, default_);
  }
  
  public SimpleTypeParameter withBound(hydra.util.Opt<hydra.ext.python.syntax.Expression> bound) {
    java.util.Objects.requireNonNull((bound));
    return new SimpleTypeParameter(name, bound, default_);
  }
  
  public SimpleTypeParameter withDefault(hydra.util.Opt<hydra.ext.python.syntax.Expression> default_) {
    java.util.Objects.requireNonNull((default_));
    return new SimpleTypeParameter(name, bound, default_);
  }
}