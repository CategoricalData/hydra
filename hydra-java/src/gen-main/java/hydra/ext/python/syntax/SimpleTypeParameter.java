// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class SimpleTypeParameter implements Serializable, Comparable<SimpleTypeParameter> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SimpleTypeParameter");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_BOUND = new hydra.core.Name("bound");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Expression> bound;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Expression> default_;
  
  public SimpleTypeParameter (hydra.ext.python.syntax.Name name, hydra.util.Maybe<hydra.ext.python.syntax.Expression> bound, hydra.util.Maybe<hydra.ext.python.syntax.Expression> default_) {
    this.name = name;
    this.bound = bound;
    this.default_ = default_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleTypeParameter)) {
      return false;
    }
    SimpleTypeParameter o = (SimpleTypeParameter) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.bound,
      o.bound) && java.util.Objects.equals(
      this.default_,
      o.default_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(bound) + 5 * java.util.Objects.hashCode(default_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SimpleTypeParameter other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      bound.hashCode(),
      other.bound.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      default_.hashCode(),
      other.default_.hashCode());
  }
  
  public SimpleTypeParameter withName(hydra.ext.python.syntax.Name name) {
    return new SimpleTypeParameter(name, bound, default_);
  }
  
  public SimpleTypeParameter withBound(hydra.util.Maybe<hydra.ext.python.syntax.Expression> bound) {
    return new SimpleTypeParameter(name, bound, default_);
  }
  
  public SimpleTypeParameter withDefault(hydra.util.Maybe<hydra.ext.python.syntax.Expression> default_) {
    return new SimpleTypeParameter(name, bound, default_);
  }
}
