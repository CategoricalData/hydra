// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class DoubleStarTypeParameter implements Serializable, Comparable<DoubleStarTypeParameter> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.DoubleStarTypeParameter");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Expression> default_;
  
  public DoubleStarTypeParameter (hydra.ext.python.syntax.Name name, hydra.util.Maybe<hydra.ext.python.syntax.Expression> default_) {
    this.name = name;
    this.default_ = default_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DoubleStarTypeParameter)) {
      return false;
    }
    DoubleStarTypeParameter o = (DoubleStarTypeParameter) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.default_,
      o.default_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(default_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DoubleStarTypeParameter other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      default_.hashCode(),
      other.default_.hashCode());
  }
  
  public DoubleStarTypeParameter withName(hydra.ext.python.syntax.Name name) {
    return new DoubleStarTypeParameter(name, default_);
  }
  
  public DoubleStarTypeParameter withDefault(hydra.util.Maybe<hydra.ext.python.syntax.Expression> default_) {
    return new DoubleStarTypeParameter(name, default_);
  }
}
