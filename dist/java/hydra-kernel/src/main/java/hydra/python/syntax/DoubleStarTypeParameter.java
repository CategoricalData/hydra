// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class DoubleStarTypeParameter implements Serializable, Comparable<DoubleStarTypeParameter> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.DoubleStarTypeParameter");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name DEFAULT = new hydra.core.Name("default");

  public final hydra.python.syntax.Name name;

  public final hydra.util.Maybe<hydra.python.syntax.Expression> default_;

  public DoubleStarTypeParameter (hydra.python.syntax.Name name, hydra.util.Maybe<hydra.python.syntax.Expression> default_) {
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
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      default_,
      other.default_);
  }

  public DoubleStarTypeParameter withName(hydra.python.syntax.Name name) {
    return new DoubleStarTypeParameter(name, default_);
  }

  public DoubleStarTypeParameter withDefault(hydra.util.Maybe<hydra.python.syntax.Expression> default_) {
    return new DoubleStarTypeParameter(name, default_);
  }
}
