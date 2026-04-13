// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class StarTypeParameter implements Serializable, Comparable<StarTypeParameter> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.StarTypeParameter");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name DEFAULT = new hydra.core.Name("default");

  public final hydra.python.syntax.Name name;

  public final hydra.util.Maybe<hydra.python.syntax.StarExpression> default_;

  public StarTypeParameter (hydra.python.syntax.Name name, hydra.util.Maybe<hydra.python.syntax.StarExpression> default_) {
    this.name = name;
    this.default_ = default_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StarTypeParameter)) {
      return false;
    }
    StarTypeParameter o = (StarTypeParameter) other;
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
  public int compareTo(StarTypeParameter other) {
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

  public StarTypeParameter withName(hydra.python.syntax.Name name) {
    return new StarTypeParameter(name, default_);
  }

  public StarTypeParameter withDefault(hydra.util.Maybe<hydra.python.syntax.StarExpression> default_) {
    return new StarTypeParameter(name, default_);
  }
}
