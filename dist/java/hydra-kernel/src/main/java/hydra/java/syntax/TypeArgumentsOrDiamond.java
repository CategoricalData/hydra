// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class TypeArgumentsOrDiamond implements Serializable, Comparable<TypeArgumentsOrDiamond> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.TypeArgumentsOrDiamond");

  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");

  public static final hydra.core.Name DIAMOND = new hydra.core.Name("diamond");

  private TypeArgumentsOrDiamond () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Arguments instance) ;

    R visit(Diamond instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeArgumentsOrDiamond instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Arguments instance) {
      return otherwise(instance);
    }

    default R visit(Diamond instance) {
      return otherwise(instance);
    }
  }

  public static final class Arguments extends hydra.java.syntax.TypeArgumentsOrDiamond implements Serializable {
    public final java.util.List<hydra.java.syntax.TypeArgument> value;

    public Arguments (java.util.List<hydra.java.syntax.TypeArgument> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Arguments)) {
        return false;
      }
      Arguments o = (Arguments) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeArgumentsOrDiamond other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Arguments o = (Arguments) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Diamond extends hydra.java.syntax.TypeArgumentsOrDiamond implements Serializable {
    public Diamond () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Diamond)) {
        return false;
      }
      Diamond o = (Diamond) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeArgumentsOrDiamond other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
