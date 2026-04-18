// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class GeneralValueSpecification implements Serializable, Comparable<GeneralValueSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GeneralValueSpecification");

  public static final hydra.core.Name DYNAMIC_PARAMETER_SPECIFICATION = new hydra.core.Name("dynamicParameterSpecification");

  public static final hydra.core.Name SESSION_USER = new hydra.core.Name("sessionUser");

  private GeneralValueSpecification () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DynamicParameterSpecification instance) ;

    R visit(SessionUser instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GeneralValueSpecification instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DynamicParameterSpecification instance) {
      return otherwise(instance);
    }

    default R visit(SessionUser instance) {
      return otherwise(instance);
    }
  }

  public static final class DynamicParameterSpecification extends openGql.grammar.GeneralValueSpecification implements Serializable {
    public final String value;

    public DynamicParameterSpecification (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DynamicParameterSpecification)) {
        return false;
      }
      DynamicParameterSpecification o = (DynamicParameterSpecification) other;
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
    public int compareTo(GeneralValueSpecification other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DynamicParameterSpecification o = (DynamicParameterSpecification) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SessionUser extends openGql.grammar.GeneralValueSpecification implements Serializable {
    public SessionUser () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SessionUser)) {
        return false;
      }
      SessionUser o = (SessionUser) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GeneralValueSpecification other) {
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
