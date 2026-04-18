// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class GeneralizingBinder implements Serializable, Comparable<GeneralizingBinder> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.GeneralizingBinder");

  public static final hydra.core.Name EXPLICIT = new hydra.core.Name("explicit");

  public static final hydra.core.Name IMPLICIT_MAXIMALLY_INSERTED = new hydra.core.Name("implicitMaximallyInserted");

  public static final hydra.core.Name IMPLICIT_NON_MAXIMALLY_INSERTED = new hydra.core.Name("implicitNonMaximallyInserted");

  private GeneralizingBinder () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Explicit instance) ;

    R visit(ImplicitMaximallyInserted instance) ;

    R visit(ImplicitNonMaximallyInserted instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GeneralizingBinder instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Explicit instance) {
      return otherwise(instance);
    }

    default R visit(ImplicitMaximallyInserted instance) {
      return otherwise(instance);
    }

    default R visit(ImplicitNonMaximallyInserted instance) {
      return otherwise(instance);
    }
  }

  /**
   * Terms surrounded by `( ) introduce their free variables as explicit arguments
   */
  public static final class Explicit extends hydra.coq.syntax.GeneralizingBinder implements Serializable {
    public final hydra.coq.syntax.TypeclassConstraint value;

    public Explicit (hydra.coq.syntax.TypeclassConstraint value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Explicit)) {
        return false;
      }
      Explicit o = (Explicit) other;
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
    public int compareTo(GeneralizingBinder other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Explicit o = (Explicit) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Terms surrounded by `{ } introduce their free variables as maximally inserted implicit arguments
   */
  public static final class ImplicitMaximallyInserted extends hydra.coq.syntax.GeneralizingBinder implements Serializable {
    public final hydra.coq.syntax.TypeclassConstraint value;

    public ImplicitMaximallyInserted (hydra.coq.syntax.TypeclassConstraint value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImplicitMaximallyInserted)) {
        return false;
      }
      ImplicitMaximallyInserted o = (ImplicitMaximallyInserted) other;
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
    public int compareTo(GeneralizingBinder other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ImplicitMaximallyInserted o = (ImplicitMaximallyInserted) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Terms surrounded by `[ ] introduce them as non-maximally inserted implicit arguments
   */
  public static final class ImplicitNonMaximallyInserted extends hydra.coq.syntax.GeneralizingBinder implements Serializable {
    public final hydra.coq.syntax.TypeclassConstraint value;

    public ImplicitNonMaximallyInserted (hydra.coq.syntax.TypeclassConstraint value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImplicitNonMaximallyInserted)) {
        return false;
      }
      ImplicitNonMaximallyInserted o = (ImplicitNonMaximallyInserted) other;
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
    public int compareTo(GeneralizingBinder other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ImplicitNonMaximallyInserted o = (ImplicitNonMaximallyInserted) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
