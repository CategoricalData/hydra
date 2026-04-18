// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * In the context of a function definition, these forms specify that name is an implicit argument.
 */
public abstract class ImplicitBinders implements Serializable, Comparable<ImplicitBinders> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.ImplicitBinders");

  public static final hydra.core.Name MAXIMALLY_INSERTED = new hydra.core.Name("maximallyInserted");

  public static final hydra.core.Name NON_MAXIMALLY_INSERTED = new hydra.core.Name("nonMaximallyInserted");

  private ImplicitBinders () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(MaximallyInserted instance) ;

    R visit(NonMaximallyInserted instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImplicitBinders instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(MaximallyInserted instance) {
      return otherwise(instance);
    }

    default R visit(NonMaximallyInserted instance) {
      return otherwise(instance);
    }
  }

  /**
   * The first form, with curly braces, makes name a maximally inserted implicit argument
   */
  public static final class MaximallyInserted extends hydra.coq.syntax.ImplicitBinders implements Serializable {
    public final hydra.coq.syntax.TypeBinders value;

    public MaximallyInserted (hydra.coq.syntax.TypeBinders value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaximallyInserted)) {
        return false;
      }
      MaximallyInserted o = (MaximallyInserted) other;
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
    public int compareTo(ImplicitBinders other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MaximallyInserted o = (MaximallyInserted) other;
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
   * The second form, with square brackets, makes name a non-maximally inserted implicit argument.
   */
  public static final class NonMaximallyInserted extends hydra.coq.syntax.ImplicitBinders implements Serializable {
    public final hydra.coq.syntax.TypeBinders value;

    public NonMaximallyInserted (hydra.coq.syntax.TypeBinders value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NonMaximallyInserted)) {
        return false;
      }
      NonMaximallyInserted o = (NonMaximallyInserted) other;
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
    public int compareTo(ImplicitBinders other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NonMaximallyInserted o = (NonMaximallyInserted) other;
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
