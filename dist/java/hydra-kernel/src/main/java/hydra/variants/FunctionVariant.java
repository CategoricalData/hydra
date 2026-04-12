// Note: this is an automatically generated file. Do not edit.

package hydra.variants;

import java.io.Serializable;

/**
 * The identifier of a function constructor
 */
public abstract class FunctionVariant implements Serializable, Comparable<FunctionVariant> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.variants.FunctionVariant");

  public static final hydra.core.Name ELIMINATION = new hydra.core.Name("elimination");

  public static final hydra.core.Name LAMBDA = new hydra.core.Name("lambda");

  private FunctionVariant () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Elimination instance) ;

    R visit(Lambda instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FunctionVariant instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Elimination instance) {
      return otherwise(instance);
    }

    default R visit(Lambda instance) {
      return otherwise(instance);
    }
  }

  public static final class Elimination extends hydra.variants.FunctionVariant implements Serializable {
    public Elimination () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Elimination)) {
        return false;
      }
      Elimination o = (Elimination) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(FunctionVariant other) {
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

  public static final class Lambda extends hydra.variants.FunctionVariant implements Serializable {
    public Lambda () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lambda)) {
        return false;
      }
      Lambda o = (Lambda) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(FunctionVariant other) {
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
