// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class BooleanLiteral implements Serializable, Comparable<BooleanLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.BooleanLiteral");

  public static final hydra.core.Name TRUE = new hydra.core.Name("True");

  public static final hydra.core.Name FALSE = new hydra.core.Name("False");

  private BooleanLiteral () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(True instance) ;

    R visit(False instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BooleanLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(True instance) {
      return otherwise(instance);
    }

    default R visit(False instance) {
      return otherwise(instance);
    }
  }

  public static final class True extends hydra.shex.syntax.BooleanLiteral implements Serializable {
    public True () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof True)) {
        return false;
      }
      True o = (True) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BooleanLiteral other) {
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

  public static final class False extends hydra.shex.syntax.BooleanLiteral implements Serializable {
    public False () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof False)) {
        return false;
      }
      False o = (False) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BooleanLiteral other) {
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
