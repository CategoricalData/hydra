// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class Sign implements Serializable, Comparable<Sign> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Sign");

  public static final hydra.core.Name PLUS = new hydra.core.Name("plus");

  public static final hydra.core.Name MINUS = new hydra.core.Name("minus");

  private Sign () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Plus instance) ;

    R visit(Minus instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Sign instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Plus instance) {
      return otherwise(instance);
    }

    default R visit(Minus instance) {
      return otherwise(instance);
    }
  }

  public static final class Plus extends openGql.grammar.Sign implements Serializable {
    public Plus () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plus)) {
        return false;
      }
      Plus o = (Plus) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Sign other) {
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

  public static final class Minus extends openGql.grammar.Sign implements Serializable {
    public Minus () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Minus)) {
        return false;
      }
      Minus o = (Minus) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Sign other) {
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
