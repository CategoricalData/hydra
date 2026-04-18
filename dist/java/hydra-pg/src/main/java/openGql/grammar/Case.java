// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class Case implements Serializable, Comparable<Case> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Case");

  public static final hydra.core.Name UPPER = new hydra.core.Name("upper");

  public static final hydra.core.Name LOWER = new hydra.core.Name("lower");

  private Case () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Upper instance) ;

    R visit(Lower instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Case instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Upper instance) {
      return otherwise(instance);
    }

    default R visit(Lower instance) {
      return otherwise(instance);
    }
  }

  public static final class Upper extends openGql.grammar.Case implements Serializable {
    public Upper () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Upper)) {
        return false;
      }
      Upper o = (Upper) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Case other) {
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

  public static final class Lower extends openGql.grammar.Case implements Serializable {
    public Lower () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lower)) {
        return false;
      }
      Lower o = (Lower) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Case other) {
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
