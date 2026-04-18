// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SetQuantifier implements Serializable, Comparable<SetQuantifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SetQuantifier");

  public static final hydra.core.Name DISTINCT = new hydra.core.Name("distinct");

  public static final hydra.core.Name ALL = new hydra.core.Name("all");

  private SetQuantifier () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Distinct instance) ;

    R visit(All instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SetQuantifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Distinct instance) {
      return otherwise(instance);
    }

    default R visit(All instance) {
      return otherwise(instance);
    }
  }

  public static final class Distinct extends openGql.grammar.SetQuantifier implements Serializable {
    public Distinct () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Distinct)) {
        return false;
      }
      Distinct o = (Distinct) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SetQuantifier other) {
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

  public static final class All extends openGql.grammar.SetQuantifier implements Serializable {
    public All () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SetQuantifier other) {
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
