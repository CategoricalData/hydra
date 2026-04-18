// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TrimSpecification implements Serializable, Comparable<TrimSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TrimSpecification");

  public static final hydra.core.Name LEADING = new hydra.core.Name("leading");

  public static final hydra.core.Name TRAILING = new hydra.core.Name("trailing");

  public static final hydra.core.Name BOTH = new hydra.core.Name("both");

  private TrimSpecification () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Leading instance) ;

    R visit(Trailing instance) ;

    R visit(Both instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TrimSpecification instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Leading instance) {
      return otherwise(instance);
    }

    default R visit(Trailing instance) {
      return otherwise(instance);
    }

    default R visit(Both instance) {
      return otherwise(instance);
    }
  }

  public static final class Leading extends openGql.grammar.TrimSpecification implements Serializable {
    public Leading () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Leading)) {
        return false;
      }
      Leading o = (Leading) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrimSpecification other) {
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

  public static final class Trailing extends openGql.grammar.TrimSpecification implements Serializable {
    public Trailing () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Trailing)) {
        return false;
      }
      Trailing o = (Trailing) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrimSpecification other) {
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

  public static final class Both extends openGql.grammar.TrimSpecification implements Serializable {
    public Both () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Both)) {
        return false;
      }
      Both o = (Both) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrimSpecification other) {
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
