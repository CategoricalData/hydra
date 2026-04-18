// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SetOperatorType implements Serializable, Comparable<SetOperatorType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SetOperatorType");

  public static final hydra.core.Name UNION = new hydra.core.Name("union");

  public static final hydra.core.Name EXCEPT = new hydra.core.Name("except");

  public static final hydra.core.Name INTERSECT = new hydra.core.Name("intersect");

  private SetOperatorType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Union instance) ;

    R visit(Except instance) ;

    R visit(Intersect instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SetOperatorType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Union instance) {
      return otherwise(instance);
    }

    default R visit(Except instance) {
      return otherwise(instance);
    }

    default R visit(Intersect instance) {
      return otherwise(instance);
    }
  }

  public static final class Union extends openGql.grammar.SetOperatorType implements Serializable {
    public Union () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SetOperatorType other) {
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

  public static final class Except extends openGql.grammar.SetOperatorType implements Serializable {
    public Except () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Except)) {
        return false;
      }
      Except o = (Except) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SetOperatorType other) {
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

  public static final class Intersect extends openGql.grammar.SetOperatorType implements Serializable {
    public Intersect () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Intersect)) {
        return false;
      }
      Intersect o = (Intersect) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SetOperatorType other) {
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
