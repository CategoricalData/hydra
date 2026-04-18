// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * The style of boolean literals in a dialect
 */
public abstract class BooleanStyle implements Serializable, Comparable<BooleanStyle> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.BooleanStyle");

  public static final hydra.core.Name TRUE_FALSE = new hydra.core.Name("trueFalse");

  public static final hydra.core.Name T_NIL = new hydra.core.Name("tNil");

  public static final hydra.core.Name HASH_T_F = new hydra.core.Name("hashTF");

  private BooleanStyle () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(TrueFalse instance) ;

    R visit(TNil instance) ;

    R visit(HashTF instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BooleanStyle instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(TrueFalse instance) {
      return otherwise(instance);
    }

    default R visit(TNil instance) {
      return otherwise(instance);
    }

    default R visit(HashTF instance) {
      return otherwise(instance);
    }
  }

  public static final class TrueFalse extends hydra.lisp.syntax.BooleanStyle implements Serializable {
    public TrueFalse () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TrueFalse)) {
        return false;
      }
      TrueFalse o = (TrueFalse) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BooleanStyle other) {
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

  public static final class TNil extends hydra.lisp.syntax.BooleanStyle implements Serializable {
    public TNil () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TNil)) {
        return false;
      }
      TNil o = (TNil) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BooleanStyle other) {
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

  public static final class HashTF extends hydra.lisp.syntax.BooleanStyle implements Serializable {
    public HashTF () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HashTF)) {
        return false;
      }
      HashTF o = (HashTF) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BooleanStyle other) {
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
