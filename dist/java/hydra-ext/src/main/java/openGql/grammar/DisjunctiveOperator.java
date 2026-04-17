// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DisjunctiveOperator implements Serializable, Comparable<DisjunctiveOperator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DisjunctiveOperator");

  public static final hydra.core.Name OR = new hydra.core.Name("or");

  public static final hydra.core.Name XOR = new hydra.core.Name("xor");

  private DisjunctiveOperator () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Or instance) ;

    R visit(Xor instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DisjunctiveOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Or instance) {
      return otherwise(instance);
    }

    default R visit(Xor instance) {
      return otherwise(instance);
    }
  }

  public static final class Or extends openGql.grammar.DisjunctiveOperator implements Serializable {
    public Or () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DisjunctiveOperator other) {
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

  public static final class Xor extends openGql.grammar.DisjunctiveOperator implements Serializable {
    public Xor () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Xor)) {
        return false;
      }
      Xor o = (Xor) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DisjunctiveOperator other) {
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
