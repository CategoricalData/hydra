// Note: this is an automatically generated file. Do not edit.

package com.gdblab.pathAlgebra.syntax;

import java.io.Serializable;

public abstract class BoolOp implements Serializable, Comparable<BoolOp> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("com.gdblab.pathAlgebra.syntax.BoolOp");

  public static final hydra.core.Name AND = new hydra.core.Name("and");

  public static final hydra.core.Name OR = new hydra.core.Name("or");

  private BoolOp () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(And instance) ;

    R visit(Or instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BoolOp instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(And instance) {
      return otherwise(instance);
    }

    default R visit(Or instance) {
      return otherwise(instance);
    }
  }

  public static final class And extends com.gdblab.pathAlgebra.syntax.BoolOp implements Serializable {
    public And () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BoolOp other) {
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

  public static final class Or extends com.gdblab.pathAlgebra.syntax.BoolOp implements Serializable {
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
    public int compareTo(BoolOp other) {
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
