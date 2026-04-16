// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class MultDivOperator implements Serializable, Comparable<MultDivOperator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.MultDivOperator");

  public static final hydra.core.Name MULTIPLY = new hydra.core.Name("multiply");

  public static final hydra.core.Name DIVIDE = new hydra.core.Name("divide");

  private MultDivOperator () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Multiply instance) ;

    R visit(Divide instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MultDivOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Multiply instance) {
      return otherwise(instance);
    }

    default R visit(Divide instance) {
      return otherwise(instance);
    }
  }

  public static final class Multiply extends openGql.grammar.MultDivOperator implements Serializable {
    public Multiply () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Multiply)) {
        return false;
      }
      Multiply o = (Multiply) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MultDivOperator other) {
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

  public static final class Divide extends openGql.grammar.MultDivOperator implements Serializable {
    public Divide () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Divide)) {
        return false;
      }
      Divide o = (Divide) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MultDivOperator other) {
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
