// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public abstract class AddOrSubtractOperator implements Serializable, Comparable<AddOrSubtractOperator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.AddOrSubtractOperator");

  public static final hydra.core.Name ADD = new hydra.core.Name("add");

  public static final hydra.core.Name SUBTRACT = new hydra.core.Name("subtract");

  private AddOrSubtractOperator () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Add instance) ;

    R visit(Subtract instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AddOrSubtractOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Add instance) {
      return otherwise(instance);
    }

    default R visit(Subtract instance) {
      return otherwise(instance);
    }
  }

  public static final class Add extends hydra.cypher.openCypher.AddOrSubtractOperator implements Serializable {
    public Add () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Add)) {
        return false;
      }
      Add o = (Add) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AddOrSubtractOperator other) {
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

  public static final class Subtract extends hydra.cypher.openCypher.AddOrSubtractOperator implements Serializable {
    public Subtract () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subtract)) {
        return false;
      }
      Subtract o = (Subtract) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AddOrSubtractOperator other) {
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
