// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class OrderingSpecification implements Serializable, Comparable<OrderingSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OrderingSpecification");

  public static final hydra.core.Name ASCENDING = new hydra.core.Name("ascending");

  public static final hydra.core.Name DESCENDING = new hydra.core.Name("descending");

  private OrderingSpecification () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Ascending instance) ;

    R visit(Descending instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OrderingSpecification instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Ascending instance) {
      return otherwise(instance);
    }

    default R visit(Descending instance) {
      return otherwise(instance);
    }
  }

  public static final class Ascending extends openGql.grammar.OrderingSpecification implements Serializable {
    public Ascending () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ascending)) {
        return false;
      }
      Ascending o = (Ascending) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrderingSpecification other) {
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

  public static final class Descending extends openGql.grammar.OrderingSpecification implements Serializable {
    public Descending () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Descending)) {
        return false;
      }
      Descending o = (Descending) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OrderingSpecification other) {
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
