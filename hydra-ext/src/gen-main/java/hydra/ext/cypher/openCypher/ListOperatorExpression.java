// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class ListOperatorExpression implements Serializable, Comparable<ListOperatorExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.ListOperatorExpression");

  public static final hydra.core.Name SINGLE = new hydra.core.Name("single");

  public static final hydra.core.Name RANGE = new hydra.core.Name("range");

  private ListOperatorExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Single instance) ;

    R visit(Range instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ListOperatorExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Single instance) {
      return otherwise(instance);
    }

    default R visit(Range instance) {
      return otherwise(instance);
    }
  }

  public static final class Single extends hydra.ext.cypher.openCypher.ListOperatorExpression implements Serializable {
    public final hydra.ext.cypher.openCypher.Expression value;

    public Single (hydra.ext.cypher.openCypher.Expression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Single)) {
        return false;
      }
      Single o = (Single) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ListOperatorExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Single o = (Single) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Range extends hydra.ext.cypher.openCypher.ListOperatorExpression implements Serializable {
    public final hydra.ext.cypher.openCypher.RangeExpression value;

    public Range (hydra.ext.cypher.openCypher.RangeExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Range)) {
        return false;
      }
      Range o = (Range) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ListOperatorExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Range o = (Range) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
