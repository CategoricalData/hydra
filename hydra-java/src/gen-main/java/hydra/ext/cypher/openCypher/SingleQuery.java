// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class SingleQuery implements Serializable, Comparable<SingleQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.SingleQuery");

  public static final hydra.core.Name SINGLE_PART = new hydra.core.Name("singlePart");

  public static final hydra.core.Name MULTI_PART = new hydra.core.Name("multiPart");

  private SingleQuery () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(SinglePart instance) ;

    R visit(MultiPart instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SingleQuery instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(SinglePart instance) {
      return otherwise(instance);
    }

    default R visit(MultiPart instance) {
      return otherwise(instance);
    }
  }

  public static final class SinglePart extends hydra.ext.cypher.openCypher.SingleQuery implements Serializable {
    public final hydra.ext.cypher.openCypher.SinglePartQuery value;

    public SinglePart (hydra.ext.cypher.openCypher.SinglePartQuery value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SinglePart)) {
        return false;
      }
      SinglePart o = (SinglePart) other;
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
    public int compareTo(SingleQuery other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SinglePart o = (SinglePart) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MultiPart extends hydra.ext.cypher.openCypher.SingleQuery implements Serializable {
    public final hydra.ext.cypher.openCypher.MultiPartQuery value;

    public MultiPart (hydra.ext.cypher.openCypher.MultiPartQuery value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiPart)) {
        return false;
      }
      MultiPart o = (MultiPart) other;
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
    public int compareTo(SingleQuery other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MultiPart o = (MultiPart) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
