// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EdgeReferenceValueType implements Serializable, Comparable<EdgeReferenceValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeReferenceValueType");

  public static final hydra.core.Name OPEN = new hydra.core.Name("open");

  public static final hydra.core.Name CLOSED = new hydra.core.Name("closed");

  private EdgeReferenceValueType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Open instance) ;

    R visit(Closed instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgeReferenceValueType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Open instance) {
      return otherwise(instance);
    }

    default R visit(Closed instance) {
      return otherwise(instance);
    }
  }

  public static final class Open extends openGql.grammar.EdgeReferenceValueType implements Serializable {
    public final openGql.grammar.OpenEdgeReferenceValueType value;

    public Open (openGql.grammar.OpenEdgeReferenceValueType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Open)) {
        return false;
      }
      Open o = (Open) other;
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
    public int compareTo(EdgeReferenceValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Open o = (Open) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Closed extends openGql.grammar.EdgeReferenceValueType implements Serializable {
    public final openGql.grammar.ClosedEdgeReferenceValueType value;

    public Closed (openGql.grammar.ClosedEdgeReferenceValueType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Closed)) {
        return false;
      }
      Closed o = (Closed) other;
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
    public int compareTo(EdgeReferenceValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Closed o = (Closed) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
