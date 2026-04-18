// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EdgeTypePatternDirected implements Serializable, Comparable<EdgeTypePatternDirected> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeTypePatternDirected");

  public static final hydra.core.Name POINTING_RIGHT = new hydra.core.Name("pointingRight");

  public static final hydra.core.Name POINTING_LEFT = new hydra.core.Name("pointingLeft");

  private EdgeTypePatternDirected () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PointingRight instance) ;

    R visit(PointingLeft instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgeTypePatternDirected instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PointingRight instance) {
      return otherwise(instance);
    }

    default R visit(PointingLeft instance) {
      return otherwise(instance);
    }
  }

  public static final class PointingRight extends openGql.grammar.EdgeTypePatternDirected implements Serializable {
    public final openGql.grammar.EdgeTypePatternPointingRight value;

    public PointingRight (openGql.grammar.EdgeTypePatternPointingRight value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PointingRight)) {
        return false;
      }
      PointingRight o = (PointingRight) other;
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
    public int compareTo(EdgeTypePatternDirected other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PointingRight o = (PointingRight) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PointingLeft extends openGql.grammar.EdgeTypePatternDirected implements Serializable {
    public final openGql.grammar.EdgeTypePatternPointingLeft value;

    public PointingLeft (openGql.grammar.EdgeTypePatternPointingLeft value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PointingLeft)) {
        return false;
      }
      PointingLeft o = (PointingLeft) other;
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
    public int compareTo(EdgeTypePatternDirected other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PointingLeft o = (PointingLeft) other;
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
