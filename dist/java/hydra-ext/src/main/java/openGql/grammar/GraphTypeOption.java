// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class GraphTypeOption implements Serializable, Comparable<GraphTypeOption> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GraphTypeOption");

  public static final hydra.core.Name OPEN_GRAPH_TYPE = new hydra.core.Name("openGraphType");

  public static final hydra.core.Name OF_GRAPH_TYPE = new hydra.core.Name("ofGraphType");

  private GraphTypeOption () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(OpenGraphType instance) ;

    R visit(OfGraphType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GraphTypeOption instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(OpenGraphType instance) {
      return otherwise(instance);
    }

    default R visit(OfGraphType instance) {
      return otherwise(instance);
    }
  }

  public static final class OpenGraphType extends openGql.grammar.GraphTypeOption implements Serializable {
    public final openGql.grammar.OpenGraphType value;

    public OpenGraphType (openGql.grammar.OpenGraphType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OpenGraphType)) {
        return false;
      }
      OpenGraphType o = (OpenGraphType) other;
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
    public int compareTo(GraphTypeOption other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OpenGraphType o = (OpenGraphType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OfGraphType extends openGql.grammar.GraphTypeOption implements Serializable {
    public final openGql.grammar.OfGraphType value;

    public OfGraphType (openGql.grammar.OfGraphType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OfGraphType)) {
        return false;
      }
      OfGraphType o = (OfGraphType) other;
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
    public int compareTo(GraphTypeOption other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OfGraphType o = (OfGraphType) other;
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
