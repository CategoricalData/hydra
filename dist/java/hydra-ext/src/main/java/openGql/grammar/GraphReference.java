// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class GraphReference implements Serializable, Comparable<GraphReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GraphReference");

  public static final hydra.core.Name PARENT_AND_GRAPH_NAME = new hydra.core.Name("parentAndGraphName");

  public static final hydra.core.Name DELIMITED_GRAPH_NAME = new hydra.core.Name("delimitedGraphName");

  public static final hydra.core.Name HOME_GRAPH = new hydra.core.Name("homeGraph");

  public static final hydra.core.Name PARAMETER_SPECIFICATION = new hydra.core.Name("parameterSpecification");

  private GraphReference () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ParentAndGraphName instance) ;

    R visit(DelimitedGraphName instance) ;

    R visit(HomeGraph instance) ;

    R visit(ParameterSpecification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GraphReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ParentAndGraphName instance) {
      return otherwise(instance);
    }

    default R visit(DelimitedGraphName instance) {
      return otherwise(instance);
    }

    default R visit(HomeGraph instance) {
      return otherwise(instance);
    }

    default R visit(ParameterSpecification instance) {
      return otherwise(instance);
    }
  }

  public static final class ParentAndGraphName extends openGql.grammar.GraphReference implements Serializable {
    public final openGql.grammar.ParentAndGraphName value;

    public ParentAndGraphName (openGql.grammar.ParentAndGraphName value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParentAndGraphName)) {
        return false;
      }
      ParentAndGraphName o = (ParentAndGraphName) other;
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
    public int compareTo(GraphReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ParentAndGraphName o = (ParentAndGraphName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DelimitedGraphName extends openGql.grammar.GraphReference implements Serializable {
    public final String value;

    public DelimitedGraphName (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DelimitedGraphName)) {
        return false;
      }
      DelimitedGraphName o = (DelimitedGraphName) other;
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
    public int compareTo(GraphReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DelimitedGraphName o = (DelimitedGraphName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class HomeGraph extends openGql.grammar.GraphReference implements Serializable {
    public final openGql.grammar.HomeGraph value;

    public HomeGraph (openGql.grammar.HomeGraph value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HomeGraph)) {
        return false;
      }
      HomeGraph o = (HomeGraph) other;
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
    public int compareTo(GraphReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HomeGraph o = (HomeGraph) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ParameterSpecification extends openGql.grammar.GraphReference implements Serializable {
    public final java.lang.Void value;

    public ParameterSpecification (java.lang.Void value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParameterSpecification)) {
        return false;
      }
      ParameterSpecification o = (ParameterSpecification) other;
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
    public int compareTo(GraphReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ParameterSpecification o = (ParameterSpecification) other;
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
