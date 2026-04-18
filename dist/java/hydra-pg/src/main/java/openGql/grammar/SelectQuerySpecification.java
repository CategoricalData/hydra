// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SelectQuerySpecification implements Serializable, Comparable<SelectQuerySpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SelectQuerySpecification");

  public static final hydra.core.Name NESTED = new hydra.core.Name("nested");

  public static final hydra.core.Name GRAPH_AND_NESTED = new hydra.core.Name("graphAndNested");

  private SelectQuerySpecification () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Nested instance) ;

    R visit(GraphAndNested instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SelectQuerySpecification instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Nested instance) {
      return otherwise(instance);
    }

    default R visit(GraphAndNested instance) {
      return otherwise(instance);
    }
  }

  public static final class Nested extends openGql.grammar.SelectQuerySpecification implements Serializable {
    public final openGql.grammar.ProcedureBody value;

    public Nested (openGql.grammar.ProcedureBody value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nested)) {
        return false;
      }
      Nested o = (Nested) other;
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
    public int compareTo(SelectQuerySpecification other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Nested o = (Nested) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class GraphAndNested extends openGql.grammar.SelectQuerySpecification implements Serializable {
    public final openGql.grammar.GraphAndNestedQuerySpecification value;

    public GraphAndNested (openGql.grammar.GraphAndNestedQuerySpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GraphAndNested)) {
        return false;
      }
      GraphAndNested o = (GraphAndNested) other;
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
    public int compareTo(SelectQuerySpecification other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GraphAndNested o = (GraphAndNested) other;
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
