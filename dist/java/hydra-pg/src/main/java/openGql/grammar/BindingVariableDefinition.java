// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class BindingVariableDefinition implements Serializable, Comparable<BindingVariableDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BindingVariableDefinition");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public static final hydra.core.Name TABLE = new hydra.core.Name("table");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  private BindingVariableDefinition () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Graph instance) ;

    R visit(Table instance) ;

    R visit(Value instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BindingVariableDefinition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Graph instance) {
      return otherwise(instance);
    }

    default R visit(Table instance) {
      return otherwise(instance);
    }

    default R visit(Value instance) {
      return otherwise(instance);
    }
  }

  public static final class Graph extends openGql.grammar.BindingVariableDefinition implements Serializable {
    public final openGql.grammar.GraphVariableDefinition value;

    public Graph (openGql.grammar.GraphVariableDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Graph)) {
        return false;
      }
      Graph o = (Graph) other;
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
    public int compareTo(BindingVariableDefinition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Graph o = (Graph) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Table extends openGql.grammar.BindingVariableDefinition implements Serializable {
    public final openGql.grammar.BindingTableVariableDefinition value;

    public Table (openGql.grammar.BindingTableVariableDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Table)) {
        return false;
      }
      Table o = (Table) other;
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
    public int compareTo(BindingVariableDefinition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Table o = (Table) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Value extends openGql.grammar.BindingVariableDefinition implements Serializable {
    public final openGql.grammar.ValueVariableDefinition value;

    public Value (openGql.grammar.ValueVariableDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) other;
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
    public int compareTo(BindingVariableDefinition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Value o = (Value) other;
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
