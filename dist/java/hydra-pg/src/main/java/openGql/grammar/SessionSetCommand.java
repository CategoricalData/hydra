// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SessionSetCommand implements Serializable, Comparable<SessionSetCommand> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SessionSetCommand");

  public static final hydra.core.Name SCHEMA = new hydra.core.Name("schema");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public static final hydra.core.Name TIME_ZONE = new hydra.core.Name("timeZone");

  public static final hydra.core.Name PARAMETER = new hydra.core.Name("parameter");

  private SessionSetCommand () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Schema instance) ;

    R visit(Graph instance) ;

    R visit(TimeZone instance) ;

    R visit(Parameter instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SessionSetCommand instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Schema instance) {
      return otherwise(instance);
    }

    default R visit(Graph instance) {
      return otherwise(instance);
    }

    default R visit(TimeZone instance) {
      return otherwise(instance);
    }

    default R visit(Parameter instance) {
      return otherwise(instance);
    }
  }

  public static final class Schema extends openGql.grammar.SessionSetCommand implements Serializable {
    public final openGql.grammar.SchemaReference value;

    public Schema (openGql.grammar.SchemaReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Schema)) {
        return false;
      }
      Schema o = (Schema) other;
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
    public int compareTo(SessionSetCommand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Schema o = (Schema) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Graph extends openGql.grammar.SessionSetCommand implements Serializable {
    public final openGql.grammar.GraphExpression value;

    public Graph (openGql.grammar.GraphExpression value) {
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
    public int compareTo(SessionSetCommand other) {
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

  public static final class TimeZone extends openGql.grammar.SessionSetCommand implements Serializable {
    public final String value;

    public TimeZone (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimeZone)) {
        return false;
      }
      TimeZone o = (TimeZone) other;
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
    public int compareTo(SessionSetCommand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TimeZone o = (TimeZone) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Parameter extends openGql.grammar.SessionSetCommand implements Serializable {
    public final openGql.grammar.SessionSetParameterClause value;

    public Parameter (openGql.grammar.SessionSetParameterClause value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parameter)) {
        return false;
      }
      Parameter o = (Parameter) other;
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
    public int compareTo(SessionSetCommand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parameter o = (Parameter) other;
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
