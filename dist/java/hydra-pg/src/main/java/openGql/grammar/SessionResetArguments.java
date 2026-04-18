// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SessionResetArguments implements Serializable, Comparable<SessionResetArguments> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SessionResetArguments");

  public static final hydra.core.Name PARAMETERS_OR_CHARACTERISTICS = new hydra.core.Name("parametersOrCharacteristics");

  public static final hydra.core.Name SCHEMA = new hydra.core.Name("schema");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public static final hydra.core.Name TIME_ZONE = new hydra.core.Name("timeZone");

  public static final hydra.core.Name PARAMETER_SESSION_SPECIFICATION = new hydra.core.Name("parameterSessionSpecification");

  private SessionResetArguments () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ParametersOrCharacteristics instance) ;

    R visit(Schema instance) ;

    R visit(Graph instance) ;

    R visit(TimeZone instance) ;

    R visit(ParameterSessionSpecification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SessionResetArguments instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ParametersOrCharacteristics instance) {
      return otherwise(instance);
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

    default R visit(ParameterSessionSpecification instance) {
      return otherwise(instance);
    }
  }

  public static final class ParametersOrCharacteristics extends openGql.grammar.SessionResetArguments implements Serializable {
    public final openGql.grammar.AllParametersOrCharacteristics value;

    public ParametersOrCharacteristics (openGql.grammar.AllParametersOrCharacteristics value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParametersOrCharacteristics)) {
        return false;
      }
      ParametersOrCharacteristics o = (ParametersOrCharacteristics) other;
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
    public int compareTo(SessionResetArguments other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ParametersOrCharacteristics o = (ParametersOrCharacteristics) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Schema extends openGql.grammar.SessionResetArguments implements Serializable {
    public Schema () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Schema)) {
        return false;
      }
      Schema o = (Schema) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SessionResetArguments other) {
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

  public static final class Graph extends openGql.grammar.SessionResetArguments implements Serializable {
    public Graph () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Graph)) {
        return false;
      }
      Graph o = (Graph) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SessionResetArguments other) {
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

  public static final class TimeZone extends openGql.grammar.SessionResetArguments implements Serializable {
    public TimeZone () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimeZone)) {
        return false;
      }
      TimeZone o = (TimeZone) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SessionResetArguments other) {
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

  public static final class ParameterSessionSpecification extends openGql.grammar.SessionResetArguments implements Serializable {
    public final openGql.grammar.ParameterSessionSpecification value;

    public ParameterSessionSpecification (openGql.grammar.ParameterSessionSpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParameterSessionSpecification)) {
        return false;
      }
      ParameterSessionSpecification o = (ParameterSessionSpecification) other;
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
    public int compareTo(SessionResetArguments other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ParameterSessionSpecification o = (ParameterSessionSpecification) other;
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
