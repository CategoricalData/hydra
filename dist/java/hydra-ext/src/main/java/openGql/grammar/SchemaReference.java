// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SchemaReference implements Serializable, Comparable<SchemaReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SchemaReference");

  public static final hydra.core.Name ABSOLUTE_REFERENCE = new hydra.core.Name("absoluteReference");

  public static final hydra.core.Name RELATIVE_REFERENCE = new hydra.core.Name("relativeReference");

  public static final hydra.core.Name PARAMETER_SPECIFICATION = new hydra.core.Name("parameterSpecification");

  private SchemaReference () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(AbsoluteReference instance) ;

    R visit(RelativeReference instance) ;

    R visit(ParameterSpecification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SchemaReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(AbsoluteReference instance) {
      return otherwise(instance);
    }

    default R visit(RelativeReference instance) {
      return otherwise(instance);
    }

    default R visit(ParameterSpecification instance) {
      return otherwise(instance);
    }
  }

  public static final class AbsoluteReference extends openGql.grammar.SchemaReference implements Serializable {
    public final openGql.grammar.AbsoluteCatalogSchemaReference value;

    public AbsoluteReference (openGql.grammar.AbsoluteCatalogSchemaReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AbsoluteReference)) {
        return false;
      }
      AbsoluteReference o = (AbsoluteReference) other;
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
    public int compareTo(SchemaReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AbsoluteReference o = (AbsoluteReference) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class RelativeReference extends openGql.grammar.SchemaReference implements Serializable {
    public final openGql.grammar.RelativeCatalogSchemaReference value;

    public RelativeReference (openGql.grammar.RelativeCatalogSchemaReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RelativeReference)) {
        return false;
      }
      RelativeReference o = (RelativeReference) other;
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
    public int compareTo(SchemaReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RelativeReference o = (RelativeReference) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ParameterSpecification extends openGql.grammar.SchemaReference implements Serializable {
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
    public int compareTo(SchemaReference other) {
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
