// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class BindingTableReference implements Serializable, Comparable<BindingTableReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BindingTableReference");

  public static final hydra.core.Name PARENT_AND_TABLE_NAME = new hydra.core.Name("parentAndTableName");

  public static final hydra.core.Name DELIMITED_BINDING_TABLE_NAME = new hydra.core.Name("delimitedBindingTableName");

  public static final hydra.core.Name PARAMETER_SPECIFICATION = new hydra.core.Name("parameterSpecification");

  private BindingTableReference () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ParentAndTableName instance) ;

    R visit(DelimitedBindingTableName instance) ;

    R visit(ParameterSpecification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BindingTableReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ParentAndTableName instance) {
      return otherwise(instance);
    }

    default R visit(DelimitedBindingTableName instance) {
      return otherwise(instance);
    }

    default R visit(ParameterSpecification instance) {
      return otherwise(instance);
    }
  }

  public static final class ParentAndTableName extends openGql.grammar.BindingTableReference implements Serializable {
    public final openGql.grammar.ParentAndTableName value;

    public ParentAndTableName (openGql.grammar.ParentAndTableName value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParentAndTableName)) {
        return false;
      }
      ParentAndTableName o = (ParentAndTableName) other;
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
    public int compareTo(BindingTableReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ParentAndTableName o = (ParentAndTableName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DelimitedBindingTableName extends openGql.grammar.BindingTableReference implements Serializable {
    public final String value;

    public DelimitedBindingTableName (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DelimitedBindingTableName)) {
        return false;
      }
      DelimitedBindingTableName o = (DelimitedBindingTableName) other;
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
    public int compareTo(BindingTableReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DelimitedBindingTableName o = (DelimitedBindingTableName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ParameterSpecification extends openGql.grammar.BindingTableReference implements Serializable {
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
    public int compareTo(BindingTableReference other) {
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
