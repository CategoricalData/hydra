// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ProcedureReference implements Serializable, Comparable<ProcedureReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ProcedureReference");

  public static final hydra.core.Name PARENT_AND_PROCEDURE_NAME = new hydra.core.Name("parentAndProcedureName");

  public static final hydra.core.Name PARAMETER_SPECIFICATION = new hydra.core.Name("parameterSpecification");

  private ProcedureReference () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ParentAndProcedureName instance) ;

    R visit(ParameterSpecification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ProcedureReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ParentAndProcedureName instance) {
      return otherwise(instance);
    }

    default R visit(ParameterSpecification instance) {
      return otherwise(instance);
    }
  }

  public static final class ParentAndProcedureName extends openGql.grammar.ProcedureReference implements Serializable {
    public final openGql.grammar.CatalogProcedureParentAndName value;

    public ParentAndProcedureName (openGql.grammar.CatalogProcedureParentAndName value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParentAndProcedureName)) {
        return false;
      }
      ParentAndProcedureName o = (ParentAndProcedureName) other;
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
    public int compareTo(ProcedureReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ParentAndProcedureName o = (ParentAndProcedureName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ParameterSpecification extends openGql.grammar.ProcedureReference implements Serializable {
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
    public int compareTo(ProcedureReference other) {
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
