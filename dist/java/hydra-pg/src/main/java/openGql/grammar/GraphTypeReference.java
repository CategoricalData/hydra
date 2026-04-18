// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class GraphTypeReference implements Serializable, Comparable<GraphTypeReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GraphTypeReference");

  public static final hydra.core.Name PARENT_AND_TYPE_NAME = new hydra.core.Name("parentAndTypeName");

  public static final hydra.core.Name PARAMETER_SPECIFICATION = new hydra.core.Name("parameterSpecification");

  private GraphTypeReference () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ParentAndTypeName instance) ;

    R visit(ParameterSpecification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GraphTypeReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ParentAndTypeName instance) {
      return otherwise(instance);
    }

    default R visit(ParameterSpecification instance) {
      return otherwise(instance);
    }
  }

  public static final class ParentAndTypeName extends openGql.grammar.GraphTypeReference implements Serializable {
    public final openGql.grammar.CatalogGraphTypeParentAndName value;

    public ParentAndTypeName (openGql.grammar.CatalogGraphTypeParentAndName value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParentAndTypeName)) {
        return false;
      }
      ParentAndTypeName o = (ParentAndTypeName) other;
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
    public int compareTo(GraphTypeReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ParentAndTypeName o = (ParentAndTypeName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ParameterSpecification extends openGql.grammar.GraphTypeReference implements Serializable {
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
    public int compareTo(GraphTypeReference other) {
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
