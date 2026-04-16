// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NodeTypeImpliedContent implements Serializable, Comparable<NodeTypeImpliedContent> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NodeTypeImpliedContent");

  public static final hydra.core.Name LABEL_SET = new hydra.core.Name("labelSet");

  public static final hydra.core.Name PROPERTY_TYPES = new hydra.core.Name("propertyTypes");

  public static final hydra.core.Name LABEL_SET_WITH_PROPERTIES = new hydra.core.Name("labelSetWithProperties");

  private NodeTypeImpliedContent () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(LabelSet instance) ;

    R visit(PropertyTypes instance) ;

    R visit(LabelSetWithProperties instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NodeTypeImpliedContent instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(LabelSet instance) {
      return otherwise(instance);
    }

    default R visit(PropertyTypes instance) {
      return otherwise(instance);
    }

    default R visit(LabelSetWithProperties instance) {
      return otherwise(instance);
    }
  }

  public static final class LabelSet extends openGql.grammar.NodeTypeImpliedContent implements Serializable {
    public final openGql.grammar.LabelSetPhrase value;

    public LabelSet (openGql.grammar.LabelSetPhrase value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LabelSet)) {
        return false;
      }
      LabelSet o = (LabelSet) other;
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
    public int compareTo(NodeTypeImpliedContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LabelSet o = (LabelSet) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PropertyTypes extends openGql.grammar.NodeTypeImpliedContent implements Serializable {
    public final hydra.util.Maybe<java.util.List<openGql.grammar.PropertyType>> value;

    public PropertyTypes (hydra.util.Maybe<java.util.List<openGql.grammar.PropertyType>> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyTypes)) {
        return false;
      }
      PropertyTypes o = (PropertyTypes) other;
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
    public int compareTo(NodeTypeImpliedContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PropertyTypes o = (PropertyTypes) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LabelSetWithProperties extends openGql.grammar.NodeTypeImpliedContent implements Serializable {
    public final openGql.grammar.NodeLabelSetWithProperties value;

    public LabelSetWithProperties (openGql.grammar.NodeLabelSetWithProperties value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LabelSetWithProperties)) {
        return false;
      }
      LabelSetWithProperties o = (LabelSetWithProperties) other;
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
    public int compareTo(NodeTypeImpliedContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LabelSetWithProperties o = (LabelSetWithProperties) other;
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
