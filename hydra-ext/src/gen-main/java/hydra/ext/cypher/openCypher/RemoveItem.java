// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class RemoveItem implements Serializable, Comparable<RemoveItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.RemoveItem");

  public static final hydra.core.Name VARIABLE_LABELS = new hydra.core.Name("variableLabels");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  private RemoveItem () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(VariableLabels instance) ;

    R visit(Property instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RemoveItem instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(VariableLabels instance) {
      return otherwise(instance);
    }

    default R visit(Property instance) {
      return otherwise(instance);
    }
  }

  public static final class VariableLabels extends hydra.ext.cypher.openCypher.RemoveItem implements Serializable {
    public final hydra.ext.cypher.openCypher.VariableAndNodeLabels value;

    public VariableLabels (hydra.ext.cypher.openCypher.VariableAndNodeLabels value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariableLabels)) {
        return false;
      }
      VariableLabels o = (VariableLabels) other;
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
    public int compareTo(RemoveItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      VariableLabels o = (VariableLabels) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Property extends hydra.ext.cypher.openCypher.RemoveItem implements Serializable {
    public final hydra.ext.cypher.openCypher.PropertyExpression value;

    public Property (hydra.ext.cypher.openCypher.PropertyExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) other;
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
    public int compareTo(RemoveItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Property o = (Property) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
