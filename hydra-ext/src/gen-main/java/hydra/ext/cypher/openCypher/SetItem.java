// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class SetItem implements Serializable, Comparable<SetItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.SetItem");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name VARIABLE_EQUAL = new hydra.core.Name("variableEqual");

  public static final hydra.core.Name VARIABLE_PLUS_EQUAL = new hydra.core.Name("variablePlusEqual");

  public static final hydra.core.Name VARIABLE_LABELS = new hydra.core.Name("variableLabels");

  private SetItem () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Property instance) ;

    R visit(VariableEqual instance) ;

    R visit(VariablePlusEqual instance) ;

    R visit(VariableLabels instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SetItem instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Property instance) {
      return otherwise(instance);
    }

    default R visit(VariableEqual instance) {
      return otherwise(instance);
    }

    default R visit(VariablePlusEqual instance) {
      return otherwise(instance);
    }

    default R visit(VariableLabels instance) {
      return otherwise(instance);
    }
  }

  public static final class Property extends hydra.ext.cypher.openCypher.SetItem implements Serializable {
    public final hydra.ext.cypher.openCypher.PropertyEquals value;

    public Property (hydra.ext.cypher.openCypher.PropertyEquals value) {
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
    public int compareTo(SetItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Property o = (Property) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class VariableEqual extends hydra.ext.cypher.openCypher.SetItem implements Serializable {
    public final hydra.ext.cypher.openCypher.VariableEquals value;

    public VariableEqual (hydra.ext.cypher.openCypher.VariableEquals value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariableEqual)) {
        return false;
      }
      VariableEqual o = (VariableEqual) other;
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
    public int compareTo(SetItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      VariableEqual o = (VariableEqual) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class VariablePlusEqual extends hydra.ext.cypher.openCypher.SetItem implements Serializable {
    public final hydra.ext.cypher.openCypher.VariablePlusEquals value;

    public VariablePlusEqual (hydra.ext.cypher.openCypher.VariablePlusEquals value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariablePlusEqual)) {
        return false;
      }
      VariablePlusEqual o = (VariablePlusEqual) other;
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
    public int compareTo(SetItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      VariablePlusEqual o = (VariablePlusEqual) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class VariableLabels extends hydra.ext.cypher.openCypher.SetItem implements Serializable {
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
    public int compareTo(SetItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      VariableLabels o = (VariableLabels) other;
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
