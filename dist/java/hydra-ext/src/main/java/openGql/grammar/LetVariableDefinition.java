// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class LetVariableDefinition implements Serializable, Comparable<LetVariableDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LetVariableDefinition");

  public static final hydra.core.Name VALUE_VARIABLE = new hydra.core.Name("valueVariable");

  public static final hydra.core.Name BINDING_EQUALS_VALUE = new hydra.core.Name("bindingEqualsValue");

  private LetVariableDefinition () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ValueVariable instance) ;

    R visit(BindingEqualsValue instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LetVariableDefinition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ValueVariable instance) {
      return otherwise(instance);
    }

    default R visit(BindingEqualsValue instance) {
      return otherwise(instance);
    }
  }

  public static final class ValueVariable extends openGql.grammar.LetVariableDefinition implements Serializable {
    public final openGql.grammar.ValueVariableDefinition value;

    public ValueVariable (openGql.grammar.ValueVariableDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ValueVariable)) {
        return false;
      }
      ValueVariable o = (ValueVariable) other;
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
    public int compareTo(LetVariableDefinition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ValueVariable o = (ValueVariable) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BindingEqualsValue extends openGql.grammar.LetVariableDefinition implements Serializable {
    public final openGql.grammar.BindingEqualsValue value;

    public BindingEqualsValue (openGql.grammar.BindingEqualsValue value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BindingEqualsValue)) {
        return false;
      }
      BindingEqualsValue o = (BindingEqualsValue) other;
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
    public int compareTo(LetVariableDefinition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BindingEqualsValue o = (BindingEqualsValue) other;
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
