// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class StringArgument implements Serializable, Comparable<StringArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringArgument");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  private StringArgument () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Value instance) ;

    R visit(Variable instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringArgument instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Value instance) {
      return otherwise(instance);
    }

    default R visit(Variable instance) {
      return otherwise(instance);
    }
  }

  public static final class Value extends hydra.ext.org.apache.tinkerpop.gremlin.StringArgument implements Serializable {
    public final String value;

    public Value (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) other;
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
    public int compareTo(StringArgument other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Value o = (Value) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Variable extends hydra.ext.org.apache.tinkerpop.gremlin.StringArgument implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.Identifier value;

    public Variable (hydra.ext.org.apache.tinkerpop.gremlin.Identifier value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) other;
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
    public int compareTo(StringArgument other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Variable o = (Variable) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
