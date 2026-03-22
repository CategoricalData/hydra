// Note: this is an automatically generated file. Do not edit.

package hydra.pg.mapping;

import java.io.Serializable;

/**
 * A mapping specification producing values (usually literal values) whose type is understood in context
 */
public abstract class ValueSpec implements Serializable, Comparable<ValueSpec> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.mapping.ValueSpec");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  private ValueSpec () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Value instance) ;

    R visit(Pattern instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ValueSpec instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Value instance) {
      return otherwise(instance);
    }

    default R visit(Pattern instance) {
      return otherwise(instance);
    }
  }

  /**
   * A trivial no-op specification which passes the entire value
   */
  public static final class Value extends hydra.pg.mapping.ValueSpec implements Serializable {
    public Value () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ValueSpec other) {
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

  /**
   * A compact path representing the function, e.g. engine-${engineInfo/model/name}
   */
  public static final class Pattern extends hydra.pg.mapping.ValueSpec implements Serializable {
    public final String value;

    public Pattern (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) other;
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
    public int compareTo(ValueSpec other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pattern o = (Pattern) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
