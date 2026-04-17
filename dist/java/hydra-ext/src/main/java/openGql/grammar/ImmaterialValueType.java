// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ImmaterialValueType implements Serializable, Comparable<ImmaterialValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ImmaterialValueType");

  public static final hydra.core.Name NULL_TYPE = new hydra.core.Name("nullType");

  public static final hydra.core.Name EMPTY_TYPE = new hydra.core.Name("emptyType");

  private ImmaterialValueType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(NullType instance) ;

    R visit(EmptyType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImmaterialValueType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(NullType instance) {
      return otherwise(instance);
    }

    default R visit(EmptyType instance) {
      return otherwise(instance);
    }
  }

  public static final class NullType extends openGql.grammar.ImmaterialValueType implements Serializable {
    public final java.lang.Void value;

    public NullType (java.lang.Void value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullType)) {
        return false;
      }
      NullType o = (NullType) other;
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
    public int compareTo(ImmaterialValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NullType o = (NullType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class EmptyType extends openGql.grammar.ImmaterialValueType implements Serializable {
    public final java.lang.Void value;

    public EmptyType (java.lang.Void value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptyType)) {
        return false;
      }
      EmptyType o = (EmptyType) other;
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
    public int compareTo(ImmaterialValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EmptyType o = (EmptyType) other;
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
