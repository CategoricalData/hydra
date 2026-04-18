// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TemporalType implements Serializable, Comparable<TemporalType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TemporalType");

  public static final hydra.core.Name INSTANT_TYPE = new hydra.core.Name("instantType");

  public static final hydra.core.Name DURATION_TYPE = new hydra.core.Name("durationType");

  private TemporalType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(InstantType instance) ;

    R visit(DurationType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TemporalType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(InstantType instance) {
      return otherwise(instance);
    }

    default R visit(DurationType instance) {
      return otherwise(instance);
    }
  }

  public static final class InstantType extends openGql.grammar.TemporalType implements Serializable {
    public final openGql.grammar.TemporalInstantType value;

    public InstantType (openGql.grammar.TemporalInstantType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InstantType)) {
        return false;
      }
      InstantType o = (InstantType) other;
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
    public int compareTo(TemporalType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InstantType o = (InstantType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DurationType extends openGql.grammar.TemporalType implements Serializable {
    public final openGql.grammar.TemporalDurationType value;

    public DurationType (openGql.grammar.TemporalDurationType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DurationType)) {
        return false;
      }
      DurationType o = (DurationType) other;
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
    public int compareTo(TemporalType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DurationType o = (DurationType) other;
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
