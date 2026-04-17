// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class LinearQueryStatement implements Serializable, Comparable<LinearQueryStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LinearQueryStatement");

  public static final hydra.core.Name FOCUSED = new hydra.core.Name("focused");

  public static final hydra.core.Name AMBIENT = new hydra.core.Name("ambient");

  private LinearQueryStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Focused instance) ;

    R visit(Ambient instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LinearQueryStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Focused instance) {
      return otherwise(instance);
    }

    default R visit(Ambient instance) {
      return otherwise(instance);
    }
  }

  public static final class Focused extends openGql.grammar.LinearQueryStatement implements Serializable {
    public final openGql.grammar.FocusedLinearQueryStatement value;

    public Focused (openGql.grammar.FocusedLinearQueryStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Focused)) {
        return false;
      }
      Focused o = (Focused) other;
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
    public int compareTo(LinearQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Focused o = (Focused) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Ambient extends openGql.grammar.LinearQueryStatement implements Serializable {
    public final openGql.grammar.AmbientLinearQueryStatement value;

    public Ambient (openGql.grammar.AmbientLinearQueryStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ambient)) {
        return false;
      }
      Ambient o = (Ambient) other;
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
    public int compareTo(LinearQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Ambient o = (Ambient) other;
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
