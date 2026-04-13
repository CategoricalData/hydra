// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class DelTarget implements Serializable, Comparable<DelTarget> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.DelTarget");

  public static final hydra.core.Name PRIMARY_AND_NAME = new hydra.core.Name("primaryAndName");

  public static final hydra.core.Name PRIMARY_AND_SLICES = new hydra.core.Name("primaryAndSlices");

  public static final hydra.core.Name DEL_T_ATOM = new hydra.core.Name("delTAtom");

  private DelTarget () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PrimaryAndName instance) ;

    R visit(PrimaryAndSlices instance) ;

    R visit(DelTAtom instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DelTarget instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PrimaryAndName instance) {
      return otherwise(instance);
    }

    default R visit(PrimaryAndSlices instance) {
      return otherwise(instance);
    }

    default R visit(DelTAtom instance) {
      return otherwise(instance);
    }
  }

  public static final class PrimaryAndName extends hydra.python.syntax.DelTarget implements Serializable {
    public final hydra.python.syntax.TPrimaryAndName value;

    public PrimaryAndName (hydra.python.syntax.TPrimaryAndName value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimaryAndName)) {
        return false;
      }
      PrimaryAndName o = (PrimaryAndName) other;
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
    public int compareTo(DelTarget other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PrimaryAndName o = (PrimaryAndName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PrimaryAndSlices extends hydra.python.syntax.DelTarget implements Serializable {
    public final hydra.python.syntax.TPrimaryAndSlices value;

    public PrimaryAndSlices (hydra.python.syntax.TPrimaryAndSlices value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimaryAndSlices)) {
        return false;
      }
      PrimaryAndSlices o = (PrimaryAndSlices) other;
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
    public int compareTo(DelTarget other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PrimaryAndSlices o = (PrimaryAndSlices) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DelTAtom extends hydra.python.syntax.DelTarget implements Serializable {
    public final hydra.python.syntax.DelTAtom value;

    public DelTAtom (hydra.python.syntax.DelTAtom value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DelTAtom)) {
        return false;
      }
      DelTAtom o = (DelTAtom) other;
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
    public int compareTo(DelTarget other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DelTAtom o = (DelTAtom) other;
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
