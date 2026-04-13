// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public abstract class DelTAtom implements Serializable, Comparable<DelTAtom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.DelTAtom");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  public static final hydra.core.Name TARGETS = new hydra.core.Name("targets");

  private DelTAtom () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Name instance) ;

    R visit(Target instance) ;

    R visit(Targets instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DelTAtom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }

    default R visit(Target instance) {
      return otherwise(instance);
    }

    default R visit(Targets instance) {
      return otherwise(instance);
    }
  }

  public static final class Name extends hydra.python.syntax.DelTAtom implements Serializable {
    public final hydra.python.syntax.Name value;

    public Name (hydra.python.syntax.Name value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) other;
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
    public int compareTo(DelTAtom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Name o = (Name) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Target extends hydra.python.syntax.DelTAtom implements Serializable {
    public final hydra.python.syntax.DelTarget value;

    public Target (hydra.python.syntax.DelTarget value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Target)) {
        return false;
      }
      Target o = (Target) other;
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
    public int compareTo(DelTAtom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Target o = (Target) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Targets extends hydra.python.syntax.DelTAtom implements Serializable {
    public final hydra.python.syntax.DelTargets value;

    public Targets (hydra.python.syntax.DelTargets value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Targets)) {
        return false;
      }
      Targets o = (Targets) other;
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
    public int compareTo(DelTAtom other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Targets o = (Targets) other;
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
