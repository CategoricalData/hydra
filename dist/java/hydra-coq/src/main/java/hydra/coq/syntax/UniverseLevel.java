// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class UniverseLevel implements Serializable, Comparable<UniverseLevel> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.UniverseLevel");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name PROP = new hydra.core.Name("prop");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name IGNORED = new hydra.core.Name("ignored");

  public static final hydra.core.Name QUALID = new hydra.core.Name("qualid");

  private UniverseLevel () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Set instance) ;

    R visit(Prop instance) ;

    R visit(Type instance) ;

    R visit(Ignored instance) ;

    R visit(Qualid instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UniverseLevel instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Set instance) {
      return otherwise(instance);
    }

    default R visit(Prop instance) {
      return otherwise(instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }

    default R visit(Ignored instance) {
      return otherwise(instance);
    }

    default R visit(Qualid instance) {
      return otherwise(instance);
    }
  }

  public static final class Set extends hydra.coq.syntax.UniverseLevel implements Serializable {
    public Set () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UniverseLevel other) {
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

  public static final class Prop extends hydra.coq.syntax.UniverseLevel implements Serializable {
    public Prop () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Prop)) {
        return false;
      }
      Prop o = (Prop) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UniverseLevel other) {
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

  public static final class Type extends hydra.coq.syntax.UniverseLevel implements Serializable {
    public Type () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UniverseLevel other) {
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

  public static final class Ignored extends hydra.coq.syntax.UniverseLevel implements Serializable {
    public Ignored () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ignored)) {
        return false;
      }
      Ignored o = (Ignored) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UniverseLevel other) {
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

  public static final class Qualid extends hydra.coq.syntax.UniverseLevel implements Serializable {
    public final hydra.coq.syntax.Qualid value;

    public Qualid (hydra.coq.syntax.Qualid value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qualid)) {
        return false;
      }
      Qualid o = (Qualid) other;
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
    public int compareTo(UniverseLevel other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Qualid o = (Qualid) other;
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
