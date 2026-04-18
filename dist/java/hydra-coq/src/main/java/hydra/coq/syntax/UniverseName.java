// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class UniverseName implements Serializable, Comparable<UniverseName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.UniverseName");

  public static final hydra.core.Name QUALID = new hydra.core.Name("qualid");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name PROP = new hydra.core.Name("prop");

  private UniverseName () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Qualid instance) ;

    R visit(Set instance) ;

    R visit(Prop instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UniverseName instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Qualid instance) {
      return otherwise(instance);
    }

    default R visit(Set instance) {
      return otherwise(instance);
    }

    default R visit(Prop instance) {
      return otherwise(instance);
    }
  }

  public static final class Qualid extends hydra.coq.syntax.UniverseName implements Serializable {
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
    public int compareTo(UniverseName other) {
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

  public static final class Set extends hydra.coq.syntax.UniverseName implements Serializable {
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
    public int compareTo(UniverseName other) {
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

  public static final class Prop extends hydra.coq.syntax.UniverseName implements Serializable {
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
    public int compareTo(UniverseName other) {
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
}
