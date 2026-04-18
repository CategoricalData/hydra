// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class OpenBinders implements Serializable, Comparable<OpenBinders> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.OpenBinders");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name BINDERS = new hydra.core.Name("binders");

  private OpenBinders () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Type instance) ;

    R visit(Binders instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OpenBinders instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }

    default R visit(Binders instance) {
      return otherwise(instance);
    }
  }

  public static final class Type extends hydra.coq.syntax.OpenBinders implements Serializable {
    public final hydra.coq.syntax.TypeBinders value;

    public Type (hydra.coq.syntax.TypeBinders value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) other;
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
    public int compareTo(OpenBinders other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Type o = (Type) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Binders extends hydra.coq.syntax.OpenBinders implements Serializable {
    public final java.util.List<hydra.coq.syntax.Binder> value;

    public Binders (java.util.List<hydra.coq.syntax.Binder> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binders)) {
        return false;
      }
      Binders o = (Binders) other;
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
    public int compareTo(OpenBinders other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Binders o = (Binders) other;
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
