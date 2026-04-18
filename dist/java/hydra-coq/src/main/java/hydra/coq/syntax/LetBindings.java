// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class LetBindings implements Serializable, Comparable<LetBindings> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.LetBindings");

  public static final hydra.core.Name NAMED = new hydra.core.Name("named");

  public static final hydra.core.Name DESTRUCTURING = new hydra.core.Name("destructuring");

  private LetBindings () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Named instance) ;

    R visit(Destructuring instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LetBindings instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Named instance) {
      return otherwise(instance);
    }

    default R visit(Destructuring instance) {
      return otherwise(instance);
    }
  }

  public static final class Named extends hydra.coq.syntax.LetBindings implements Serializable {
    public final hydra.coq.syntax.LetNamed value;

    public Named (hydra.coq.syntax.LetNamed value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Named)) {
        return false;
      }
      Named o = (Named) other;
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
    public int compareTo(LetBindings other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Named o = (Named) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Destructuring extends hydra.coq.syntax.LetBindings implements Serializable {
    public final hydra.coq.syntax.LetDestructuring value;

    public Destructuring (hydra.coq.syntax.LetDestructuring value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Destructuring)) {
        return false;
      }
      Destructuring o = (Destructuring) other;
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
    public int compareTo(LetBindings other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Destructuring o = (Destructuring) other;
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
