// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A local binding
 */
public abstract class LocalBinding implements Serializable, Comparable<LocalBinding> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.LocalBinding");

  public static final hydra.core.Name SIGNATURE = new hydra.core.Name("signature");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  private LocalBinding () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Signature instance) ;

    R visit(Value instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LocalBinding instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Signature instance) {
      return otherwise(instance);
    }

    default R visit(Value instance) {
      return otherwise(instance);
    }
  }

  /**
   * A type signature
   */
  public static final class Signature extends hydra.haskell.syntax.LocalBinding implements Serializable {
    public final hydra.haskell.syntax.TypeSignature value;

    public Signature (hydra.haskell.syntax.TypeSignature value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Signature)) {
        return false;
      }
      Signature o = (Signature) other;
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
    public int compareTo(LocalBinding other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Signature o = (Signature) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A value binding
   */
  public static final class Value extends hydra.haskell.syntax.LocalBinding implements Serializable {
    public final hydra.haskell.syntax.ValueBinding value;

    public Value (hydra.haskell.syntax.ValueBinding value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) other;
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
    public int compareTo(LocalBinding other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Value o = (Value) other;
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
