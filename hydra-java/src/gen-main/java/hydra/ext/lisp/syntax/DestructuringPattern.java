// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A destructuring pattern
 */
public abstract class DestructuringPattern implements Serializable, Comparable<DestructuringPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.DestructuringPattern");

  public static final hydra.core.Name SEQUENTIAL = new hydra.core.Name("sequential");

  public static final hydra.core.Name ASSOCIATIVE = new hydra.core.Name("associative");

  public static final hydra.core.Name REST = new hydra.core.Name("rest");

  private DestructuringPattern () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Sequential instance) ;

    R visit(Associative instance) ;

    R visit(Rest instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DestructuringPattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Sequential instance) {
      return otherwise(instance);
    }

    default R visit(Associative instance) {
      return otherwise(instance);
    }

    default R visit(Rest instance) {
      return otherwise(instance);
    }
  }

  /**
   * Sequential destructuring: [a b c] in Clojure, (a b c) in others
   */
  public static final class Sequential extends hydra.ext.lisp.syntax.DestructuringPattern implements Serializable {
    public final hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> value;

    public Sequential (hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequential)) {
        return false;
      }
      Sequential o = (Sequential) other;
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
    public int compareTo(DestructuringPattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequential o = (Sequential) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Associative/map destructuring: {:keys [a b]} in Clojure
   */
  public static final class Associative extends hydra.ext.lisp.syntax.DestructuringPattern implements Serializable {
    public final hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> value;

    public Associative (hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Associative)) {
        return false;
      }
      Associative o = (Associative) other;
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
    public int compareTo(DestructuringPattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Associative o = (Associative) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Destructuring with a rest element: [a b & rest] (leading symbols + rest symbol)
   */
  public static final class Rest extends hydra.ext.lisp.syntax.DestructuringPattern implements Serializable {
    public final hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> value;

    public Rest (hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rest)) {
        return false;
      }
      Rest o = (Rest) other;
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
    public int compareTo(DestructuringPattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Rest o = (Rest) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
