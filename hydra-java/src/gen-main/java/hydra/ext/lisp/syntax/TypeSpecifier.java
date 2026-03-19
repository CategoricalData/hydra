// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A type specifier
 */
public abstract class TypeSpecifier implements Serializable, Comparable<TypeSpecifier> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.TypeSpecifier");

  public static final hydra.core.Name NAMED = new hydra.core.Name("named");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name MAYBE = new hydra.core.Name("maybe");

  public static final hydra.core.Name MAP = new hydra.core.Name("map");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name PAIR = new hydra.core.Name("pair");

  public static final hydra.core.Name EITHER = new hydra.core.Name("either");

  public static final hydra.core.Name UNIT = new hydra.core.Name("unit");

  private TypeSpecifier () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Named instance) ;

    R visit(List instance) ;

    R visit(Function instance) ;

    R visit(Maybe instance) ;

    R visit(Map instance) ;

    R visit(Set instance) ;

    R visit(Pair instance) ;

    R visit(Either instance) ;

    R visit(Unit instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeSpecifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Named instance) {
      return otherwise(instance);
    }

    default R visit(List instance) {
      return otherwise(instance);
    }

    default R visit(Function instance) {
      return otherwise(instance);
    }

    default R visit(Maybe instance) {
      return otherwise(instance);
    }

    default R visit(Map instance) {
      return otherwise(instance);
    }

    default R visit(Set instance) {
      return otherwise(instance);
    }

    default R visit(Pair instance) {
      return otherwise(instance);
    }

    default R visit(Either instance) {
      return otherwise(instance);
    }

    default R visit(Unit instance) {
      return otherwise(instance);
    }
  }

  /**
   * A named type reference
   */
  public static final class Named extends hydra.ext.lisp.syntax.TypeSpecifier implements Serializable {
    public final hydra.ext.lisp.syntax.Symbol value;

    public Named (hydra.ext.lisp.syntax.Symbol value) {
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
    public int compareTo(TypeSpecifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Named o = (Named) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A list type
   */
  public static final class List extends hydra.ext.lisp.syntax.TypeSpecifier implements Serializable {
    public final hydra.ext.lisp.syntax.TypeSpecifier value;

    public List (hydra.ext.lisp.syntax.TypeSpecifier value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) other;
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
    public int compareTo(TypeSpecifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      List o = (List) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A function type (params and return)
   */
  public static final class Function extends hydra.ext.lisp.syntax.TypeSpecifier implements Serializable {
    public final hydra.util.ConsList<hydra.ext.lisp.syntax.TypeSpecifier> value;

    public Function (hydra.util.ConsList<hydra.ext.lisp.syntax.TypeSpecifier> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) other;
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
    public int compareTo(TypeSpecifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Function o = (Function) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * An optional type
   */
  public static final class Maybe extends hydra.ext.lisp.syntax.TypeSpecifier implements Serializable {
    public final hydra.ext.lisp.syntax.TypeSpecifier value;

    public Maybe (hydra.ext.lisp.syntax.TypeSpecifier value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Maybe)) {
        return false;
      }
      Maybe o = (Maybe) other;
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
    public int compareTo(TypeSpecifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Maybe o = (Maybe) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A map type (key and value type specifiers)
   */
  public static final class Map extends hydra.ext.lisp.syntax.TypeSpecifier implements Serializable {
    public final hydra.util.ConsList<hydra.ext.lisp.syntax.TypeSpecifier> value;

    public Map (hydra.util.ConsList<hydra.ext.lisp.syntax.TypeSpecifier> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) other;
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
    public int compareTo(TypeSpecifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Map o = (Map) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A set type
   */
  public static final class Set extends hydra.ext.lisp.syntax.TypeSpecifier implements Serializable {
    public final hydra.ext.lisp.syntax.TypeSpecifier value;

    public Set (hydra.ext.lisp.syntax.TypeSpecifier value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) other;
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
    public int compareTo(TypeSpecifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Set o = (Set) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A pair/tuple type (two type specifiers)
   */
  public static final class Pair extends hydra.ext.lisp.syntax.TypeSpecifier implements Serializable {
    public final hydra.util.ConsList<hydra.ext.lisp.syntax.TypeSpecifier> value;

    public Pair (hydra.util.ConsList<hydra.ext.lisp.syntax.TypeSpecifier> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pair)) {
        return false;
      }
      Pair o = (Pair) other;
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
    public int compareTo(TypeSpecifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pair o = (Pair) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * An either/union type (two type specifiers)
   */
  public static final class Either extends hydra.ext.lisp.syntax.TypeSpecifier implements Serializable {
    public final hydra.util.ConsList<hydra.ext.lisp.syntax.TypeSpecifier> value;

    public Either (hydra.util.ConsList<hydra.ext.lisp.syntax.TypeSpecifier> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Either)) {
        return false;
      }
      Either o = (Either) other;
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
    public int compareTo(TypeSpecifier other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Either o = (Either) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * The unit type
   */
  public static final class Unit extends hydra.ext.lisp.syntax.TypeSpecifier implements Serializable {
    public Unit () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unit)) {
        return false;
      }
      Unit o = (Unit) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeSpecifier other) {
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
