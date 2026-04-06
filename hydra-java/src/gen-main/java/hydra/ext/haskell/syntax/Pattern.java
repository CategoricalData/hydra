// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * A pattern
 */
public abstract class Pattern implements Serializable, Comparable<Pattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.Pattern");

  public static final hydra.core.Name APPLICATION = new hydra.core.Name("application");

  public static final hydra.core.Name AS = new hydra.core.Name("as");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name PARENS = new hydra.core.Name("parens");

  public static final hydra.core.Name RECORD = new hydra.core.Name("record");

  public static final hydra.core.Name TUPLE = new hydra.core.Name("tuple");

  public static final hydra.core.Name TYPED = new hydra.core.Name("typed");

  public static final hydra.core.Name WILDCARD = new hydra.core.Name("wildcard");

  private Pattern () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Application instance) ;

    R visit(As instance) ;

    R visit(List instance) ;

    R visit(Literal instance) ;

    R visit(Name instance) ;

    R visit(Parens instance) ;

    R visit(Record instance) ;

    R visit(Tuple instance) ;

    R visit(Typed instance) ;

    R visit(Wildcard instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Application instance) {
      return otherwise(instance);
    }

    default R visit(As instance) {
      return otherwise(instance);
    }

    default R visit(List instance) {
      return otherwise(instance);
    }

    default R visit(Literal instance) {
      return otherwise(instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }

    default R visit(Parens instance) {
      return otherwise(instance);
    }

    default R visit(Record instance) {
      return otherwise(instance);
    }

    default R visit(Tuple instance) {
      return otherwise(instance);
    }

    default R visit(Typed instance) {
      return otherwise(instance);
    }

    default R visit(Wildcard instance) {
      return otherwise(instance);
    }
  }

  /**
   * An application pattern
   */
  public static final class Application extends hydra.ext.haskell.syntax.Pattern implements Serializable {
    public final hydra.ext.haskell.syntax.ApplicationPattern value;

    public Application (hydra.ext.haskell.syntax.ApplicationPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Application o = (Application) other;
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
   * An 'as' pattern
   */
  public static final class As extends hydra.ext.haskell.syntax.Pattern implements Serializable {
    public final hydra.ext.haskell.syntax.AsPattern value;

    public As (hydra.ext.haskell.syntax.AsPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof As)) {
        return false;
      }
      As o = (As) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      As o = (As) other;
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
   * A list pattern
   */
  public static final class List extends hydra.ext.haskell.syntax.Pattern implements Serializable {
    public final java.util.List<hydra.ext.haskell.syntax.Pattern> value;

    public List (java.util.List<hydra.ext.haskell.syntax.Pattern> value) {
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      List o = (List) other;
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
   * A literal pattern
   */
  public static final class Literal extends hydra.ext.haskell.syntax.Pattern implements Serializable {
    public final hydra.ext.haskell.syntax.Literal value;

    public Literal (hydra.ext.haskell.syntax.Literal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Literal o = (Literal) other;
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
   * A name pattern
   */
  public static final class Name extends hydra.ext.haskell.syntax.Pattern implements Serializable {
    public final hydra.ext.haskell.syntax.Name value;

    public Name (hydra.ext.haskell.syntax.Name value) {
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
    public int compareTo(Pattern other) {
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

  /**
   * A parenthesized pattern
   */
  public static final class Parens extends hydra.ext.haskell.syntax.Pattern implements Serializable {
    public final hydra.ext.haskell.syntax.Pattern value;

    public Parens (hydra.ext.haskell.syntax.Pattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parens o = (Parens) other;
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
   * A record pattern
   */
  public static final class Record extends hydra.ext.haskell.syntax.Pattern implements Serializable {
    public final hydra.ext.haskell.syntax.RecordPattern value;

    public Record (hydra.ext.haskell.syntax.RecordPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Record o = (Record) other;
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
   * A tuple pattern
   */
  public static final class Tuple extends hydra.ext.haskell.syntax.Pattern implements Serializable {
    public final java.util.List<hydra.ext.haskell.syntax.Pattern> value;

    public Tuple (java.util.List<hydra.ext.haskell.syntax.Pattern> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Tuple o = (Tuple) other;
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
   * A typed pattern
   */
  public static final class Typed extends hydra.ext.haskell.syntax.Pattern implements Serializable {
    public final hydra.ext.haskell.syntax.TypedPattern value;

    public Typed (hydra.ext.haskell.syntax.TypedPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Typed)) {
        return false;
      }
      Typed o = (Typed) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Typed o = (Typed) other;
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
   * A wildcard pattern
   */
  public static final class Wildcard extends hydra.ext.haskell.syntax.Pattern implements Serializable {
    public Wildcard () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wildcard)) {
        return false;
      }
      Wildcard o = (Wildcard) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Pattern other) {
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
