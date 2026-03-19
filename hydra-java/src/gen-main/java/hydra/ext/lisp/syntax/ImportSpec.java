// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * An import specification describing how to import symbols
 */
public abstract class ImportSpec implements Serializable, Comparable<ImportSpec> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.ImportSpec");

  public static final hydra.core.Name ALL = new hydra.core.Name("all");

  public static final hydra.core.Name ALIAS = new hydra.core.Name("alias");

  public static final hydra.core.Name ONLY = new hydra.core.Name("only");

  public static final hydra.core.Name RENAME = new hydra.core.Name("rename");

  private ImportSpec () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(All instance) ;

    R visit(Alias instance) ;

    R visit(Only instance) ;

    R visit(Rename instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImportSpec instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(All instance) {
      return otherwise(instance);
    }

    default R visit(Alias instance) {
      return otherwise(instance);
    }

    default R visit(Only instance) {
      return otherwise(instance);
    }

    default R visit(Rename instance) {
      return otherwise(instance);
    }
  }

  /**
   * Import everything
   */
  public static final class All extends hydra.ext.lisp.syntax.ImportSpec implements Serializable {
    public All () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ImportSpec other) {
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

  /**
   * Import with an alias: (:require [name :as alias]) in Clojure
   */
  public static final class Alias extends hydra.ext.lisp.syntax.ImportSpec implements Serializable {
    public final hydra.ext.lisp.syntax.Symbol value;

    public Alias (hydra.ext.lisp.syntax.Symbol value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Alias)) {
        return false;
      }
      Alias o = (Alias) other;
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
    public int compareTo(ImportSpec other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Alias o = (Alias) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Import specific symbols: (:require [name :refer [sym1 sym2]]) in Clojure
   */
  public static final class Only extends hydra.ext.lisp.syntax.ImportSpec implements Serializable {
    public final hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> value;

    public Only (hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Only)) {
        return false;
      }
      Only o = (Only) other;
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
    public int compareTo(ImportSpec other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Only o = (Only) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Import with renaming: list of (from, to) symbol pairs
   */
  public static final class Rename extends hydra.ext.lisp.syntax.ImportSpec implements Serializable {
    public final hydra.util.ConsList<hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol>> value;

    public Rename (hydra.util.ConsList<hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol>> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rename)) {
        return false;
      }
      Rename o = (Rename) other;
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
    public int compareTo(ImportSpec other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Rename o = (Rename) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
