// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ReturnStatementBody implements Serializable, Comparable<ReturnStatementBody> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ReturnStatementBody");

  public static final hydra.core.Name ITEMS = new hydra.core.Name("items");

  public static final hydra.core.Name NO_BINDINGS = new hydra.core.Name("noBindings");

  private ReturnStatementBody () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Items instance) ;

    R visit(NoBindings instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ReturnStatementBody instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Items instance) {
      return otherwise(instance);
    }

    default R visit(NoBindings instance) {
      return otherwise(instance);
    }
  }

  public static final class Items extends openGql.grammar.ReturnStatementBody implements Serializable {
    public final openGql.grammar.ReturnItemsAndGroupBy value;

    public Items (openGql.grammar.ReturnItemsAndGroupBy value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Items)) {
        return false;
      }
      Items o = (Items) other;
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
    public int compareTo(ReturnStatementBody other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Items o = (Items) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NoBindings extends openGql.grammar.ReturnStatementBody implements Serializable {
    public NoBindings () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NoBindings)) {
        return false;
      }
      NoBindings o = (NoBindings) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ReturnStatementBody other) {
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
