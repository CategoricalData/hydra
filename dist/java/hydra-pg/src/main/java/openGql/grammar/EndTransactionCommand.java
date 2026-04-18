// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EndTransactionCommand implements Serializable, Comparable<EndTransactionCommand> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EndTransactionCommand");

  public static final hydra.core.Name ROLLBACK = new hydra.core.Name("rollback");

  public static final hydra.core.Name COMMIT = new hydra.core.Name("commit");

  private EndTransactionCommand () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Rollback instance) ;

    R visit(Commit instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EndTransactionCommand instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Rollback instance) {
      return otherwise(instance);
    }

    default R visit(Commit instance) {
      return otherwise(instance);
    }
  }

  public static final class Rollback extends openGql.grammar.EndTransactionCommand implements Serializable {
    public final java.lang.Void value;

    public Rollback (java.lang.Void value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rollback)) {
        return false;
      }
      Rollback o = (Rollback) other;
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
    public int compareTo(EndTransactionCommand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Rollback o = (Rollback) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Commit extends openGql.grammar.EndTransactionCommand implements Serializable {
    public final java.lang.Void value;

    public Commit (java.lang.Void value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Commit)) {
        return false;
      }
      Commit o = (Commit) other;
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
    public int compareTo(EndTransactionCommand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Commit o = (Commit) other;
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
