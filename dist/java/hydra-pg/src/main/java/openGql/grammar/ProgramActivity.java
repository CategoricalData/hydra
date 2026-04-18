// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ProgramActivity implements Serializable, Comparable<ProgramActivity> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ProgramActivity");

  public static final hydra.core.Name SESSION = new hydra.core.Name("session");

  public static final hydra.core.Name TRANSACTION = new hydra.core.Name("transaction");

  private ProgramActivity () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Session instance) ;

    R visit(Transaction instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ProgramActivity instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Session instance) {
      return otherwise(instance);
    }

    default R visit(Transaction instance) {
      return otherwise(instance);
    }
  }

  public static final class Session extends openGql.grammar.ProgramActivity implements Serializable {
    public final openGql.grammar.SessionActivity value;

    public Session (openGql.grammar.SessionActivity value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Session)) {
        return false;
      }
      Session o = (Session) other;
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
    public int compareTo(ProgramActivity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Session o = (Session) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Transaction extends openGql.grammar.ProgramActivity implements Serializable {
    public final openGql.grammar.TransactionActivity value;

    public Transaction (openGql.grammar.TransactionActivity value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Transaction)) {
        return false;
      }
      Transaction o = (Transaction) other;
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
    public int compareTo(ProgramActivity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Transaction o = (Transaction) other;
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
