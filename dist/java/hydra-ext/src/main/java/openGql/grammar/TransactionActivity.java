// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TransactionActivity implements Serializable, Comparable<TransactionActivity> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TransactionActivity");

  public static final hydra.core.Name START = new hydra.core.Name("start");

  public static final hydra.core.Name PROCEDURE = new hydra.core.Name("procedure");

  public static final hydra.core.Name END = new hydra.core.Name("end");

  private TransactionActivity () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Start instance) ;

    R visit(Procedure instance) ;

    R visit(End instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TransactionActivity instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Start instance) {
      return otherwise(instance);
    }

    default R visit(Procedure instance) {
      return otherwise(instance);
    }

    default R visit(End instance) {
      return otherwise(instance);
    }
  }

  public static final class Start extends openGql.grammar.TransactionActivity implements Serializable {
    public final openGql.grammar.StartAndMaybeProcedureAndMaybeEnd value;

    public Start (openGql.grammar.StartAndMaybeProcedureAndMaybeEnd value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Start)) {
        return false;
      }
      Start o = (Start) other;
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
    public int compareTo(TransactionActivity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Start o = (Start) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Procedure extends openGql.grammar.TransactionActivity implements Serializable {
    public final openGql.grammar.ProcedureAndMaybeEnd value;

    public Procedure (openGql.grammar.ProcedureAndMaybeEnd value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Procedure)) {
        return false;
      }
      Procedure o = (Procedure) other;
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
    public int compareTo(TransactionActivity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Procedure o = (Procedure) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class End extends openGql.grammar.TransactionActivity implements Serializable {
    public final openGql.grammar.EndTransactionCommand value;

    public End (openGql.grammar.EndTransactionCommand value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof End)) {
        return false;
      }
      End o = (End) other;
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
    public int compareTo(TransactionActivity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      End o = (End) other;
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
