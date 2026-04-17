// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TransactionAccessMode implements Serializable, Comparable<TransactionAccessMode> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TransactionAccessMode");

  public static final hydra.core.Name READ_ONLY = new hydra.core.Name("readOnly");

  public static final hydra.core.Name READ_WRITE = new hydra.core.Name("readWrite");

  private TransactionAccessMode () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ReadOnly instance) ;

    R visit(ReadWrite instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TransactionAccessMode instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ReadOnly instance) {
      return otherwise(instance);
    }

    default R visit(ReadWrite instance) {
      return otherwise(instance);
    }
  }

  public static final class ReadOnly extends openGql.grammar.TransactionAccessMode implements Serializable {
    public ReadOnly () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ReadOnly)) {
        return false;
      }
      ReadOnly o = (ReadOnly) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TransactionAccessMode other) {
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

  public static final class ReadWrite extends openGql.grammar.TransactionAccessMode implements Serializable {
    public ReadWrite () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ReadWrite)) {
        return false;
      }
      ReadWrite o = (ReadWrite) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TransactionAccessMode other) {
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
