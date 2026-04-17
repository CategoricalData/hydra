// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PrimitiveDataModifyingStatement implements Serializable, Comparable<PrimitiveDataModifyingStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PrimitiveDataModifyingStatement");

  public static final hydra.core.Name INSERT = new hydra.core.Name("insert");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name REMOVE = new hydra.core.Name("remove");

  public static final hydra.core.Name DELETE = new hydra.core.Name("delete");

  private PrimitiveDataModifyingStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Insert instance) ;

    R visit(Set instance) ;

    R visit(Remove instance) ;

    R visit(Delete instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimitiveDataModifyingStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Insert instance) {
      return otherwise(instance);
    }

    default R visit(Set instance) {
      return otherwise(instance);
    }

    default R visit(Remove instance) {
      return otherwise(instance);
    }

    default R visit(Delete instance) {
      return otherwise(instance);
    }
  }

  public static final class Insert extends openGql.grammar.PrimitiveDataModifyingStatement implements Serializable {
    public final java.util.List<openGql.grammar.InsertPathPattern> value;

    public Insert (java.util.List<openGql.grammar.InsertPathPattern> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Insert)) {
        return false;
      }
      Insert o = (Insert) other;
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
    public int compareTo(PrimitiveDataModifyingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Insert o = (Insert) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Set extends openGql.grammar.PrimitiveDataModifyingStatement implements Serializable {
    public final java.util.List<openGql.grammar.SetItem> value;

    public Set (java.util.List<openGql.grammar.SetItem> value) {
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
    public int compareTo(PrimitiveDataModifyingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Set o = (Set) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Remove extends openGql.grammar.PrimitiveDataModifyingStatement implements Serializable {
    public final java.util.List<openGql.grammar.RemoveItem> value;

    public Remove (java.util.List<openGql.grammar.RemoveItem> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Remove)) {
        return false;
      }
      Remove o = (Remove) other;
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
    public int compareTo(PrimitiveDataModifyingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Remove o = (Remove) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Delete extends openGql.grammar.PrimitiveDataModifyingStatement implements Serializable {
    public final openGql.grammar.DeleteStatement value;

    public Delete (openGql.grammar.DeleteStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Delete)) {
        return false;
      }
      Delete o = (Delete) other;
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
    public int compareTo(PrimitiveDataModifyingStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Delete o = (Delete) other;
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
