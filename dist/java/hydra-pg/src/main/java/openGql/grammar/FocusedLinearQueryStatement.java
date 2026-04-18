// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class FocusedLinearQueryStatement implements Serializable, Comparable<FocusedLinearQueryStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.FocusedLinearQueryStatement");

  public static final hydra.core.Name PARTS = new hydra.core.Name("parts");

  public static final hydra.core.Name PRIMITIVE = new hydra.core.Name("primitive");

  public static final hydra.core.Name NESTED = new hydra.core.Name("nested");

  public static final hydra.core.Name SELECT = new hydra.core.Name("select");

  private FocusedLinearQueryStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Parts instance) ;

    R visit(Primitive instance) ;

    R visit(Nested instance) ;

    R visit(Select instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FocusedLinearQueryStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Parts instance) {
      return otherwise(instance);
    }

    default R visit(Primitive instance) {
      return otherwise(instance);
    }

    default R visit(Nested instance) {
      return otherwise(instance);
    }

    default R visit(Select instance) {
      return otherwise(instance);
    }
  }

  public static final class Parts extends openGql.grammar.FocusedLinearQueryStatement implements Serializable {
    public final openGql.grammar.FocusedLinearQueryStatementPartsAndResult value;

    public Parts (openGql.grammar.FocusedLinearQueryStatementPartsAndResult value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parts)) {
        return false;
      }
      Parts o = (Parts) other;
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
    public int compareTo(FocusedLinearQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parts o = (Parts) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Primitive extends openGql.grammar.FocusedLinearQueryStatement implements Serializable {
    public final openGql.grammar.FocusedPrimitiveResultStatement value;

    public Primitive (openGql.grammar.FocusedPrimitiveResultStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) other;
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
    public int compareTo(FocusedLinearQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Primitive o = (Primitive) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Nested extends openGql.grammar.FocusedLinearQueryStatement implements Serializable {
    public final openGql.grammar.FocusedNestedQuerySpecification value;

    public Nested (openGql.grammar.FocusedNestedQuerySpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nested)) {
        return false;
      }
      Nested o = (Nested) other;
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
    public int compareTo(FocusedLinearQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Nested o = (Nested) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Select extends openGql.grammar.FocusedLinearQueryStatement implements Serializable {
    public final openGql.grammar.SelectStatement value;

    public Select (openGql.grammar.SelectStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Select)) {
        return false;
      }
      Select o = (Select) other;
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
    public int compareTo(FocusedLinearQueryStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Select o = (Select) other;
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
