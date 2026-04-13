// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class Cardinality implements Serializable, Comparable<Cardinality> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.Cardinality");

  public static final hydra.core.Name AST = new hydra.core.Name("Ast");

  public static final hydra.core.Name PLUS = new hydra.core.Name("Plus");

  public static final hydra.core.Name QUEST = new hydra.core.Name("Quest");

  public static final hydra.core.Name REPEAT_RANGE = new hydra.core.Name("RepeatRange");

  private Cardinality () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Ast instance) ;

    R visit(Plus instance) ;

    R visit(Quest instance) ;

    R visit(RepeatRange instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Cardinality instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Ast instance) {
      return otherwise(instance);
    }

    default R visit(Plus instance) {
      return otherwise(instance);
    }

    default R visit(Quest instance) {
      return otherwise(instance);
    }

    default R visit(RepeatRange instance) {
      return otherwise(instance);
    }
  }

  public static final class Ast extends hydra.shex.syntax.Cardinality implements Serializable {
    public Ast () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ast)) {
        return false;
      }
      Ast o = (Ast) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Cardinality other) {
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

  public static final class Plus extends hydra.shex.syntax.Cardinality implements Serializable {
    public Plus () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plus)) {
        return false;
      }
      Plus o = (Plus) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Cardinality other) {
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

  public static final class Quest extends hydra.shex.syntax.Cardinality implements Serializable {
    public Quest () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Quest)) {
        return false;
      }
      Quest o = (Quest) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Cardinality other) {
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

  public static final class RepeatRange extends hydra.shex.syntax.Cardinality implements Serializable {
    public final hydra.shex.syntax.RepeatRange value;

    public RepeatRange (hydra.shex.syntax.RepeatRange value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RepeatRange)) {
        return false;
      }
      RepeatRange o = (RepeatRange) other;
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
    public int compareTo(Cardinality other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RepeatRange o = (RepeatRange) other;
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
