// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class StatementWithoutTrailingSubstatement implements Serializable, Comparable<StatementWithoutTrailingSubstatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.StatementWithoutTrailingSubstatement");

  public static final hydra.core.Name BLOCK = new hydra.core.Name("block");

  public static final hydra.core.Name EMPTY = new hydra.core.Name("empty");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name ASSERT = new hydra.core.Name("assert");

  public static final hydra.core.Name SWITCH = new hydra.core.Name("switch");

  public static final hydra.core.Name DO = new hydra.core.Name("do");

  public static final hydra.core.Name BREAK = new hydra.core.Name("break");

  public static final hydra.core.Name CONTINUE = new hydra.core.Name("continue");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public static final hydra.core.Name SYNCHRONIZED = new hydra.core.Name("synchronized");

  public static final hydra.core.Name THROW = new hydra.core.Name("throw");

  public static final hydra.core.Name TRY = new hydra.core.Name("try");

  private StatementWithoutTrailingSubstatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Block instance) ;

    R visit(Empty instance) ;

    R visit(Expression instance) ;

    R visit(Assert instance) ;

    R visit(Switch instance) ;

    R visit(Do instance) ;

    R visit(Break instance) ;

    R visit(Continue instance) ;

    R visit(Return instance) ;

    R visit(Synchronized instance) ;

    R visit(Throw instance) ;

    R visit(Try instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StatementWithoutTrailingSubstatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Block instance) {
      return otherwise(instance);
    }

    default R visit(Empty instance) {
      return otherwise(instance);
    }

    default R visit(Expression instance) {
      return otherwise(instance);
    }

    default R visit(Assert instance) {
      return otherwise(instance);
    }

    default R visit(Switch instance) {
      return otherwise(instance);
    }

    default R visit(Do instance) {
      return otherwise(instance);
    }

    default R visit(Break instance) {
      return otherwise(instance);
    }

    default R visit(Continue instance) {
      return otherwise(instance);
    }

    default R visit(Return instance) {
      return otherwise(instance);
    }

    default R visit(Synchronized instance) {
      return otherwise(instance);
    }

    default R visit(Throw instance) {
      return otherwise(instance);
    }

    default R visit(Try instance) {
      return otherwise(instance);
    }
  }

  public static final class Block extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.Block value;

    public Block (hydra.java.syntax.Block value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Block)) {
        return false;
      }
      Block o = (Block) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Block o = (Block) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Empty extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public Empty () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Empty)) {
        return false;
      }
      Empty o = (Empty) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(StatementWithoutTrailingSubstatement other) {
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

  public static final class Expression extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.ExpressionStatement value;

    public Expression (hydra.java.syntax.ExpressionStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Expression)) {
        return false;
      }
      Expression o = (Expression) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Expression o = (Expression) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Assert extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.AssertStatement value;

    public Assert (hydra.java.syntax.AssertStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assert)) {
        return false;
      }
      Assert o = (Assert) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Assert o = (Assert) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Switch extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.SwitchStatement value;

    public Switch (hydra.java.syntax.SwitchStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Switch)) {
        return false;
      }
      Switch o = (Switch) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Switch o = (Switch) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Do extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.DoStatement value;

    public Do (hydra.java.syntax.DoStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Do)) {
        return false;
      }
      Do o = (Do) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Do o = (Do) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Break extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.BreakStatement value;

    public Break (hydra.java.syntax.BreakStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Break)) {
        return false;
      }
      Break o = (Break) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Break o = (Break) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Continue extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.ContinueStatement value;

    public Continue (hydra.java.syntax.ContinueStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Continue)) {
        return false;
      }
      Continue o = (Continue) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Continue o = (Continue) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Return extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.ReturnStatement value;

    public Return (hydra.java.syntax.ReturnStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Return)) {
        return false;
      }
      Return o = (Return) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Return o = (Return) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Synchronized extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.SynchronizedStatement value;

    public Synchronized (hydra.java.syntax.SynchronizedStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Synchronized)) {
        return false;
      }
      Synchronized o = (Synchronized) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Synchronized o = (Synchronized) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Throw extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.ThrowStatement value;

    public Throw (hydra.java.syntax.ThrowStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Throw)) {
        return false;
      }
      Throw o = (Throw) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Throw o = (Throw) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Try extends hydra.java.syntax.StatementWithoutTrailingSubstatement implements Serializable {
    public final hydra.java.syntax.TryStatement value;

    public Try (hydra.java.syntax.TryStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Try)) {
        return false;
      }
      Try o = (Try) other;
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
    public int compareTo(StatementWithoutTrailingSubstatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Try o = (Try) other;
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
