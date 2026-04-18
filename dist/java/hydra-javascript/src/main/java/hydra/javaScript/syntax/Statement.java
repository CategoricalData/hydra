// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A JavaScript statement
 */
public abstract class Statement implements Serializable, Comparable<Statement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.Statement");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  public static final hydra.core.Name BLOCK = new hydra.core.Name("block");

  public static final hydra.core.Name EMPTY = new hydra.core.Name("empty");

  public static final hydra.core.Name DEBUGGER = new hydra.core.Name("debugger");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public static final hydra.core.Name BREAK = new hydra.core.Name("break");

  public static final hydra.core.Name CONTINUE = new hydra.core.Name("continue");

  public static final hydra.core.Name IF = new hydra.core.Name("if");

  public static final hydra.core.Name SWITCH = new hydra.core.Name("switch");

  public static final hydra.core.Name THROW = new hydra.core.Name("throw");

  public static final hydra.core.Name TRY = new hydra.core.Name("try");

  public static final hydra.core.Name WHILE = new hydra.core.Name("while");

  public static final hydra.core.Name DO_WHILE = new hydra.core.Name("doWhile");

  public static final hydra.core.Name FOR = new hydra.core.Name("for");

  public static final hydra.core.Name FOR_IN = new hydra.core.Name("forIn");

  public static final hydra.core.Name FOR_OF = new hydra.core.Name("forOf");

  public static final hydra.core.Name VARIABLE_DECLARATION = new hydra.core.Name("variableDeclaration");

  public static final hydra.core.Name FUNCTION_DECLARATION = new hydra.core.Name("functionDeclaration");

  public static final hydra.core.Name CLASS_DECLARATION = new hydra.core.Name("classDeclaration");

  public static final hydra.core.Name LABELED = new hydra.core.Name("labeled");

  private Statement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Expression instance) ;

    R visit(Block instance) ;

    R visit(Empty instance) ;

    R visit(Debugger instance) ;

    R visit(Return instance) ;

    R visit(Break instance) ;

    R visit(Continue instance) ;

    R visit(If instance) ;

    R visit(Switch instance) ;

    R visit(Throw instance) ;

    R visit(Try instance) ;

    R visit(While instance) ;

    R visit(DoWhile instance) ;

    R visit(For instance) ;

    R visit(ForIn instance) ;

    R visit(ForOf instance) ;

    R visit(VariableDeclaration instance) ;

    R visit(FunctionDeclaration instance) ;

    R visit(ClassDeclaration instance) ;

    R visit(Labeled instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Statement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Expression instance) {
      return otherwise(instance);
    }

    default R visit(Block instance) {
      return otherwise(instance);
    }

    default R visit(Empty instance) {
      return otherwise(instance);
    }

    default R visit(Debugger instance) {
      return otherwise(instance);
    }

    default R visit(Return instance) {
      return otherwise(instance);
    }

    default R visit(Break instance) {
      return otherwise(instance);
    }

    default R visit(Continue instance) {
      return otherwise(instance);
    }

    default R visit(If instance) {
      return otherwise(instance);
    }

    default R visit(Switch instance) {
      return otherwise(instance);
    }

    default R visit(Throw instance) {
      return otherwise(instance);
    }

    default R visit(Try instance) {
      return otherwise(instance);
    }

    default R visit(While instance) {
      return otherwise(instance);
    }

    default R visit(DoWhile instance) {
      return otherwise(instance);
    }

    default R visit(For instance) {
      return otherwise(instance);
    }

    default R visit(ForIn instance) {
      return otherwise(instance);
    }

    default R visit(ForOf instance) {
      return otherwise(instance);
    }

    default R visit(VariableDeclaration instance) {
      return otherwise(instance);
    }

    default R visit(FunctionDeclaration instance) {
      return otherwise(instance);
    }

    default R visit(ClassDeclaration instance) {
      return otherwise(instance);
    }

    default R visit(Labeled instance) {
      return otherwise(instance);
    }
  }

  /**
   * An expression statement
   */
  public static final class Expression extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.Expression value;

    public Expression (hydra.javaScript.syntax.Expression value) {
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
    public int compareTo(Statement other) {
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

  /**
   * A block statement
   */
  public static final class Block extends hydra.javaScript.syntax.Statement implements Serializable {
    public final java.util.List<hydra.javaScript.syntax.Statement> value;

    public Block (java.util.List<hydra.javaScript.syntax.Statement> value) {
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
    public int compareTo(Statement other) {
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

  /**
   * An empty statement (;)
   */
  public static final class Empty extends hydra.javaScript.syntax.Statement implements Serializable {
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
    public int compareTo(Statement other) {
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
   * A debugger statement
   */
  public static final class Debugger extends hydra.javaScript.syntax.Statement implements Serializable {
    public Debugger () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Debugger)) {
        return false;
      }
      Debugger o = (Debugger) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Statement other) {
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
   * A return statement
   */
  public static final class Return extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.util.Maybe<hydra.javaScript.syntax.Expression> value;

    public Return (hydra.util.Maybe<hydra.javaScript.syntax.Expression> value) {
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
    public int compareTo(Statement other) {
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

  /**
   * A break statement
   */
  public static final class Break extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.util.Maybe<hydra.javaScript.syntax.Identifier> value;

    public Break (hydra.util.Maybe<hydra.javaScript.syntax.Identifier> value) {
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
    public int compareTo(Statement other) {
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

  /**
   * A continue statement
   */
  public static final class Continue extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.util.Maybe<hydra.javaScript.syntax.Identifier> value;

    public Continue (hydra.util.Maybe<hydra.javaScript.syntax.Identifier> value) {
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
    public int compareTo(Statement other) {
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

  /**
   * An if statement
   */
  public static final class If extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.IfStatement value;

    public If (hydra.javaScript.syntax.IfStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof If)) {
        return false;
      }
      If o = (If) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      If o = (If) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A switch statement
   */
  public static final class Switch extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.SwitchStatement value;

    public Switch (hydra.javaScript.syntax.SwitchStatement value) {
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
    public int compareTo(Statement other) {
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

  /**
   * A throw statement
   */
  public static final class Throw extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.ThrowStatement value;

    public Throw (hydra.javaScript.syntax.ThrowStatement value) {
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
    public int compareTo(Statement other) {
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

  /**
   * A try statement
   */
  public static final class Try extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.TryStatement value;

    public Try (hydra.javaScript.syntax.TryStatement value) {
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
    public int compareTo(Statement other) {
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

  /**
   * A while statement
   */
  public static final class While extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.WhileStatement value;

    public While (hydra.javaScript.syntax.WhileStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof While)) {
        return false;
      }
      While o = (While) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      While o = (While) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A do-while statement
   */
  public static final class DoWhile extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.DoWhileStatement value;

    public DoWhile (hydra.javaScript.syntax.DoWhileStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DoWhile)) {
        return false;
      }
      DoWhile o = (DoWhile) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DoWhile o = (DoWhile) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A for statement
   */
  public static final class For extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.ForStatement value;

    public For (hydra.javaScript.syntax.ForStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof For)) {
        return false;
      }
      For o = (For) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      For o = (For) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A for-in statement
   */
  public static final class ForIn extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.ForInStatement value;

    public ForIn (hydra.javaScript.syntax.ForInStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ForIn)) {
        return false;
      }
      ForIn o = (ForIn) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ForIn o = (ForIn) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A for-of statement
   */
  public static final class ForOf extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.ForOfStatement value;

    public ForOf (hydra.javaScript.syntax.ForOfStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ForOf)) {
        return false;
      }
      ForOf o = (ForOf) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ForOf o = (ForOf) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A variable declaration
   */
  public static final class VariableDeclaration extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.VariableDeclaration value;

    public VariableDeclaration (hydra.javaScript.syntax.VariableDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariableDeclaration)) {
        return false;
      }
      VariableDeclaration o = (VariableDeclaration) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      VariableDeclaration o = (VariableDeclaration) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A function declaration
   */
  public static final class FunctionDeclaration extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.FunctionDeclaration value;

    public FunctionDeclaration (hydra.javaScript.syntax.FunctionDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionDeclaration)) {
        return false;
      }
      FunctionDeclaration o = (FunctionDeclaration) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FunctionDeclaration o = (FunctionDeclaration) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A class declaration
   */
  public static final class ClassDeclaration extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.ClassDeclaration value;

    public ClassDeclaration (hydra.javaScript.syntax.ClassDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassDeclaration)) {
        return false;
      }
      ClassDeclaration o = (ClassDeclaration) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ClassDeclaration o = (ClassDeclaration) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A labeled statement
   */
  public static final class Labeled extends hydra.javaScript.syntax.Statement implements Serializable {
    public final hydra.javaScript.syntax.LabeledStatement value;

    public Labeled (hydra.javaScript.syntax.LabeledStatement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Labeled)) {
        return false;
      }
      Labeled o = (Labeled) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Labeled o = (Labeled) other;
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
