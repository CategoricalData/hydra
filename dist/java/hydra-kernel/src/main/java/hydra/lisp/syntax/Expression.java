// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A Lisp expression
 */
public abstract class Expression implements Serializable, Comparable<Expression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.Expression");

  public static final hydra.core.Name APPLICATION = new hydra.core.Name("application");

  public static final hydra.core.Name LAMBDA = new hydra.core.Name("lambda");

  public static final hydra.core.Name LET = new hydra.core.Name("let");

  public static final hydra.core.Name IF = new hydra.core.Name("if");

  public static final hydra.core.Name COND = new hydra.core.Name("cond");

  public static final hydra.core.Name CASE = new hydra.core.Name("case");

  public static final hydra.core.Name AND = new hydra.core.Name("and");

  public static final hydra.core.Name OR = new hydra.core.Name("or");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public static final hydra.core.Name DO = new hydra.core.Name("do");

  public static final hydra.core.Name BEGIN = new hydra.core.Name("begin");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name VECTOR = new hydra.core.Name("vector");

  public static final hydra.core.Name MAP = new hydra.core.Name("map");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name CONS = new hydra.core.Name("cons");

  public static final hydra.core.Name DOTTED_PAIR = new hydra.core.Name("dottedPair");

  public static final hydra.core.Name FIELD_ACCESS = new hydra.core.Name("fieldAccess");

  public static final hydra.core.Name TYPE_ANNOTATION = new hydra.core.Name("typeAnnotation");

  public static final hydra.core.Name QUOTE = new hydra.core.Name("quote");

  public static final hydra.core.Name QUASIQUOTE = new hydra.core.Name("quasiquote");

  public static final hydra.core.Name UNQUOTE = new hydra.core.Name("unquote");

  public static final hydra.core.Name SPLICING_UNQUOTE = new hydra.core.Name("splicingUnquote");

  public static final hydra.core.Name S_EXPRESSION = new hydra.core.Name("sExpression");

  private Expression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Application instance) ;

    R visit(Lambda instance) ;

    R visit(Let instance) ;

    R visit(If instance) ;

    R visit(Cond instance) ;

    R visit(Case instance) ;

    R visit(And instance) ;

    R visit(Or instance) ;

    R visit(Not instance) ;

    R visit(Do instance) ;

    R visit(Begin instance) ;

    R visit(Variable instance) ;

    R visit(Literal instance) ;

    R visit(List instance) ;

    R visit(Vector instance) ;

    R visit(Map instance) ;

    R visit(Set instance) ;

    R visit(Cons instance) ;

    R visit(DottedPair instance) ;

    R visit(FieldAccess instance) ;

    R visit(TypeAnnotation instance) ;

    R visit(Quote instance) ;

    R visit(Quasiquote instance) ;

    R visit(Unquote instance) ;

    R visit(SplicingUnquote instance) ;

    R visit(SExpression instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Expression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Application instance) {
      return otherwise(instance);
    }

    default R visit(Lambda instance) {
      return otherwise(instance);
    }

    default R visit(Let instance) {
      return otherwise(instance);
    }

    default R visit(If instance) {
      return otherwise(instance);
    }

    default R visit(Cond instance) {
      return otherwise(instance);
    }

    default R visit(Case instance) {
      return otherwise(instance);
    }

    default R visit(And instance) {
      return otherwise(instance);
    }

    default R visit(Or instance) {
      return otherwise(instance);
    }

    default R visit(Not instance) {
      return otherwise(instance);
    }

    default R visit(Do instance) {
      return otherwise(instance);
    }

    default R visit(Begin instance) {
      return otherwise(instance);
    }

    default R visit(Variable instance) {
      return otherwise(instance);
    }

    default R visit(Literal instance) {
      return otherwise(instance);
    }

    default R visit(List instance) {
      return otherwise(instance);
    }

    default R visit(Vector instance) {
      return otherwise(instance);
    }

    default R visit(Map instance) {
      return otherwise(instance);
    }

    default R visit(Set instance) {
      return otherwise(instance);
    }

    default R visit(Cons instance) {
      return otherwise(instance);
    }

    default R visit(DottedPair instance) {
      return otherwise(instance);
    }

    default R visit(FieldAccess instance) {
      return otherwise(instance);
    }

    default R visit(TypeAnnotation instance) {
      return otherwise(instance);
    }

    default R visit(Quote instance) {
      return otherwise(instance);
    }

    default R visit(Quasiquote instance) {
      return otherwise(instance);
    }

    default R visit(Unquote instance) {
      return otherwise(instance);
    }

    default R visit(SplicingUnquote instance) {
      return otherwise(instance);
    }

    default R visit(SExpression instance) {
      return otherwise(instance);
    }
  }

  /**
   * Function application: (f arg1 arg2 ...)
   */
  public static final class Application extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.Application value;

    public Application (hydra.lisp.syntax.Application value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Application o = (Application) other;
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
   * Anonymous function
   */
  public static final class Lambda extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.Lambda value;

    public Lambda (hydra.lisp.syntax.Lambda value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lambda)) {
        return false;
      }
      Lambda o = (Lambda) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Lambda o = (Lambda) other;
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
   * Local variable binding
   */
  public static final class Let extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.LetExpression value;

    public Let (hydra.lisp.syntax.LetExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Let o = (Let) other;
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
   * Conditional expression
   */
  public static final class If extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.IfExpression value;

    public If (hydra.lisp.syntax.IfExpression value) {
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
    public int compareTo(Expression other) {
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
   * Multi-branch conditional
   */
  public static final class Cond extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.CondExpression value;

    public Cond (hydra.lisp.syntax.CondExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cond)) {
        return false;
      }
      Cond o = (Cond) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Cond o = (Cond) other;
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
   * Case/match dispatch
   */
  public static final class Case extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.CaseExpression value;

    public Case (hydra.lisp.syntax.CaseExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Case)) {
        return false;
      }
      Case o = (Case) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Case o = (Case) other;
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
   * Logical and (short-circuiting)
   */
  public static final class And extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.AndExpression value;

    public And (hydra.lisp.syntax.AndExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      And o = (And) other;
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
   * Logical or (short-circuiting)
   */
  public static final class Or extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.OrExpression value;

    public Or (hydra.lisp.syntax.OrExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Or o = (Or) other;
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
   * Logical negation
   */
  public static final class Not extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.NotExpression value;

    public Not (hydra.lisp.syntax.NotExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Not o = (Not) other;
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
   * Sequential evaluation (progn/do/begin)
   */
  public static final class Do extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.DoExpression value;

    public Do (hydra.lisp.syntax.DoExpression value) {
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
    public int compareTo(Expression other) {
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

  /**
   * Sequential evaluation (explicit begin block)
   */
  public static final class Begin extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.BeginExpression value;

    public Begin (hydra.lisp.syntax.BeginExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Begin)) {
        return false;
      }
      Begin o = (Begin) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Begin o = (Begin) other;
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
   * Variable reference
   */
  public static final class Variable extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.VariableReference value;

    public Variable (hydra.lisp.syntax.VariableReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Variable o = (Variable) other;
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
   * A literal value
   */
  public static final class Literal extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.Literal value;

    public Literal (hydra.lisp.syntax.Literal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Literal o = (Literal) other;
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
   * A list literal
   */
  public static final class List extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.ListLiteral value;

    public List (hydra.lisp.syntax.ListLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      List o = (List) other;
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
   * A vector literal
   */
  public static final class Vector extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.VectorLiteral value;

    public Vector (hydra.lisp.syntax.VectorLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vector)) {
        return false;
      }
      Vector o = (Vector) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Vector o = (Vector) other;
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
   * A map/association literal
   */
  public static final class Map extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.MapLiteral value;

    public Map (hydra.lisp.syntax.MapLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Map o = (Map) other;
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
   * A set literal
   */
  public static final class Set extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.SetLiteral value;

    public Set (hydra.lisp.syntax.SetLiteral value) {
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
    public int compareTo(Expression other) {
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

  /**
   * A cons expression
   */
  public static final class Cons extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.ConsExpression value;

    public Cons (hydra.lisp.syntax.ConsExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cons)) {
        return false;
      }
      Cons o = (Cons) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Cons o = (Cons) other;
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
   * A dotted pair literal
   */
  public static final class DottedPair extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.DottedPair value;

    public DottedPair (hydra.lisp.syntax.DottedPair value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DottedPair)) {
        return false;
      }
      DottedPair o = (DottedPair) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DottedPair o = (DottedPair) other;
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
   * Field access on a record/struct
   */
  public static final class FieldAccess extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.FieldAccess value;

    public FieldAccess (hydra.lisp.syntax.FieldAccess value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FieldAccess)) {
        return false;
      }
      FieldAccess o = (FieldAccess) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FieldAccess o = (FieldAccess) other;
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
   * A type-annotated expression
   */
  public static final class TypeAnnotation extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.TypeAnnotation value;

    public TypeAnnotation (hydra.lisp.syntax.TypeAnnotation value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeAnnotation)) {
        return false;
      }
      TypeAnnotation o = (TypeAnnotation) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeAnnotation o = (TypeAnnotation) other;
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
   * A quoted expression
   */
  public static final class Quote extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.QuoteExpression value;

    public Quote (hydra.lisp.syntax.QuoteExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Quote)) {
        return false;
      }
      Quote o = (Quote) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Quote o = (Quote) other;
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
   * A quasiquoted expression
   */
  public static final class Quasiquote extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.QuasiquoteExpression value;

    public Quasiquote (hydra.lisp.syntax.QuasiquoteExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Quasiquote)) {
        return false;
      }
      Quasiquote o = (Quasiquote) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Quasiquote o = (Quasiquote) other;
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
   * An unquoted expression within a quasiquote
   */
  public static final class Unquote extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.UnquoteExpression value;

    public Unquote (hydra.lisp.syntax.UnquoteExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unquote)) {
        return false;
      }
      Unquote o = (Unquote) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Unquote o = (Unquote) other;
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
   * A splicing unquote within a quasiquote
   */
  public static final class SplicingUnquote extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.SplicingUnquoteExpression value;

    public SplicingUnquote (hydra.lisp.syntax.SplicingUnquoteExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SplicingUnquote)) {
        return false;
      }
      SplicingUnquote o = (SplicingUnquote) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SplicingUnquote o = (SplicingUnquote) other;
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
   * An arbitrary S-expression (escape hatch for dialect-specific forms)
   */
  public static final class SExpression extends hydra.lisp.syntax.Expression implements Serializable {
    public final hydra.lisp.syntax.SExpression value;

    public SExpression (hydra.lisp.syntax.SExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SExpression)) {
        return false;
      }
      SExpression o = (SExpression) other;
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
    public int compareTo(Expression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SExpression o = (SExpression) other;
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
