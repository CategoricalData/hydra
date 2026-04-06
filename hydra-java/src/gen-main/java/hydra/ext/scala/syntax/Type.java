// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class Type implements Serializable, Comparable<Type> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Type");

  public static final hydra.core.Name REF = new hydra.core.Name("ref");

  public static final hydra.core.Name ANONYMOUS_NAME = new hydra.core.Name("anonymousName");

  public static final hydra.core.Name APPLY = new hydra.core.Name("apply");

  public static final hydra.core.Name APPLY_INFIX = new hydra.core.Name("applyInfix");

  public static final hydra.core.Name FUNCTION_TYPE = new hydra.core.Name("functionType");

  public static final hydra.core.Name POLY_FUNCTION = new hydra.core.Name("polyFunction");

  public static final hydra.core.Name IMPLICIT_FUNCTION = new hydra.core.Name("implicitFunction");

  public static final hydra.core.Name TUPLE = new hydra.core.Name("tuple");

  public static final hydra.core.Name WITH = new hydra.core.Name("with");

  public static final hydra.core.Name AND = new hydra.core.Name("and");

  public static final hydra.core.Name OR = new hydra.core.Name("or");

  public static final hydra.core.Name REFINE = new hydra.core.Name("refine");

  public static final hydra.core.Name EXISTENTIAL = new hydra.core.Name("existential");

  public static final hydra.core.Name ANNOTATE = new hydra.core.Name("annotate");

  public static final hydra.core.Name LAMBDA = new hydra.core.Name("lambda");

  public static final hydra.core.Name MACRO = new hydra.core.Name("macro");

  public static final hydra.core.Name METHOD = new hydra.core.Name("method");

  public static final hydra.core.Name PLACEHOLDER = new hydra.core.Name("placeholder");

  public static final hydra.core.Name BY_NAME = new hydra.core.Name("byName");

  public static final hydra.core.Name REPEATED = new hydra.core.Name("repeated");

  public static final hydra.core.Name VAR = new hydra.core.Name("var");

  public static final hydra.core.Name TYPED_PARAM = new hydra.core.Name("typedParam");

  public static final hydra.core.Name MATCH = new hydra.core.Name("match");

  private Type () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Ref instance) ;

    R visit(AnonymousName instance) ;

    R visit(Apply instance) ;

    R visit(ApplyInfix instance) ;

    R visit(FunctionType instance) ;

    R visit(PolyFunction instance) ;

    R visit(ImplicitFunction instance) ;

    R visit(Tuple instance) ;

    R visit(With instance) ;

    R visit(And instance) ;

    R visit(Or instance) ;

    R visit(Refine instance) ;

    R visit(Existential instance) ;

    R visit(Annotate instance) ;

    R visit(Lambda instance) ;

    R visit(Macro instance) ;

    R visit(Method instance) ;

    R visit(Placeholder instance) ;

    R visit(ByName instance) ;

    R visit(Repeated instance) ;

    R visit(Var instance) ;

    R visit(TypedParam instance) ;

    R visit(Match instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Type instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Ref instance) {
      return otherwise(instance);
    }

    default R visit(AnonymousName instance) {
      return otherwise(instance);
    }

    default R visit(Apply instance) {
      return otherwise(instance);
    }

    default R visit(ApplyInfix instance) {
      return otherwise(instance);
    }

    default R visit(FunctionType instance) {
      return otherwise(instance);
    }

    default R visit(PolyFunction instance) {
      return otherwise(instance);
    }

    default R visit(ImplicitFunction instance) {
      return otherwise(instance);
    }

    default R visit(Tuple instance) {
      return otherwise(instance);
    }

    default R visit(With instance) {
      return otherwise(instance);
    }

    default R visit(And instance) {
      return otherwise(instance);
    }

    default R visit(Or instance) {
      return otherwise(instance);
    }

    default R visit(Refine instance) {
      return otherwise(instance);
    }

    default R visit(Existential instance) {
      return otherwise(instance);
    }

    default R visit(Annotate instance) {
      return otherwise(instance);
    }

    default R visit(Lambda instance) {
      return otherwise(instance);
    }

    default R visit(Macro instance) {
      return otherwise(instance);
    }

    default R visit(Method instance) {
      return otherwise(instance);
    }

    default R visit(Placeholder instance) {
      return otherwise(instance);
    }

    default R visit(ByName instance) {
      return otherwise(instance);
    }

    default R visit(Repeated instance) {
      return otherwise(instance);
    }

    default R visit(Var instance) {
      return otherwise(instance);
    }

    default R visit(TypedParam instance) {
      return otherwise(instance);
    }

    default R visit(Match instance) {
      return otherwise(instance);
    }
  }

  public static final class Ref extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Ref value;

    public Ref (hydra.ext.scala.syntax.Type_Ref value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ref)) {
        return false;
      }
      Ref o = (Ref) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Ref o = (Ref) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AnonymousName extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_AnonymousName value;

    public AnonymousName (hydra.ext.scala.syntax.Type_AnonymousName value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnonymousName)) {
        return false;
      }
      AnonymousName o = (AnonymousName) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnonymousName o = (AnonymousName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Apply extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Apply value;

    public Apply (hydra.ext.scala.syntax.Type_Apply value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Apply)) {
        return false;
      }
      Apply o = (Apply) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Apply o = (Apply) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ApplyInfix extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_ApplyInfix value;

    public ApplyInfix (hydra.ext.scala.syntax.Type_ApplyInfix value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplyInfix)) {
        return false;
      }
      ApplyInfix o = (ApplyInfix) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ApplyInfix o = (ApplyInfix) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class FunctionType extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_FunctionType value;

    public FunctionType (hydra.ext.scala.syntax.Type_FunctionType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionType)) {
        return false;
      }
      FunctionType o = (FunctionType) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FunctionType o = (FunctionType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PolyFunction extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_PolyFunction value;

    public PolyFunction (hydra.ext.scala.syntax.Type_PolyFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PolyFunction)) {
        return false;
      }
      PolyFunction o = (PolyFunction) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PolyFunction o = (PolyFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ImplicitFunction extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_ImplicitFunction value;

    public ImplicitFunction (hydra.ext.scala.syntax.Type_ImplicitFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImplicitFunction)) {
        return false;
      }
      ImplicitFunction o = (ImplicitFunction) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ImplicitFunction o = (ImplicitFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Tuple extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Tuple value;

    public Tuple (hydra.ext.scala.syntax.Type_Tuple value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Tuple o = (Tuple) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class With extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_With value;

    public With (hydra.ext.scala.syntax.Type_With value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof With)) {
        return false;
      }
      With o = (With) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      With o = (With) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class And extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_And value;

    public And (hydra.ext.scala.syntax.Type_And value) {
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
    public int compareTo(Type other) {
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

  public static final class Or extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Or value;

    public Or (hydra.ext.scala.syntax.Type_Or value) {
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
    public int compareTo(Type other) {
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

  public static final class Refine extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Refine value;

    public Refine (hydra.ext.scala.syntax.Type_Refine value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Refine)) {
        return false;
      }
      Refine o = (Refine) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Refine o = (Refine) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Existential extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Existential value;

    public Existential (hydra.ext.scala.syntax.Type_Existential value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Existential)) {
        return false;
      }
      Existential o = (Existential) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Existential o = (Existential) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Annotate extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Annotate value;

    public Annotate (hydra.ext.scala.syntax.Type_Annotate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotate)) {
        return false;
      }
      Annotate o = (Annotate) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Annotate o = (Annotate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Lambda extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Lambda value;

    public Lambda (hydra.ext.scala.syntax.Type_Lambda value) {
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
    public int compareTo(Type other) {
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

  public static final class Macro extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Macro value;

    public Macro (hydra.ext.scala.syntax.Type_Macro value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Macro)) {
        return false;
      }
      Macro o = (Macro) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Macro o = (Macro) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Method extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Method value;

    public Method (hydra.ext.scala.syntax.Type_Method value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Method)) {
        return false;
      }
      Method o = (Method) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Method o = (Method) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Placeholder extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Placeholder value;

    public Placeholder (hydra.ext.scala.syntax.Type_Placeholder value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Placeholder)) {
        return false;
      }
      Placeholder o = (Placeholder) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Placeholder o = (Placeholder) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ByName extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_ByName value;

    public ByName (hydra.ext.scala.syntax.Type_ByName value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ByName)) {
        return false;
      }
      ByName o = (ByName) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ByName o = (ByName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Repeated extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Repeated value;

    public Repeated (hydra.ext.scala.syntax.Type_Repeated value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeated)) {
        return false;
      }
      Repeated o = (Repeated) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Repeated o = (Repeated) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Var extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Var value;

    public Var (hydra.ext.scala.syntax.Type_Var value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Var)) {
        return false;
      }
      Var o = (Var) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Var o = (Var) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TypedParam extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_TypedParam value;

    public TypedParam (hydra.ext.scala.syntax.Type_TypedParam value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypedParam)) {
        return false;
      }
      TypedParam o = (TypedParam) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypedParam o = (TypedParam) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Match extends hydra.ext.scala.syntax.Type implements Serializable {
    public final hydra.ext.scala.syntax.Type_Match value;

    public Match (hydra.ext.scala.syntax.Type_Match value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) other;
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
    public int compareTo(Type other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Match o = (Match) other;
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
