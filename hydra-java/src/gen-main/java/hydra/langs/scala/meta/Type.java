// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public abstract class Type implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Type");
  
  public static final hydra.core.Name FIELD_NAME_REF = new hydra.core.Name("ref");
  
  public static final hydra.core.Name FIELD_NAME_ANONYMOUS_NAME = new hydra.core.Name("anonymousName");
  
  public static final hydra.core.Name FIELD_NAME_APPLY = new hydra.core.Name("apply");
  
  public static final hydra.core.Name FIELD_NAME_APPLY_INFIX = new hydra.core.Name("applyInfix");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION_TYPE = new hydra.core.Name("functionType");
  
  public static final hydra.core.Name FIELD_NAME_POLY_FUNCTION = new hydra.core.Name("polyFunction");
  
  public static final hydra.core.Name FIELD_NAME_IMPLICIT_FUNCTION = new hydra.core.Name("implicitFunction");
  
  public static final hydra.core.Name FIELD_NAME_TUPLE = new hydra.core.Name("tuple");
  
  public static final hydra.core.Name FIELD_NAME_WITH = new hydra.core.Name("with");
  
  public static final hydra.core.Name FIELD_NAME_AND = new hydra.core.Name("and");
  
  public static final hydra.core.Name FIELD_NAME_OR = new hydra.core.Name("or");
  
  public static final hydra.core.Name FIELD_NAME_REFINE = new hydra.core.Name("refine");
  
  public static final hydra.core.Name FIELD_NAME_EXISTENTIAL = new hydra.core.Name("existential");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATE = new hydra.core.Name("annotate");
  
  public static final hydra.core.Name FIELD_NAME_LAMBDA = new hydra.core.Name("lambda");
  
  public static final hydra.core.Name FIELD_NAME_MACRO = new hydra.core.Name("macro");
  
  public static final hydra.core.Name FIELD_NAME_METHOD = new hydra.core.Name("method");
  
  public static final hydra.core.Name FIELD_NAME_PLACEHOLDER = new hydra.core.Name("placeholder");
  
  public static final hydra.core.Name FIELD_NAME_BY_NAME = new hydra.core.Name("byName");
  
  public static final hydra.core.Name FIELD_NAME_REPEATED = new hydra.core.Name("repeated");
  
  public static final hydra.core.Name FIELD_NAME_VAR = new hydra.core.Name("var");
  
  public static final hydra.core.Name FIELD_NAME_TYPED_PARAM = new hydra.core.Name("typedParam");
  
  public static final hydra.core.Name FIELD_NAME_MATCH = new hydra.core.Name("match");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Ref instance) {
      return otherwise((instance));
    }
    
    default R visit(AnonymousName instance) {
      return otherwise((instance));
    }
    
    default R visit(Apply instance) {
      return otherwise((instance));
    }
    
    default R visit(ApplyInfix instance) {
      return otherwise((instance));
    }
    
    default R visit(FunctionType instance) {
      return otherwise((instance));
    }
    
    default R visit(PolyFunction instance) {
      return otherwise((instance));
    }
    
    default R visit(ImplicitFunction instance) {
      return otherwise((instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
    
    default R visit(With instance) {
      return otherwise((instance));
    }
    
    default R visit(And instance) {
      return otherwise((instance));
    }
    
    default R visit(Or instance) {
      return otherwise((instance));
    }
    
    default R visit(Refine instance) {
      return otherwise((instance));
    }
    
    default R visit(Existential instance) {
      return otherwise((instance));
    }
    
    default R visit(Annotate instance) {
      return otherwise((instance));
    }
    
    default R visit(Lambda instance) {
      return otherwise((instance));
    }
    
    default R visit(Macro instance) {
      return otherwise((instance));
    }
    
    default R visit(Method instance) {
      return otherwise((instance));
    }
    
    default R visit(Placeholder instance) {
      return otherwise((instance));
    }
    
    default R visit(ByName instance) {
      return otherwise((instance));
    }
    
    default R visit(Repeated instance) {
      return otherwise((instance));
    }
    
    default R visit(Var instance) {
      return otherwise((instance));
    }
    
    default R visit(TypedParam instance) {
      return otherwise((instance));
    }
    
    default R visit(Match instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ref extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Ref value;
    
    public Ref (hydra.langs.scala.meta.Type_Ref value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ref)) {
        return false;
      }
      Ref o = (Ref) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class AnonymousName extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_AnonymousName value;
    
    public AnonymousName (hydra.langs.scala.meta.Type_AnonymousName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnonymousName)) {
        return false;
      }
      AnonymousName o = (AnonymousName) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Apply extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Apply value;
    
    public Apply (hydra.langs.scala.meta.Type_Apply value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Apply)) {
        return false;
      }
      Apply o = (Apply) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ApplyInfix extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_ApplyInfix value;
    
    public ApplyInfix (hydra.langs.scala.meta.Type_ApplyInfix value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplyInfix)) {
        return false;
      }
      ApplyInfix o = (ApplyInfix) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class FunctionType extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_FunctionType value;
    
    public FunctionType (hydra.langs.scala.meta.Type_FunctionType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionType)) {
        return false;
      }
      FunctionType o = (FunctionType) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PolyFunction extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_PolyFunction value;
    
    public PolyFunction (hydra.langs.scala.meta.Type_PolyFunction value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PolyFunction)) {
        return false;
      }
      PolyFunction o = (PolyFunction) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ImplicitFunction extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_ImplicitFunction value;
    
    public ImplicitFunction (hydra.langs.scala.meta.Type_ImplicitFunction value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImplicitFunction)) {
        return false;
      }
      ImplicitFunction o = (ImplicitFunction) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Tuple extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Tuple value;
    
    public Tuple (hydra.langs.scala.meta.Type_Tuple value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class With extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_With value;
    
    public With (hydra.langs.scala.meta.Type_With value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof With)) {
        return false;
      }
      With o = (With) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class And extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_And value;
    
    public And (hydra.langs.scala.meta.Type_And value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Or extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Or value;
    
    public Or (hydra.langs.scala.meta.Type_Or value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Refine extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Refine value;
    
    public Refine (hydra.langs.scala.meta.Type_Refine value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Refine)) {
        return false;
      }
      Refine o = (Refine) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Existential extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Existential value;
    
    public Existential (hydra.langs.scala.meta.Type_Existential value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Existential)) {
        return false;
      }
      Existential o = (Existential) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Annotate extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Annotate value;
    
    public Annotate (hydra.langs.scala.meta.Type_Annotate value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotate)) {
        return false;
      }
      Annotate o = (Annotate) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Lambda extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Lambda value;
    
    public Lambda (hydra.langs.scala.meta.Type_Lambda value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lambda)) {
        return false;
      }
      Lambda o = (Lambda) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Macro extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Macro value;
    
    public Macro (hydra.langs.scala.meta.Type_Macro value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Macro)) {
        return false;
      }
      Macro o = (Macro) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Method extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Method value;
    
    public Method (hydra.langs.scala.meta.Type_Method value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Method)) {
        return false;
      }
      Method o = (Method) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Placeholder extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Placeholder value;
    
    public Placeholder (hydra.langs.scala.meta.Type_Placeholder value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Placeholder)) {
        return false;
      }
      Placeholder o = (Placeholder) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ByName extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_ByName value;
    
    public ByName (hydra.langs.scala.meta.Type_ByName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ByName)) {
        return false;
      }
      ByName o = (ByName) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Repeated extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Repeated value;
    
    public Repeated (hydra.langs.scala.meta.Type_Repeated value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeated)) {
        return false;
      }
      Repeated o = (Repeated) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Var extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Var value;
    
    public Var (hydra.langs.scala.meta.Type_Var value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Var)) {
        return false;
      }
      Var o = (Var) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TypedParam extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_TypedParam value;
    
    public TypedParam (hydra.langs.scala.meta.Type_TypedParam value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypedParam)) {
        return false;
      }
      TypedParam o = (TypedParam) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Match extends hydra.langs.scala.meta.Type implements Serializable {
    public final hydra.langs.scala.meta.Type_Match value;
    
    public Match (hydra.langs.scala.meta.Type_Match value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}