package hydra.ext.scala.meta;

public abstract class Type {
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
  
  public static final class Ref extends Type {
    public final Type_Ref value;
    
    public Ref (Type_Ref value) {
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
  
  public static final class AnonymousName extends Type {
    public final Type_AnonymousName value;
    
    public AnonymousName (Type_AnonymousName value) {
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
  
  public static final class Apply extends Type {
    public final Type_Apply value;
    
    public Apply (Type_Apply value) {
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
  
  public static final class ApplyInfix extends Type {
    public final Type_ApplyInfix value;
    
    public ApplyInfix (Type_ApplyInfix value) {
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
  
  public static final class FunctionType extends Type {
    public final Type_FunctionType value;
    
    public FunctionType (Type_FunctionType value) {
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
  
  public static final class PolyFunction extends Type {
    public final Type_PolyFunction value;
    
    public PolyFunction (Type_PolyFunction value) {
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
  
  public static final class ImplicitFunction extends Type {
    public final Type_ImplicitFunction value;
    
    public ImplicitFunction (Type_ImplicitFunction value) {
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
  
  public static final class Tuple extends Type {
    public final Type_Tuple value;
    
    public Tuple (Type_Tuple value) {
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
  
  public static final class With extends Type {
    public final Type_With value;
    
    public With (Type_With value) {
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
  
  public static final class And extends Type {
    public final Type_And value;
    
    public And (Type_And value) {
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
  
  public static final class Or extends Type {
    public final Type_Or value;
    
    public Or (Type_Or value) {
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
  
  public static final class Refine extends Type {
    public final Type_Refine value;
    
    public Refine (Type_Refine value) {
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
  
  public static final class Existential extends Type {
    public final Type_Existential value;
    
    public Existential (Type_Existential value) {
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
  
  public static final class Annotate extends Type {
    public final Type_Annotate value;
    
    public Annotate (Type_Annotate value) {
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
  
  public static final class Lambda extends Type {
    public final Type_Lambda value;
    
    public Lambda (Type_Lambda value) {
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
  
  public static final class Macro extends Type {
    public final Type_Macro value;
    
    public Macro (Type_Macro value) {
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
  
  public static final class Method extends Type {
    public final Type_Method value;
    
    public Method (Type_Method value) {
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
  
  public static final class Placeholder extends Type {
    public final Type_Placeholder value;
    
    public Placeholder (Type_Placeholder value) {
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
  
  public static final class ByName extends Type {
    public final Type_ByName value;
    
    public ByName (Type_ByName value) {
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
  
  public static final class Repeated extends Type {
    public final Type_Repeated value;
    
    public Repeated (Type_Repeated value) {
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
  
  public static final class Var extends Type {
    public final Type_Var value;
    
    public Var (Type_Var value) {
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
  
  public static final class TypedParam extends Type {
    public final Type_TypedParam value;
    
    public TypedParam (Type_TypedParam value) {
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
  
  public static final class Match extends Type {
    public final Type_Match value;
    
    public Match (Type_Match value) {
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