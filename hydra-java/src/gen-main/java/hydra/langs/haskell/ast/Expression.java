// Note: this is an automatically generated file. Do not edit.

package hydra.langs.haskell.ast;

import java.io.Serializable;

/**
 * A data expression
 */
public abstract class Expression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/haskell/ast.Expression");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATION = new hydra.core.Name("application");
  
  public static final hydra.core.Name FIELD_NAME_CASE = new hydra.core.Name("case");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRUCT_RECORD = new hydra.core.Name("constructRecord");
  
  public static final hydra.core.Name FIELD_NAME_DO = new hydra.core.Name("do");
  
  public static final hydra.core.Name FIELD_NAME_IF = new hydra.core.Name("if");
  
  public static final hydra.core.Name FIELD_NAME_INFIX_APPLICATION = new hydra.core.Name("infixApplication");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_LAMBDA = new hydra.core.Name("lambda");
  
  public static final hydra.core.Name FIELD_NAME_LEFT_SECTION = new hydra.core.Name("leftSection");
  
  public static final hydra.core.Name FIELD_NAME_LET = new hydra.core.Name("let");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name FIELD_NAME_PARENS = new hydra.core.Name("parens");
  
  public static final hydra.core.Name FIELD_NAME_PREFIX_APPLICATION = new hydra.core.Name("prefixApplication");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT_SECTION = new hydra.core.Name("rightSection");
  
  public static final hydra.core.Name FIELD_NAME_TUPLE = new hydra.core.Name("tuple");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_SIGNATURE = new hydra.core.Name("typeSignature");
  
  public static final hydra.core.Name FIELD_NAME_UPDATE_RECORD = new hydra.core.Name("updateRecord");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  private Expression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Application instance) ;
    
    R visit(Case instance) ;
    
    R visit(ConstructRecord instance) ;
    
    R visit(Do instance) ;
    
    R visit(If instance) ;
    
    R visit(InfixApplication instance) ;
    
    R visit(Literal instance) ;
    
    R visit(Lambda instance) ;
    
    R visit(LeftSection instance) ;
    
    R visit(Let instance) ;
    
    R visit(List instance) ;
    
    R visit(Parens instance) ;
    
    R visit(PrefixApplication instance) ;
    
    R visit(RightSection instance) ;
    
    R visit(Tuple instance) ;
    
    R visit(TypeSignature instance) ;
    
    R visit(UpdateRecord instance) ;
    
    R visit(Variable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Expression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Application instance) {
      return otherwise((instance));
    }
    
    default R visit(Case instance) {
      return otherwise((instance));
    }
    
    default R visit(ConstructRecord instance) {
      return otherwise((instance));
    }
    
    default R visit(Do instance) {
      return otherwise((instance));
    }
    
    default R visit(If instance) {
      return otherwise((instance));
    }
    
    default R visit(InfixApplication instance) {
      return otherwise((instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(Lambda instance) {
      return otherwise((instance));
    }
    
    default R visit(LeftSection instance) {
      return otherwise((instance));
    }
    
    default R visit(Let instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Parens instance) {
      return otherwise((instance));
    }
    
    default R visit(PrefixApplication instance) {
      return otherwise((instance));
    }
    
    default R visit(RightSection instance) {
      return otherwise((instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeSignature instance) {
      return otherwise((instance));
    }
    
    default R visit(UpdateRecord instance) {
      return otherwise((instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Application extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_Application value;
    
    public Application (hydra.langs.haskell.ast.Expression_Application value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) (other);
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
  
  public static final class Case extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_Case value;
    
    public Case (hydra.langs.haskell.ast.Expression_Case value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Case)) {
        return false;
      }
      Case o = (Case) (other);
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
  
  public static final class ConstructRecord extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_ConstructRecord value;
    
    public ConstructRecord (hydra.langs.haskell.ast.Expression_ConstructRecord value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConstructRecord)) {
        return false;
      }
      ConstructRecord o = (ConstructRecord) (other);
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
  
  public static final class Do extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final java.util.List<hydra.langs.haskell.ast.Statement> value;
    
    public Do (java.util.List<hydra.langs.haskell.ast.Statement> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Do)) {
        return false;
      }
      Do o = (Do) (other);
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
  
  public static final class If extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_If value;
    
    public If (hydra.langs.haskell.ast.Expression_If value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof If)) {
        return false;
      }
      If o = (If) (other);
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
  
  public static final class InfixApplication extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_InfixApplication value;
    
    public InfixApplication (hydra.langs.haskell.ast.Expression_InfixApplication value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InfixApplication)) {
        return false;
      }
      InfixApplication o = (InfixApplication) (other);
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
  
  public static final class Literal extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Literal value;
    
    public Literal (hydra.langs.haskell.ast.Literal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
  
  public static final class Lambda extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_Lambda value;
    
    public Lambda (hydra.langs.haskell.ast.Expression_Lambda value) {
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
  
  public static final class LeftSection extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_Section value;
    
    public LeftSection (hydra.langs.haskell.ast.Expression_Section value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftSection)) {
        return false;
      }
      LeftSection o = (LeftSection) (other);
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
  
  public static final class Let extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_Let value;
    
    public Let (hydra.langs.haskell.ast.Expression_Let value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) (other);
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
  
  public static final class List extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final java.util.List<hydra.langs.haskell.ast.Expression> value;
    
    public List (java.util.List<hydra.langs.haskell.ast.Expression> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  public static final class Parens extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression value;
    
    public Parens (hydra.langs.haskell.ast.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) (other);
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
  
  public static final class PrefixApplication extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_PrefixApplication value;
    
    public PrefixApplication (hydra.langs.haskell.ast.Expression_PrefixApplication value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrefixApplication)) {
        return false;
      }
      PrefixApplication o = (PrefixApplication) (other);
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
  
  public static final class RightSection extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_Section value;
    
    public RightSection (hydra.langs.haskell.ast.Expression_Section value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightSection)) {
        return false;
      }
      RightSection o = (RightSection) (other);
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
  
  public static final class Tuple extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final java.util.List<hydra.langs.haskell.ast.Expression> value;
    
    public Tuple (java.util.List<hydra.langs.haskell.ast.Expression> value) {
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
  
  public static final class TypeSignature extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_TypeSignature value;
    
    public TypeSignature (hydra.langs.haskell.ast.Expression_TypeSignature value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeSignature)) {
        return false;
      }
      TypeSignature o = (TypeSignature) (other);
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
  
  public static final class UpdateRecord extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Expression_UpdateRecord value;
    
    public UpdateRecord (hydra.langs.haskell.ast.Expression_UpdateRecord value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UpdateRecord)) {
        return false;
      }
      UpdateRecord o = (UpdateRecord) (other);
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
  
  public static final class Variable extends hydra.langs.haskell.ast.Expression implements Serializable {
    public final hydra.langs.haskell.ast.Name value;
    
    public Variable (hydra.langs.haskell.ast.Name value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) (other);
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