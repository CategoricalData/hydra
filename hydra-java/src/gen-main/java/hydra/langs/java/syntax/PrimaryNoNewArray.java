// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public abstract class PrimaryNoNewArray implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.PrimaryNoNewArray");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_LITERAL = new hydra.core.Name("classLiteral");
  
  public static final hydra.core.Name FIELD_NAME_THIS = new hydra.core.Name("this");
  
  public static final hydra.core.Name FIELD_NAME_DOT_THIS = new hydra.core.Name("dotThis");
  
  public static final hydra.core.Name FIELD_NAME_PARENS = new hydra.core.Name("parens");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_INSTANCE = new hydra.core.Name("classInstance");
  
  public static final hydra.core.Name FIELD_NAME_FIELD_ACCESS = new hydra.core.Name("fieldAccess");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY_ACCESS = new hydra.core.Name("arrayAccess");
  
  public static final hydra.core.Name FIELD_NAME_METHOD_INVOCATION = new hydra.core.Name("methodInvocation");
  
  public static final hydra.core.Name FIELD_NAME_METHOD_REFERENCE = new hydra.core.Name("methodReference");
  
  private PrimaryNoNewArray () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Literal instance) ;
    
    R visit(ClassLiteral instance) ;
    
    R visit(This instance) ;
    
    R visit(DotThis instance) ;
    
    R visit(Parens instance) ;
    
    R visit(ClassInstance instance) ;
    
    R visit(FieldAccess instance) ;
    
    R visit(ArrayAccess instance) ;
    
    R visit(MethodInvocation instance) ;
    
    R visit(MethodReference instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimaryNoNewArray instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(ClassLiteral instance) {
      return otherwise((instance));
    }
    
    default R visit(This instance) {
      return otherwise((instance));
    }
    
    default R visit(DotThis instance) {
      return otherwise((instance));
    }
    
    default R visit(Parens instance) {
      return otherwise((instance));
    }
    
    default R visit(ClassInstance instance) {
      return otherwise((instance));
    }
    
    default R visit(FieldAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(ArrayAccess instance) {
      return otherwise((instance));
    }
    
    default R visit(MethodInvocation instance) {
      return otherwise((instance));
    }
    
    default R visit(MethodReference instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Literal extends hydra.langs.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.langs.java.syntax.Literal value;
    
    public Literal (hydra.langs.java.syntax.Literal value) {
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
  
  public static final class ClassLiteral extends hydra.langs.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.langs.java.syntax.ClassLiteral value;
    
    public ClassLiteral (hydra.langs.java.syntax.ClassLiteral value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassLiteral)) {
        return false;
      }
      ClassLiteral o = (ClassLiteral) (other);
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
  
  public static final class This extends hydra.langs.java.syntax.PrimaryNoNewArray implements Serializable {
    public This () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof This)) {
        return false;
      }
      This o = (This) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DotThis extends hydra.langs.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.langs.java.syntax.TypeName value;
    
    public DotThis (hydra.langs.java.syntax.TypeName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DotThis)) {
        return false;
      }
      DotThis o = (DotThis) (other);
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
  
  public static final class Parens extends hydra.langs.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.langs.java.syntax.Expression value;
    
    public Parens (hydra.langs.java.syntax.Expression value) {
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
  
  public static final class ClassInstance extends hydra.langs.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.langs.java.syntax.ClassInstanceCreationExpression value;
    
    public ClassInstance (hydra.langs.java.syntax.ClassInstanceCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassInstance)) {
        return false;
      }
      ClassInstance o = (ClassInstance) (other);
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
  
  public static final class FieldAccess extends hydra.langs.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.langs.java.syntax.FieldAccess value;
    
    public FieldAccess (hydra.langs.java.syntax.FieldAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FieldAccess)) {
        return false;
      }
      FieldAccess o = (FieldAccess) (other);
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
  
  public static final class ArrayAccess extends hydra.langs.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.langs.java.syntax.ArrayAccess value;
    
    public ArrayAccess (hydra.langs.java.syntax.ArrayAccess value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ArrayAccess)) {
        return false;
      }
      ArrayAccess o = (ArrayAccess) (other);
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
  
  public static final class MethodInvocation extends hydra.langs.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.langs.java.syntax.MethodInvocation value;
    
    public MethodInvocation (hydra.langs.java.syntax.MethodInvocation value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MethodInvocation)) {
        return false;
      }
      MethodInvocation o = (MethodInvocation) (other);
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
  
  public static final class MethodReference extends hydra.langs.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.langs.java.syntax.MethodReference value;
    
    public MethodReference (hydra.langs.java.syntax.MethodReference value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MethodReference)) {
        return false;
      }
      MethodReference o = (MethodReference) (other);
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