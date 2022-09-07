package hydra.ext.java.syntax;

public abstract class PrimaryNoNewArray {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.PrimaryNoNewArray");
  
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
  
  public static final class Literal extends hydra.ext.java.syntax.PrimaryNoNewArray {
    public final hydra.ext.java.syntax.Literal value;
    
    public Literal (hydra.ext.java.syntax.Literal value) {
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
  
  public static final class ClassLiteral extends hydra.ext.java.syntax.PrimaryNoNewArray {
    public final hydra.ext.java.syntax.ClassLiteral value;
    
    public ClassLiteral (hydra.ext.java.syntax.ClassLiteral value) {
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
  
  public static final class This extends hydra.ext.java.syntax.PrimaryNoNewArray {
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
  
  public static final class DotThis extends hydra.ext.java.syntax.PrimaryNoNewArray {
    public final hydra.ext.java.syntax.TypeName value;
    
    public DotThis (hydra.ext.java.syntax.TypeName value) {
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
  
  public static final class Parens extends hydra.ext.java.syntax.PrimaryNoNewArray {
    public final hydra.ext.java.syntax.Expression value;
    
    public Parens (hydra.ext.java.syntax.Expression value) {
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
  
  public static final class ClassInstance extends hydra.ext.java.syntax.PrimaryNoNewArray {
    public final hydra.ext.java.syntax.ClassInstanceCreationExpression value;
    
    public ClassInstance (hydra.ext.java.syntax.ClassInstanceCreationExpression value) {
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
  
  public static final class FieldAccess extends hydra.ext.java.syntax.PrimaryNoNewArray {
    public final hydra.ext.java.syntax.FieldAccess value;
    
    public FieldAccess (hydra.ext.java.syntax.FieldAccess value) {
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
  
  public static final class ArrayAccess extends hydra.ext.java.syntax.PrimaryNoNewArray {
    public final hydra.ext.java.syntax.ArrayAccess value;
    
    public ArrayAccess (hydra.ext.java.syntax.ArrayAccess value) {
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
  
  public static final class MethodInvocation extends hydra.ext.java.syntax.PrimaryNoNewArray {
    public final hydra.ext.java.syntax.MethodInvocation value;
    
    public MethodInvocation (hydra.ext.java.syntax.MethodInvocation value) {
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
  
  public static final class MethodReference extends hydra.ext.java.syntax.PrimaryNoNewArray {
    public final hydra.ext.java.syntax.MethodReference value;
    
    public MethodReference (hydra.ext.java.syntax.MethodReference value) {
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