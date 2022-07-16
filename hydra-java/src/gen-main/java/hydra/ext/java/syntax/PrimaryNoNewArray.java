package hydra.ext.java.syntax;

public abstract class PrimaryNoNewArray {
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
  
  public static final class Literal extends PrimaryNoNewArray {
    public final Literal value;
    
    public Literal (Literal value) {
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
  
  public static final class ClassLiteral extends PrimaryNoNewArray {
    public final ClassLiteral value;
    
    public ClassLiteral (ClassLiteral value) {
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
  
  public static final class This extends PrimaryNoNewArray {
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
  
  public static final class DotThis extends PrimaryNoNewArray {
    public final TypeName value;
    
    public DotThis (TypeName value) {
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
  
  public static final class Parens extends PrimaryNoNewArray {
    public final Expression value;
    
    public Parens (Expression value) {
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
  
  public static final class ClassInstance extends PrimaryNoNewArray {
    public final ClassInstanceCreationExpression value;
    
    public ClassInstance (ClassInstanceCreationExpression value) {
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
  
  public static final class FieldAccess extends PrimaryNoNewArray {
    public final FieldAccess value;
    
    public FieldAccess (FieldAccess value) {
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
  
  public static final class ArrayAccess extends PrimaryNoNewArray {
    public final ArrayAccess value;
    
    public ArrayAccess (ArrayAccess value) {
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
  
  public static final class MethodInvocation extends PrimaryNoNewArray {
    public final MethodInvocation value;
    
    public MethodInvocation (MethodInvocation value) {
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
  
  public static final class MethodReference extends PrimaryNoNewArray {
    public final MethodReference value;
    
    public MethodReference (MethodReference value) {
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