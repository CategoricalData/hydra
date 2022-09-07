package hydra.ext.java.syntax;

public abstract class MethodInvocation_Variant {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.MethodInvocation.Variant");
  
  private MethodInvocation_Variant () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Type instance) ;
    
    R visit(Expression instance) ;
    
    R visit(Primary instance) ;
    
    R visit(Super instance) ;
    
    R visit(TypeSuper instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MethodInvocation_Variant instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(Expression instance) {
      return otherwise((instance));
    }
    
    default R visit(Primary instance) {
      return otherwise((instance));
    }
    
    default R visit(Super instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeSuper instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Type extends hydra.ext.java.syntax.MethodInvocation_Variant {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Type");
    
    public final hydra.ext.java.syntax.TypeName value;
    
    public Type (hydra.ext.java.syntax.TypeName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
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
  
  public static final class Expression extends hydra.ext.java.syntax.MethodInvocation_Variant {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Expression");
    
    public final hydra.ext.java.syntax.ExpressionName value;
    
    public Expression (hydra.ext.java.syntax.ExpressionName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Expression)) {
        return false;
      }
      Expression o = (Expression) (other);
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
  
  public static final class Primary extends hydra.ext.java.syntax.MethodInvocation_Variant {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Primary");
    
    public final hydra.ext.java.syntax.Primary value;
    
    public Primary (hydra.ext.java.syntax.Primary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) (other);
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
  
  public static final class Super extends hydra.ext.java.syntax.MethodInvocation_Variant {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Super");
    
    public Super () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Super)) {
        return false;
      }
      Super o = (Super) (other);
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
  
  public static final class TypeSuper extends hydra.ext.java.syntax.MethodInvocation_Variant {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.TypeSuper");
    
    public final hydra.ext.java.syntax.TypeName value;
    
    public TypeSuper (hydra.ext.java.syntax.TypeName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeSuper)) {
        return false;
      }
      TypeSuper o = (TypeSuper) (other);
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