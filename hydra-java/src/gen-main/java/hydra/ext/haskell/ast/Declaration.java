package hydra.ext.haskell.ast;

/**
 * A data or value declaration
 */
public abstract class Declaration {
  private Declaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Data instance) ;
    
    R visit(Type instance) ;
    
    R visit(ValueBinding instance) ;
    
    R visit(TypedBinding instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Declaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Data instance) {
      return otherwise((instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(ValueBinding instance) {
      return otherwise((instance));
    }
    
    default R visit(TypedBinding instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Data extends Declaration {
    public final DataDeclaration value;
    
    public Data (DataDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Data)) {
        return false;
      }
      Data o = (Data) (other);
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
  
  public static final class Type extends Declaration {
    public final TypeDeclaration value;
    
    public Type (TypeDeclaration value) {
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
  
  public static final class ValueBinding extends Declaration {
    public final ValueBinding value;
    
    public ValueBinding (ValueBinding value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ValueBinding)) {
        return false;
      }
      ValueBinding o = (ValueBinding) (other);
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
  
  public static final class TypedBinding extends Declaration {
    public final TypedBinding value;
    
    public TypedBinding (TypedBinding value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypedBinding)) {
        return false;
      }
      TypedBinding o = (TypedBinding) (other);
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