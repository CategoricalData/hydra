// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class TypeofExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.TypeofExpression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_UNBOUND_TYPE_NAME = new hydra.core.Name("unboundTypeName");
  
  public static final hydra.core.Name FIELD_NAME_VOID = new hydra.core.Name("void");
  
  private TypeofExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Type instance) ;
    
    R visit(UnboundTypeName instance) ;
    
    R visit(Void_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeofExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(UnboundTypeName instance) {
      return otherwise((instance));
    }
    
    default R visit(Void_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Type extends hydra.ext.csharp.syntax.TypeofExpression implements Serializable {
    public final hydra.ext.csharp.syntax.Type value;
    
    public Type (hydra.ext.csharp.syntax.Type value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class UnboundTypeName extends hydra.ext.csharp.syntax.TypeofExpression implements Serializable {
    public final hydra.ext.csharp.syntax.UnboundTypeName value;
    
    public UnboundTypeName (hydra.ext.csharp.syntax.UnboundTypeName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnboundTypeName)) {
        return false;
      }
      UnboundTypeName o = (UnboundTypeName) (other);
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
  
  public static final class Void_ extends hydra.ext.csharp.syntax.TypeofExpression implements Serializable {
    public Void_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Void_)) {
        return false;
      }
      Void_ o = (Void_) (other);
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
}