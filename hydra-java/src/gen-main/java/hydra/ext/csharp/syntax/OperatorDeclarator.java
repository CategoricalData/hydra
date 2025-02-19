// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class OperatorDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.OperatorDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_UNARY = new hydra.core.Name("unary");
  
  public static final hydra.core.Name FIELD_NAME_BINARY = new hydra.core.Name("binary");
  
  public static final hydra.core.Name FIELD_NAME_CONVERSION = new hydra.core.Name("conversion");
  
  private OperatorDeclarator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Unary instance) ;
    
    R visit(Binary instance) ;
    
    R visit(Conversion instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OperatorDeclarator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Unary instance) {
      return otherwise((instance));
    }
    
    default R visit(Binary instance) {
      return otherwise((instance));
    }
    
    default R visit(Conversion instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Unary extends hydra.ext.csharp.syntax.OperatorDeclarator implements Serializable {
    public final hydra.ext.csharp.syntax.UnaryOperatorDeclarator value;
    
    public Unary (hydra.ext.csharp.syntax.UnaryOperatorDeclarator value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unary)) {
        return false;
      }
      Unary o = (Unary) (other);
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
  
  public static final class Binary extends hydra.ext.csharp.syntax.OperatorDeclarator implements Serializable {
    public final hydra.ext.csharp.syntax.BinaryOperatorDeclarator value;
    
    public Binary (hydra.ext.csharp.syntax.BinaryOperatorDeclarator value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) (other);
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
  
  public static final class Conversion extends hydra.ext.csharp.syntax.OperatorDeclarator implements Serializable {
    public final hydra.ext.csharp.syntax.ConversionOperatorDeclarator value;
    
    public Conversion (hydra.ext.csharp.syntax.ConversionOperatorDeclarator value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Conversion)) {
        return false;
      }
      Conversion o = (Conversion) (other);
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