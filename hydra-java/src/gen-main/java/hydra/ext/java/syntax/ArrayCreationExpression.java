// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ArrayCreationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVE = new hydra.core.Name("primitive");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_OR_INTERFACE = new hydra.core.Name("classOrInterface");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVE_ARRAY = new hydra.core.Name("primitiveArray");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_OR_INTERFACE_ARRAY = new hydra.core.Name("classOrInterfaceArray");
  
  private ArrayCreationExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Primitive instance) ;
    
    R visit(ClassOrInterface instance) ;
    
    R visit(PrimitiveArray instance) ;
    
    R visit(ClassOrInterfaceArray instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ArrayCreationExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Primitive instance) {
      return otherwise((instance));
    }
    
    default R visit(ClassOrInterface instance) {
      return otherwise((instance));
    }
    
    default R visit(PrimitiveArray instance) {
      return otherwise((instance));
    }
    
    default R visit(ClassOrInterfaceArray instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Primitive extends hydra.ext.java.syntax.ArrayCreationExpression implements Serializable {
    public final hydra.ext.java.syntax.ArrayCreationExpression_Primitive value;
    
    public Primitive (hydra.ext.java.syntax.ArrayCreationExpression_Primitive value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) (other);
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
  
  public static final class ClassOrInterface extends hydra.ext.java.syntax.ArrayCreationExpression implements Serializable {
    public final hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface value;
    
    public ClassOrInterface (hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassOrInterface)) {
        return false;
      }
      ClassOrInterface o = (ClassOrInterface) (other);
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
  
  public static final class PrimitiveArray extends hydra.ext.java.syntax.ArrayCreationExpression implements Serializable {
    public final hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray value;
    
    public PrimitiveArray (hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimitiveArray)) {
        return false;
      }
      PrimitiveArray o = (PrimitiveArray) (other);
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
  
  public static final class ClassOrInterfaceArray extends hydra.ext.java.syntax.ArrayCreationExpression implements Serializable {
    public final hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray value;
    
    public ClassOrInterfaceArray (hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassOrInterfaceArray)) {
        return false;
      }
      ClassOrInterfaceArray o = (ClassOrInterfaceArray) (other);
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