// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A data or value declaration
 */
public abstract class Declaration implements Serializable, Comparable<Declaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.Declaration");
  
  public static final hydra.core.Name FIELD_NAME_DATA = new hydra.core.Name("data");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_VALUE_BINDING = new hydra.core.Name("valueBinding");
  
  public static final hydra.core.Name FIELD_NAME_TYPED_BINDING = new hydra.core.Name("typedBinding");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Data instance) {
      return otherwise(instance);
    }
    
    default R visit(Type instance) {
      return otherwise(instance);
    }
    
    default R visit(ValueBinding instance) {
      return otherwise(instance);
    }
    
    default R visit(TypedBinding instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * A data type declaration
   */
  public static final class Data extends hydra.ext.haskell.ast.Declaration implements Serializable {
    public final hydra.ext.haskell.ast.DataDeclaration value;
    
    public Data (hydra.ext.haskell.ast.DataDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Data)) {
        return false;
      }
      Data o = (Data) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Declaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Data o = (Data) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A type synonym declaration
   */
  public static final class Type extends hydra.ext.haskell.ast.Declaration implements Serializable {
    public final hydra.ext.haskell.ast.TypeDeclaration value;
    
    public Type (hydra.ext.haskell.ast.TypeDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Declaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Type o = (Type) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A value binding
   */
  public static final class ValueBinding extends hydra.ext.haskell.ast.Declaration implements Serializable {
    public final hydra.ext.haskell.ast.ValueBinding value;
    
    public ValueBinding (hydra.ext.haskell.ast.ValueBinding value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ValueBinding)) {
        return false;
      }
      ValueBinding o = (ValueBinding) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Declaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ValueBinding o = (ValueBinding) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A typed binding
   */
  public static final class TypedBinding extends hydra.ext.haskell.ast.Declaration implements Serializable {
    public final hydra.ext.haskell.ast.TypedBinding value;
    
    public TypedBinding (hydra.ext.haskell.ast.TypedBinding value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypedBinding)) {
        return false;
      }
      TypedBinding o = (TypedBinding) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Declaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypedBinding o = (TypedBinding) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
