// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ConstructorModifier implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ConstructorModifier");
  
  public static final hydra.core.Name FIELD_NAME_PUBLIC = new hydra.core.Name("public");
  
  public static final hydra.core.Name FIELD_NAME_PROTECTED = new hydra.core.Name("protected");
  
  public static final hydra.core.Name FIELD_NAME_INTERNAL = new hydra.core.Name("internal");
  
  public static final hydra.core.Name FIELD_NAME_PRIVATE = new hydra.core.Name("private");
  
  public static final hydra.core.Name FIELD_NAME_EXTERN = new hydra.core.Name("extern");
  
  public static final hydra.core.Name FIELD_NAME_UNSAFE = new hydra.core.Name("unsafe");
  
  private ConstructorModifier () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Public instance) ;
    
    R visit(Protected instance) ;
    
    R visit(Internal instance) ;
    
    R visit(Private instance) ;
    
    R visit(Extern instance) ;
    
    R visit(Unsafe instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ConstructorModifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Public instance) {
      return otherwise((instance));
    }
    
    default R visit(Protected instance) {
      return otherwise((instance));
    }
    
    default R visit(Internal instance) {
      return otherwise((instance));
    }
    
    default R visit(Private instance) {
      return otherwise((instance));
    }
    
    default R visit(Extern instance) {
      return otherwise((instance));
    }
    
    default R visit(Unsafe instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Public extends hydra.ext.csharp.syntax.ConstructorModifier implements Serializable {
    public Public () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Public)) {
        return false;
      }
      Public o = (Public) (other);
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
  
  public static final class Protected extends hydra.ext.csharp.syntax.ConstructorModifier implements Serializable {
    public Protected () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Protected)) {
        return false;
      }
      Protected o = (Protected) (other);
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
  
  public static final class Internal extends hydra.ext.csharp.syntax.ConstructorModifier implements Serializable {
    public Internal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Internal)) {
        return false;
      }
      Internal o = (Internal) (other);
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
  
  public static final class Private extends hydra.ext.csharp.syntax.ConstructorModifier implements Serializable {
    public Private () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Private)) {
        return false;
      }
      Private o = (Private) (other);
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
  
  public static final class Extern extends hydra.ext.csharp.syntax.ConstructorModifier implements Serializable {
    public Extern () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Extern)) {
        return false;
      }
      Extern o = (Extern) (other);
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
  
  public static final class Unsafe extends hydra.ext.csharp.syntax.ConstructorModifier implements Serializable {
    public Unsafe () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unsafe)) {
        return false;
      }
      Unsafe o = (Unsafe) (other);
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