// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class AccessorModifier implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AccessorModifier");
  
  public static final hydra.core.Name FIELD_NAME_PROTECTED = new hydra.core.Name("protected");
  
  public static final hydra.core.Name FIELD_NAME_INTERNAL = new hydra.core.Name("internal");
  
  public static final hydra.core.Name FIELD_NAME_PRIVATE = new hydra.core.Name("private");
  
  public static final hydra.core.Name FIELD_NAME_PROTECTED_INTERNAL = new hydra.core.Name("protectedInternal");
  
  public static final hydra.core.Name FIELD_NAME_INTERNAL_PROTECTED = new hydra.core.Name("internalProtected");
  
  public static final hydra.core.Name FIELD_NAME_PROTECTED_PRIVATE = new hydra.core.Name("protectedPrivate");
  
  public static final hydra.core.Name FIELD_NAME_PRIVATE_PROTECTED = new hydra.core.Name("privateProtected");
  
  private AccessorModifier () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Protected instance) ;
    
    R visit(Internal instance) ;
    
    R visit(Private instance) ;
    
    R visit(ProtectedInternal instance) ;
    
    R visit(InternalProtected instance) ;
    
    R visit(ProtectedPrivate instance) ;
    
    R visit(PrivateProtected instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AccessorModifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
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
    
    default R visit(ProtectedInternal instance) {
      return otherwise((instance));
    }
    
    default R visit(InternalProtected instance) {
      return otherwise((instance));
    }
    
    default R visit(ProtectedPrivate instance) {
      return otherwise((instance));
    }
    
    default R visit(PrivateProtected instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Protected extends hydra.ext.csharp.syntax.AccessorModifier implements Serializable {
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
  
  public static final class Internal extends hydra.ext.csharp.syntax.AccessorModifier implements Serializable {
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
  
  public static final class Private extends hydra.ext.csharp.syntax.AccessorModifier implements Serializable {
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
  
  public static final class ProtectedInternal extends hydra.ext.csharp.syntax.AccessorModifier implements Serializable {
    public ProtectedInternal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ProtectedInternal)) {
        return false;
      }
      ProtectedInternal o = (ProtectedInternal) (other);
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
  
  public static final class InternalProtected extends hydra.ext.csharp.syntax.AccessorModifier implements Serializable {
    public InternalProtected () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InternalProtected)) {
        return false;
      }
      InternalProtected o = (InternalProtected) (other);
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
  
  public static final class ProtectedPrivate extends hydra.ext.csharp.syntax.AccessorModifier implements Serializable {
    public ProtectedPrivate () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ProtectedPrivate)) {
        return false;
      }
      ProtectedPrivate o = (ProtectedPrivate) (other);
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
  
  public static final class PrivateProtected extends hydra.ext.csharp.syntax.AccessorModifier implements Serializable {
    public PrivateProtected () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrivateProtected)) {
        return false;
      }
      PrivateProtected o = (PrivateProtected) (other);
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