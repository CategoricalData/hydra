// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class NamespaceMemberDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NamespaceMemberDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACE = new hydra.core.Name("namespace");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  private NamespaceMemberDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Namespace instance) ;
    
    R visit(Type instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NamespaceMemberDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Namespace instance) {
      return otherwise((instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Namespace extends hydra.ext.csharp.syntax.NamespaceMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.NamespaceDeclaration value;
    
    public Namespace (hydra.ext.csharp.syntax.NamespaceDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Namespace)) {
        return false;
      }
      Namespace o = (Namespace) (other);
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
  
  public static final class Type extends hydra.ext.csharp.syntax.NamespaceMemberDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.TypeDeclaration value;
    
    public Type (hydra.ext.csharp.syntax.TypeDeclaration value) {
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
}