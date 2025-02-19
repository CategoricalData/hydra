// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class NamespaceOrTypeName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NamespaceOrTypeName");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIED = new hydra.core.Name("qualified");
  
  public static final hydra.core.Name FIELD_NAME_ALIAS = new hydra.core.Name("alias");
  
  private NamespaceOrTypeName () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Identifier instance) ;
    
    R visit(Qualified instance) ;
    
    R visit(Alias instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NamespaceOrTypeName instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Identifier instance) {
      return otherwise((instance));
    }
    
    default R visit(Qualified instance) {
      return otherwise((instance));
    }
    
    default R visit(Alias instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Identifier extends hydra.ext.csharp.syntax.NamespaceOrTypeName implements Serializable {
    public final hydra.ext.csharp.syntax.IdentifierNamespaceOrTypeName value;
    
    public Identifier (hydra.ext.csharp.syntax.IdentifierNamespaceOrTypeName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Identifier)) {
        return false;
      }
      Identifier o = (Identifier) (other);
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
  
  public static final class Qualified extends hydra.ext.csharp.syntax.NamespaceOrTypeName implements Serializable {
    public final hydra.ext.csharp.syntax.QualifiedNamespaceOrTypeName value;
    
    public Qualified (hydra.ext.csharp.syntax.QualifiedNamespaceOrTypeName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qualified)) {
        return false;
      }
      Qualified o = (Qualified) (other);
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
  
  public static final class Alias extends hydra.ext.csharp.syntax.NamespaceOrTypeName implements Serializable {
    public final hydra.ext.csharp.syntax.QualifiedAliasMember value;
    
    public Alias (hydra.ext.csharp.syntax.QualifiedAliasMember value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Alias)) {
        return false;
      }
      Alias o = (Alias) (other);
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