// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class UsingDirective implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.UsingDirective");
  
  public static final hydra.core.Name FIELD_NAME_ALIAS = new hydra.core.Name("alias");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACE = new hydra.core.Name("namespace");
  
  public static final hydra.core.Name FIELD_NAME_STATIC = new hydra.core.Name("static");
  
  private UsingDirective () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Alias instance) ;
    
    R visit(Namespace instance) ;
    
    R visit(Static instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UsingDirective instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Alias instance) {
      return otherwise((instance));
    }
    
    default R visit(Namespace instance) {
      return otherwise((instance));
    }
    
    default R visit(Static instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Alias extends hydra.ext.csharp.syntax.UsingDirective implements Serializable {
    public final hydra.ext.csharp.syntax.UsingAliasDirective value;
    
    public Alias (hydra.ext.csharp.syntax.UsingAliasDirective value) {
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
  
  public static final class Namespace extends hydra.ext.csharp.syntax.UsingDirective implements Serializable {
    public final hydra.ext.csharp.syntax.NamespaceName value;
    
    public Namespace (hydra.ext.csharp.syntax.NamespaceName value) {
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
  
  public static final class Static extends hydra.ext.csharp.syntax.UsingDirective implements Serializable {
    public final hydra.ext.csharp.syntax.TypeName value;
    
    public Static (hydra.ext.csharp.syntax.TypeName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Static)) {
        return false;
      }
      Static o = (Static) (other);
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