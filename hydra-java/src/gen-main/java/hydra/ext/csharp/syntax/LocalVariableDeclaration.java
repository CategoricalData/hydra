// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class LocalVariableDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.LocalVariableDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_IMPLICITLY_TYPED = new hydra.core.Name("implicitlyTyped");
  
  public static final hydra.core.Name FIELD_NAME_EXPLICITLY_TYPED = new hydra.core.Name("explicitlyTyped");
  
  public static final hydra.core.Name FIELD_NAME_REF = new hydra.core.Name("ref");
  
  private LocalVariableDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ImplicitlyTyped instance) ;
    
    R visit(ExplicitlyTyped instance) ;
    
    R visit(Ref instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LocalVariableDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ImplicitlyTyped instance) {
      return otherwise((instance));
    }
    
    default R visit(ExplicitlyTyped instance) {
      return otherwise((instance));
    }
    
    default R visit(Ref instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ImplicitlyTyped extends hydra.ext.csharp.syntax.LocalVariableDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.ImplicitlyTypedLocalVariableDeclaration value;
    
    public ImplicitlyTyped (hydra.ext.csharp.syntax.ImplicitlyTypedLocalVariableDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImplicitlyTyped)) {
        return false;
      }
      ImplicitlyTyped o = (ImplicitlyTyped) (other);
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
  
  public static final class ExplicitlyTyped extends hydra.ext.csharp.syntax.LocalVariableDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.ExplicitlyTypedLocalVariableDeclaration value;
    
    public ExplicitlyTyped (hydra.ext.csharp.syntax.ExplicitlyTypedLocalVariableDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExplicitlyTyped)) {
        return false;
      }
      ExplicitlyTyped o = (ExplicitlyTyped) (other);
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
  
  public static final class Ref extends hydra.ext.csharp.syntax.LocalVariableDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.RefLocalVariableDeclaration value;
    
    public Ref (hydra.ext.csharp.syntax.RefLocalVariableDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ref)) {
        return false;
      }
      Ref o = (Ref) (other);
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