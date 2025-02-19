// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class AnonymousFunctionSignature implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AnonymousFunctionSignature");
  
  public static final hydra.core.Name FIELD_NAME_EXPLICIT = new hydra.core.Name("explicit");
  
  public static final hydra.core.Name FIELD_NAME_IMPLICIT = new hydra.core.Name("implicit");
  
  private AnonymousFunctionSignature () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Explicit instance) ;
    
    R visit(Implicit instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AnonymousFunctionSignature instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Explicit instance) {
      return otherwise((instance));
    }
    
    default R visit(Implicit instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Explicit extends hydra.ext.csharp.syntax.AnonymousFunctionSignature implements Serializable {
    public final java.util.List<hydra.ext.csharp.syntax.ExplicitAnonymousFunctionParameter> value;
    
    public Explicit (java.util.List<hydra.ext.csharp.syntax.ExplicitAnonymousFunctionParameter> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Explicit)) {
        return false;
      }
      Explicit o = (Explicit) (other);
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
  
  public static final class Implicit extends hydra.ext.csharp.syntax.AnonymousFunctionSignature implements Serializable {
    public final java.util.List<hydra.ext.csharp.syntax.Identifier> value;
    
    public Implicit (java.util.List<hydra.ext.csharp.syntax.Identifier> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Implicit)) {
        return false;
      }
      Implicit o = (Implicit) (other);
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