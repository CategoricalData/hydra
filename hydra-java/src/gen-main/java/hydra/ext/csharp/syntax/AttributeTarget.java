// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class AttributeTarget implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AttributeTarget");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_KEYWORD = new hydra.core.Name("keyword");
  
  private AttributeTarget () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Identifier instance) ;
    
    R visit(Keyword instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AttributeTarget instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Identifier instance) {
      return otherwise((instance));
    }
    
    default R visit(Keyword instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Identifier extends hydra.ext.csharp.syntax.AttributeTarget implements Serializable {
    public final hydra.ext.csharp.syntax.Identifier value;
    
    public Identifier (hydra.ext.csharp.syntax.Identifier value) {
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
  
  public static final class Keyword extends hydra.ext.csharp.syntax.AttributeTarget implements Serializable {
    public final hydra.ext.csharp.syntax.Keyword value;
    
    public Keyword (hydra.ext.csharp.syntax.Keyword value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Keyword)) {
        return false;
      }
      Keyword o = (Keyword) (other);
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