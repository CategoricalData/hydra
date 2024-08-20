// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class Directive implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.Directive");
  
  public static final hydra.core.Name FIELD_NAME_BASE_DECL = new hydra.core.Name("baseDecl");
  
  public static final hydra.core.Name FIELD_NAME_PREFIX_DECL = new hydra.core.Name("prefixDecl");
  
  private Directive () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(BaseDecl instance) ;
    
    R visit(PrefixDecl instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Directive instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(BaseDecl instance) {
      return otherwise((instance));
    }
    
    default R visit(PrefixDecl instance) {
      return otherwise((instance));
    }
  }
  
  public static final class BaseDecl extends hydra.ext.io.shex.syntax.Directive implements Serializable {
    public final hydra.ext.io.shex.syntax.BaseDecl value;
    
    public BaseDecl (hydra.ext.io.shex.syntax.BaseDecl value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BaseDecl)) {
        return false;
      }
      BaseDecl o = (BaseDecl) (other);
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
  
  public static final class PrefixDecl extends hydra.ext.io.shex.syntax.Directive implements Serializable {
    public final hydra.ext.io.shex.syntax.PrefixDecl value;
    
    public PrefixDecl (hydra.ext.io.shex.syntax.PrefixDecl value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrefixDecl)) {
        return false;
      }
      PrefixDecl o = (PrefixDecl) (other);
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