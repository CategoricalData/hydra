// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Pattern10 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Pattern10");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public static final hydra.core.Name FIELD_NAME_PATTERNS = new hydra.core.Name("patterns");
  
  public static final hydra.core.Name FIELD_NAME_QUALIID = new hydra.core.Name("qualiid");
  
  private Pattern10 () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(As instance) ;
    
    R visit(Patterns instance) ;
    
    R visit(Qualiid instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern10 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(As instance) {
      return otherwise((instance));
    }
    
    default R visit(Patterns instance) {
      return otherwise((instance));
    }
    
    default R visit(Qualiid instance) {
      return otherwise((instance));
    }
  }
  
  public static final class As extends hydra.ext.fr.inria.coq.syntax.Pattern10 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Pattern10_As value;
    
    public As (hydra.ext.fr.inria.coq.syntax.Pattern10_As value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof As)) {
        return false;
      }
      As o = (As) (other);
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
  
  public static final class Patterns extends hydra.ext.fr.inria.coq.syntax.Pattern10 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Pattern10_Patterns value;
    
    public Patterns (hydra.ext.fr.inria.coq.syntax.Pattern10_Patterns value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Patterns)) {
        return false;
      }
      Patterns o = (Patterns) (other);
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
  
  public static final class Qualiid extends hydra.ext.fr.inria.coq.syntax.Pattern10 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Pattern10_Qualid value;
    
    public Qualiid (hydra.ext.fr.inria.coq.syntax.Pattern10_Qualid value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qualiid)) {
        return false;
      }
      Qualiid o = (Qualiid) (other);
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