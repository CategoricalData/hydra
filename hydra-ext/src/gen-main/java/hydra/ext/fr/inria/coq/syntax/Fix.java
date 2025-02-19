// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Fix implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Fix");
  
  public static final hydra.core.Name FIELD_NAME_DECL = new hydra.core.Name("decl");
  
  public static final hydra.core.Name FIELD_NAME_QUAL = new hydra.core.Name("qual");
  
  private Fix () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Decl instance) ;
    
    R visit(Qual instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Fix instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Decl instance) {
      return otherwise((instance));
    }
    
    default R visit(Qual instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Decl extends hydra.ext.fr.inria.coq.syntax.Fix implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Fix_Decl value;
    
    public Decl (hydra.ext.fr.inria.coq.syntax.Fix_Decl value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decl)) {
        return false;
      }
      Decl o = (Decl) (other);
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
  
  public static final class Qual extends hydra.ext.fr.inria.coq.syntax.Fix implements Serializable {
    public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Fix_Qual> value;
    
    public Qual (hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Fix_Qual> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qual)) {
        return false;
      }
      Qual o = (Qual) (other);
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