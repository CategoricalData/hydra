// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Term100 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Term100");
  
  public static final hydra.core.Name FIELD_NAME_CAST = new hydra.core.Name("cast");
  
  public static final hydra.core.Name FIELD_NAME_TERM10 = new hydra.core.Name("term10");
  
  private Term100 () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Cast instance) ;
    
    R visit(Term10 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term100 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Cast instance) {
      return otherwise((instance));
    }
    
    default R visit(Term10 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Cast extends hydra.ext.fr.inria.coq.syntax.Term100 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.TypeCast value;
    
    public Cast (hydra.ext.fr.inria.coq.syntax.TypeCast value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cast)) {
        return false;
      }
      Cast o = (Cast) (other);
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
  
  public static final class Term10 extends hydra.ext.fr.inria.coq.syntax.Term100 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Term10 value;
    
    public Term10 (hydra.ext.fr.inria.coq.syntax.Term10 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term10)) {
        return false;
      }
      Term10 o = (Term10) (other);
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