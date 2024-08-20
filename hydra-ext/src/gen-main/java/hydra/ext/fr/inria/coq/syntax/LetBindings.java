// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class LetBindings implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.LetBindings");
  
  public static final hydra.core.Name FIELD_NAME_NAMED = new hydra.core.Name("named");
  
  public static final hydra.core.Name FIELD_NAME_DESTRUCTURING = new hydra.core.Name("destructuring");
  
  private LetBindings () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Named instance) ;
    
    R visit(Destructuring instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LetBindings instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Named instance) {
      return otherwise((instance));
    }
    
    default R visit(Destructuring instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Named extends hydra.ext.fr.inria.coq.syntax.LetBindings implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.LetNamed value;
    
    public Named (hydra.ext.fr.inria.coq.syntax.LetNamed value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Named)) {
        return false;
      }
      Named o = (Named) (other);
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
  
  public static final class Destructuring extends hydra.ext.fr.inria.coq.syntax.LetBindings implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.LetDestructuring value;
    
    public Destructuring (hydra.ext.fr.inria.coq.syntax.LetDestructuring value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Destructuring)) {
        return false;
      }
      Destructuring o = (Destructuring) (other);
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