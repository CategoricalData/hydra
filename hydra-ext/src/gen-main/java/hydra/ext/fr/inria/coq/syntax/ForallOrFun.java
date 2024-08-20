// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class ForallOrFun implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.ForallOrFun");
  
  public static final hydra.core.Name FIELD_NAME_FORALL = new hydra.core.Name("forall");
  
  public static final hydra.core.Name FIELD_NAME_FUN = new hydra.core.Name("fun");
  
  private ForallOrFun () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Forall instance) ;
    
    R visit(Fun instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ForallOrFun instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Forall instance) {
      return otherwise((instance));
    }
    
    default R visit(Fun instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Forall extends hydra.ext.fr.inria.coq.syntax.ForallOrFun implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Forall value;
    
    public Forall (hydra.ext.fr.inria.coq.syntax.Forall value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Forall)) {
        return false;
      }
      Forall o = (Forall) (other);
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
  
  public static final class Fun extends hydra.ext.fr.inria.coq.syntax.ForallOrFun implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Fun value;
    
    public Fun (hydra.ext.fr.inria.coq.syntax.Fun value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fun)) {
        return false;
      }
      Fun o = (Fun) (other);
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