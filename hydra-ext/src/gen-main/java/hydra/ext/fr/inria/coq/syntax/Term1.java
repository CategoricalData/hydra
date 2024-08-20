// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Term1 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Term1");
  
  public static final hydra.core.Name FIELD_NAME_PROJECTION = new hydra.core.Name("projection");
  
  public static final hydra.core.Name FIELD_NAME_SCOPE = new hydra.core.Name("scope");
  
  public static final hydra.core.Name FIELD_NAME_TERM0 = new hydra.core.Name("term0");
  
  private Term1 () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Projection instance) ;
    
    R visit(Scope instance) ;
    
    R visit(Term0 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term1 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Projection instance) {
      return otherwise((instance));
    }
    
    default R visit(Scope instance) {
      return otherwise((instance));
    }
    
    default R visit(Term0 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Projection extends hydra.ext.fr.inria.coq.syntax.Term1 implements Serializable {
    public Projection () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Projection)) {
        return false;
      }
      Projection o = (Projection) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Scope extends hydra.ext.fr.inria.coq.syntax.Term1 implements Serializable {
    public Scope () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Scope)) {
        return false;
      }
      Scope o = (Scope) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Term0 extends hydra.ext.fr.inria.coq.syntax.Term1 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Term0 value;
    
    public Term0 (hydra.ext.fr.inria.coq.syntax.Term0 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term0)) {
        return false;
      }
      Term0 o = (Term0) (other);
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