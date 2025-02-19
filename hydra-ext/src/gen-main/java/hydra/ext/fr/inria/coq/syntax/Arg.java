// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Arg implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Arg");
  
  public static final hydra.core.Name FIELD_NAME_IDENT = new hydra.core.Name("ident");
  
  public static final hydra.core.Name FIELD_NAME_NATURAL = new hydra.core.Name("natural");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  private Arg () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ident instance) ;
    
    R visit(Natural instance) ;
    
    R visit(Term instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Arg instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Ident instance) {
      return otherwise((instance));
    }
    
    default R visit(Natural instance) {
      return otherwise((instance));
    }
    
    default R visit(Term instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ident extends hydra.ext.fr.inria.coq.syntax.Arg implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.IdentArg value;
    
    public Ident (hydra.ext.fr.inria.coq.syntax.IdentArg value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ident)) {
        return false;
      }
      Ident o = (Ident) (other);
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
  
  public static final class Natural extends hydra.ext.fr.inria.coq.syntax.Arg implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.NaturalArg value;
    
    public Natural (hydra.ext.fr.inria.coq.syntax.NaturalArg value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Natural)) {
        return false;
      }
      Natural o = (Natural) (other);
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
  
  public static final class Term extends hydra.ext.fr.inria.coq.syntax.Arg implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Term1 value;
    
    public Term (hydra.ext.fr.inria.coq.syntax.Term1 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term)) {
        return false;
      }
      Term o = (Term) (other);
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