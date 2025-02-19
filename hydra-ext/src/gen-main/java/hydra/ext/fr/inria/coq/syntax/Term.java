// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Term implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.Term");
  
  public static final hydra.core.Name FIELD_NAME_FORALL_OR_FUN = new hydra.core.Name("forallOrFun");
  
  public static final hydra.core.Name FIELD_NAME_LET = new hydra.core.Name("let");
  
  public static final hydra.core.Name FIELD_NAME_IF = new hydra.core.Name("if");
  
  public static final hydra.core.Name FIELD_NAME_FIX = new hydra.core.Name("fix");
  
  public static final hydra.core.Name FIELD_NAME_COFIX = new hydra.core.Name("cofix");
  
  public static final hydra.core.Name FIELD_NAME_TERM100 = new hydra.core.Name("term100");
  
  private Term () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ForallOrFun instance) ;
    
    R visit(Let instance) ;
    
    R visit(If instance) ;
    
    R visit(Fix instance) ;
    
    R visit(Cofix instance) ;
    
    R visit(Term100 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ForallOrFun instance) {
      return otherwise((instance));
    }
    
    default R visit(Let instance) {
      return otherwise((instance));
    }
    
    default R visit(If instance) {
      return otherwise((instance));
    }
    
    default R visit(Fix instance) {
      return otherwise((instance));
    }
    
    default R visit(Cofix instance) {
      return otherwise((instance));
    }
    
    default R visit(Term100 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ForallOrFun extends hydra.ext.fr.inria.coq.syntax.Term implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.ForallOrFun value;
    
    public ForallOrFun (hydra.ext.fr.inria.coq.syntax.ForallOrFun value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ForallOrFun)) {
        return false;
      }
      ForallOrFun o = (ForallOrFun) (other);
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
  
  public static final class Let extends hydra.ext.fr.inria.coq.syntax.Term implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Let value;
    
    public Let (hydra.ext.fr.inria.coq.syntax.Let value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) (other);
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
  
  public static final class If extends hydra.ext.fr.inria.coq.syntax.Term implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.If value;
    
    public If (hydra.ext.fr.inria.coq.syntax.If value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof If)) {
        return false;
      }
      If o = (If) (other);
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
  
  public static final class Fix extends hydra.ext.fr.inria.coq.syntax.Term implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Fix value;
    
    public Fix (hydra.ext.fr.inria.coq.syntax.Fix value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fix)) {
        return false;
      }
      Fix o = (Fix) (other);
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
  
  public static final class Cofix extends hydra.ext.fr.inria.coq.syntax.Term implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Cofix value;
    
    public Cofix (hydra.ext.fr.inria.coq.syntax.Cofix value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cofix)) {
        return false;
      }
      Cofix o = (Cofix) (other);
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
  
  public static final class Term100 extends hydra.ext.fr.inria.coq.syntax.Term implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Term100 value;
    
    public Term100 (hydra.ext.fr.inria.coq.syntax.Term100 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term100)) {
        return false;
      }
      Term100 o = (Term100) (other);
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