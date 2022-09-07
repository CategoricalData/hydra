package hydra.ext.coq.syntax;

public abstract class Term {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Term");
  
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
  
  public static final class ForallOrFun extends hydra.ext.coq.syntax.Term {
    public final hydra.ext.coq.syntax.ForallOrFun value;
    
    public ForallOrFun (hydra.ext.coq.syntax.ForallOrFun value) {
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
  
  public static final class Let extends hydra.ext.coq.syntax.Term {
    public final hydra.ext.coq.syntax.Let value;
    
    public Let (hydra.ext.coq.syntax.Let value) {
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
  
  public static final class If extends hydra.ext.coq.syntax.Term {
    public final hydra.ext.coq.syntax.If value;
    
    public If (hydra.ext.coq.syntax.If value) {
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
  
  public static final class Fix extends hydra.ext.coq.syntax.Term {
    public final hydra.ext.coq.syntax.Fix value;
    
    public Fix (hydra.ext.coq.syntax.Fix value) {
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
  
  public static final class Cofix extends hydra.ext.coq.syntax.Term {
    public final hydra.ext.coq.syntax.Cofix value;
    
    public Cofix (hydra.ext.coq.syntax.Cofix value) {
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
  
  public static final class Term100 extends hydra.ext.coq.syntax.Term {
    public final hydra.ext.coq.syntax.Term100 value;
    
    public Term100 (hydra.ext.coq.syntax.Term100 value) {
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