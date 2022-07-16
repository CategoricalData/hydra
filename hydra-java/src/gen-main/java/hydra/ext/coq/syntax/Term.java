package hydra.ext.coq.syntax;

public abstract class Term {
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
  
  public static final class ForallOrFun extends Term {
    public final ForallOrFun value;
    
    public ForallOrFun (ForallOrFun value) {
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
  
  public static final class Let extends Term {
    public final Let value;
    
    public Let (Let value) {
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
  
  public static final class If extends Term {
    public final If value;
    
    public If (If value) {
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
  
  public static final class Fix extends Term {
    public final Fix value;
    
    public Fix (Fix value) {
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
  
  public static final class Cofix extends Term {
    public final Cofix value;
    
    public Cofix (Cofix value) {
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
  
  public static final class Term100 extends Term {
    public final Term100 value;
    
    public Term100 (Term100 value) {
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