package hydra.ext.datalog.syntax;

public abstract class Program_Elmt {
  private Program_Elmt () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Fact instance) ;
    
    R visit(Rule instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Program_Elmt instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Fact instance) {
      return otherwise((instance));
    }
    
    default R visit(Rule instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Fact extends hydra.ext.datalog.syntax.Program_Elmt {
    public final hydra.ext.datalog.syntax.Fact value;
    
    public Fact (hydra.ext.datalog.syntax.Fact value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fact)) {
        return false;
      }
      Fact o = (Fact) (other);
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
  
  public static final class Rule extends hydra.ext.datalog.syntax.Program_Elmt {
    public final hydra.ext.datalog.syntax.Rule value;
    
    public Rule (hydra.ext.datalog.syntax.Rule value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rule)) {
        return false;
      }
      Rule o = (Rule) (other);
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