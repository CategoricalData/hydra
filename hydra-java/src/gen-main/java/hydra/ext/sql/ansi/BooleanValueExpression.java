package hydra.ext.sql.ansi;

public abstract class BooleanValueExpression {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.BooleanValueExpression");
  
  private BooleanValueExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Term instance) ;
    
    R visit(Or instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BooleanValueExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Term instance) {
      return otherwise((instance));
    }
    
    default R visit(Or instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Term extends hydra.ext.sql.ansi.BooleanValueExpression {
    public final hydra.ext.sql.ansi.BooleanTerm value;
    
    public Term (hydra.ext.sql.ansi.BooleanTerm value) {
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
  
  public static final class Or extends hydra.ext.sql.ansi.BooleanValueExpression {
    public final hydra.ext.sql.ansi.BooleanValueExpression_Or value;
    
    public Or (hydra.ext.sql.ansi.BooleanValueExpression_Or value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) (other);
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