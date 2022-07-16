package hydra.ext.java.syntax;

public abstract class ForInit {
  private ForInit () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Statements instance) ;
    
    R visit(LocalVariable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ForInit instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Statements instance) {
      return otherwise((instance));
    }
    
    default R visit(LocalVariable instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Note: list cannot be empty
   */
  public static final class Statements extends ForInit {
    /**
     * Note: list cannot be empty
     */
    public final java.util.List<StatementExpression> value;
    
    public Statements (java.util.List<StatementExpression> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Statements)) {
        return false;
      }
      Statements o = (Statements) (other);
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
  
  public static final class LocalVariable extends ForInit {
    public final LocalVariableDeclaration value;
    
    public LocalVariable (LocalVariableDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocalVariable)) {
        return false;
      }
      LocalVariable o = (LocalVariable) (other);
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