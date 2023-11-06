package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class NonArithmeticOperatorInfix implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NonArithmeticOperatorInfix");
  
  private NonArithmeticOperatorInfix () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(List instance) ;
    
    R visit(Property instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NonArithmeticOperatorInfix instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
  }
  
  public static final class List extends hydra.langs.cypher.openCypher.NonArithmeticOperatorInfix implements Serializable {
    public final hydra.langs.cypher.openCypher.ListOperatorExpression value;
    
    public List (hydra.langs.cypher.openCypher.ListOperatorExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  public static final class Property extends hydra.langs.cypher.openCypher.NonArithmeticOperatorInfix implements Serializable {
    public final hydra.langs.cypher.openCypher.PropertyKeyName value;
    
    public Property (hydra.langs.cypher.openCypher.PropertyKeyName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
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