package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class StringPredicateOperator implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StringPredicateOperator");
  
  private StringPredicateOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Starts instance) ;
    
    R visit(Ends instance) ;
    
    R visit(Contains instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringPredicateOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Starts instance) {
      return otherwise((instance));
    }
    
    default R visit(Ends instance) {
      return otherwise((instance));
    }
    
    default R visit(Contains instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Starts extends hydra.langs.cypher.openCypher.StringPredicateOperator implements Serializable {
    public Starts () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Starts)) {
        return false;
      }
      Starts o = (Starts) (other);
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
  
  public static final class Ends extends hydra.langs.cypher.openCypher.StringPredicateOperator implements Serializable {
    public Ends () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ends)) {
        return false;
      }
      Ends o = (Ends) (other);
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
  
  public static final class Contains extends hydra.langs.cypher.openCypher.StringPredicateOperator implements Serializable {
    public Contains () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Contains)) {
        return false;
      }
      Contains o = (Contains) (other);
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
}