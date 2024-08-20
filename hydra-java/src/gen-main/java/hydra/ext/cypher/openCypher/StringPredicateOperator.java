// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class StringPredicateOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.StringPredicateOperator");
  
  public static final hydra.core.Name FIELD_NAME_STARTS_WITH = new hydra.core.Name("startsWith");
  
  public static final hydra.core.Name FIELD_NAME_ENDS_WITH = new hydra.core.Name("endsWith");
  
  public static final hydra.core.Name FIELD_NAME_CONTAINS = new hydra.core.Name("contains");
  
  private StringPredicateOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(StartsWith instance) ;
    
    R visit(EndsWith instance) ;
    
    R visit(Contains instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringPredicateOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(StartsWith instance) {
      return otherwise((instance));
    }
    
    default R visit(EndsWith instance) {
      return otherwise((instance));
    }
    
    default R visit(Contains instance) {
      return otherwise((instance));
    }
  }
  
  public static final class StartsWith extends hydra.ext.cypher.openCypher.StringPredicateOperator implements Serializable {
    public StartsWith () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StartsWith)) {
        return false;
      }
      StartsWith o = (StartsWith) (other);
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
  
  public static final class EndsWith extends hydra.ext.cypher.openCypher.StringPredicateOperator implements Serializable {
    public EndsWith () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EndsWith)) {
        return false;
      }
      EndsWith o = (EndsWith) (other);
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
  
  public static final class Contains extends hydra.ext.cypher.openCypher.StringPredicateOperator implements Serializable {
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
