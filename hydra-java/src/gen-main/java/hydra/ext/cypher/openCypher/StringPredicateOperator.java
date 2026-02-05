// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class StringPredicateOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.StringPredicateOperator");
  
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
    public final Boolean value;
    
    public StartsWith (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      return other instanceof StartsWith;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class EndsWith extends hydra.ext.cypher.openCypher.StringPredicateOperator implements Serializable {
    public final Boolean value;
    
    public EndsWith (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      return other instanceof EndsWith;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Contains extends hydra.ext.cypher.openCypher.StringPredicateOperator implements Serializable {
    public final Boolean value;
    
    public Contains (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      return other instanceof Contains;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
