// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public abstract class OperationType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.OperationType");
  
  public static final hydra.core.Name FIELD_NAME_QUERY = new hydra.core.Name("query");
  
  public static final hydra.core.Name FIELD_NAME_MUTATION = new hydra.core.Name("mutation");
  
  public static final hydra.core.Name FIELD_NAME_SUBSCRIPTION = new hydra.core.Name("subscription");
  
  private OperationType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Query instance) ;
    
    R visit(Mutation instance) ;
    
    R visit(Subscription instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OperationType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Query instance) {
      return otherwise((instance));
    }
    
    default R visit(Mutation instance) {
      return otherwise((instance));
    }
    
    default R visit(Subscription instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Query extends hydra.ext.org.graphql.syntax.OperationType implements Serializable {
    public Query () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Query)) {
        return false;
      }
      Query o = (Query) (other);
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
  
  public static final class Mutation extends hydra.ext.org.graphql.syntax.OperationType implements Serializable {
    public Mutation () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mutation)) {
        return false;
      }
      Mutation o = (Mutation) (other);
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
  
  public static final class Subscription extends hydra.ext.org.graphql.syntax.OperationType implements Serializable {
    public Subscription () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subscription)) {
        return false;
      }
      Subscription o = (Subscription) (other);
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