// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class RelationalOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RelationalOperator");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN = new hydra.core.Name("lessThan");
  
  public static final hydra.core.Name FIELD_NAME_GREATER_THAN = new hydra.core.Name("greaterThan");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN_OR_EQUAL = new hydra.core.Name("lessThanOrEqual");
  
  public static final hydra.core.Name FIELD_NAME_GREATER_THAN_OR_EQUAL = new hydra.core.Name("greaterThanOrEqual");
  
  private RelationalOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(LessThan instance) ;
    
    R visit(GreaterThan instance) ;
    
    R visit(LessThanOrEqual instance) ;
    
    R visit(GreaterThanOrEqual instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RelationalOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(LessThan instance) {
      return otherwise((instance));
    }
    
    default R visit(GreaterThan instance) {
      return otherwise((instance));
    }
    
    default R visit(LessThanOrEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(GreaterThanOrEqual instance) {
      return otherwise((instance));
    }
  }
  
  public static final class LessThan extends hydra.ext.csharp.syntax.RelationalOperator implements Serializable {
    public LessThan () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThan)) {
        return false;
      }
      LessThan o = (LessThan) (other);
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
  
  public static final class GreaterThan extends hydra.ext.csharp.syntax.RelationalOperator implements Serializable {
    public GreaterThan () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThan)) {
        return false;
      }
      GreaterThan o = (GreaterThan) (other);
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
  
  public static final class LessThanOrEqual extends hydra.ext.csharp.syntax.RelationalOperator implements Serializable {
    public LessThanOrEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThanOrEqual)) {
        return false;
      }
      LessThanOrEqual o = (LessThanOrEqual) (other);
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
  
  public static final class GreaterThanOrEqual extends hydra.ext.csharp.syntax.RelationalOperator implements Serializable {
    public GreaterThanOrEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThanOrEqual)) {
        return false;
      }
      GreaterThanOrEqual o = (GreaterThanOrEqual) (other);
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