package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public abstract class BinaryBooleanOperator implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.BinaryBooleanOperator");
  
  private BinaryBooleanOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(And instance) ;
    
    R visit(Or instance) ;
    
    R visit(Xor instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BinaryBooleanOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(And instance) {
      return otherwise((instance));
    }
    
    default R visit(Or instance) {
      return otherwise((instance));
    }
    
    default R visit(Xor instance) {
      return otherwise((instance));
    }
  }
  
  public static final class And extends hydra.langs.tinkerpop.queries.BinaryBooleanOperator implements Serializable {
    public And () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) (other);
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
  
  public static final class Or extends hydra.langs.tinkerpop.queries.BinaryBooleanOperator implements Serializable {
    public Or () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) (other);
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
  
  public static final class Xor extends hydra.langs.tinkerpop.queries.BinaryBooleanOperator implements Serializable {
    public Xor () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Xor)) {
        return false;
      }
      Xor o = (Xor) (other);
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