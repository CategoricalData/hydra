// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TransactionPart implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TransactionPart");
  
  public static final hydra.core.Name FIELD_NAME_BEGIN = new hydra.core.Name("begin");
  
  public static final hydra.core.Name FIELD_NAME_COMMIT = new hydra.core.Name("commit");
  
  public static final hydra.core.Name FIELD_NAME_ROLLBACK = new hydra.core.Name("rollback");
  
  private TransactionPart () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Begin instance) ;
    
    R visit(Commit instance) ;
    
    R visit(Rollback instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TransactionPart instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Begin instance) {
      return otherwise((instance));
    }
    
    default R visit(Commit instance) {
      return otherwise((instance));
    }
    
    default R visit(Rollback instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Begin extends hydra.langs.tinkerpop.gremlin.TransactionPart implements Serializable {
    public Begin () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Begin)) {
        return false;
      }
      Begin o = (Begin) (other);
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
  
  public static final class Commit extends hydra.langs.tinkerpop.gremlin.TransactionPart implements Serializable {
    public Commit () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Commit)) {
        return false;
      }
      Commit o = (Commit) (other);
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
  
  public static final class Rollback extends hydra.langs.tinkerpop.gremlin.TransactionPart implements Serializable {
    public Rollback () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rollback)) {
        return false;
      }
      Rollback o = (Rollback) (other);
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