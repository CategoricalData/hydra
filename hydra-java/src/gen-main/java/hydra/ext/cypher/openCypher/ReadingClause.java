// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class ReadingClause implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.ReadingClause");
  
  public static final hydra.core.Name FIELD_NAME_MATCH = new hydra.core.Name("match");
  
  public static final hydra.core.Name FIELD_NAME_UNWIND = new hydra.core.Name("unwind");
  
  public static final hydra.core.Name FIELD_NAME_IN_QUERY_CALL = new hydra.core.Name("inQueryCall");
  
  private ReadingClause () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Match instance) ;
    
    R visit(Unwind instance) ;
    
    R visit(InQueryCall instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ReadingClause instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Match instance) {
      return otherwise((instance));
    }
    
    default R visit(Unwind instance) {
      return otherwise((instance));
    }
    
    default R visit(InQueryCall instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Match extends hydra.ext.cypher.openCypher.ReadingClause implements Serializable {
    public final hydra.ext.cypher.openCypher.Match value;
    
    public Match (hydra.ext.cypher.openCypher.Match value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) (other);
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
  
  public static final class Unwind extends hydra.ext.cypher.openCypher.ReadingClause implements Serializable {
    public final hydra.ext.cypher.openCypher.Unwind value;
    
    public Unwind (hydra.ext.cypher.openCypher.Unwind value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unwind)) {
        return false;
      }
      Unwind o = (Unwind) (other);
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
  
  public static final class InQueryCall extends hydra.ext.cypher.openCypher.ReadingClause implements Serializable {
    public final hydra.ext.cypher.openCypher.InQueryCall value;
    
    public InQueryCall (hydra.ext.cypher.openCypher.InQueryCall value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InQueryCall)) {
        return false;
      }
      InQueryCall o = (InQueryCall) (other);
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