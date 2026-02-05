// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class MatchOrCreate implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.MatchOrCreate");
  
  public static final hydra.core.Name FIELD_NAME_MATCH = new hydra.core.Name("match");
  
  public static final hydra.core.Name FIELD_NAME_CREATE = new hydra.core.Name("create");
  
  private MatchOrCreate () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Match instance) ;
    
    R visit(Create instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MatchOrCreate instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Match instance) {
      return otherwise((instance));
    }
    
    default R visit(Create instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Match extends hydra.ext.cypher.openCypher.MatchOrCreate implements Serializable {
    public final Boolean value;
    
    public Match (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      return other instanceof Match;
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
  
  public static final class Create extends hydra.ext.cypher.openCypher.MatchOrCreate implements Serializable {
    public final Boolean value;
    
    public Create (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      return other instanceof Create;
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
