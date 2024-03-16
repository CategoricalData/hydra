package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class CreateOrMatch implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.CreateOrMatch");
  
  private CreateOrMatch () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Create instance) ;
    
    R visit(Match instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CreateOrMatch instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Create instance) {
      return otherwise((instance));
    }
    
    default R visit(Match instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Create extends hydra.langs.cypher.openCypher.CreateOrMatch implements Serializable {
    public Create () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Create)) {
        return false;
      }
      Create o = (Create) (other);
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
  
  public static final class Match extends hydra.langs.cypher.openCypher.CreateOrMatch implements Serializable {
    public Match () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) (other);
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