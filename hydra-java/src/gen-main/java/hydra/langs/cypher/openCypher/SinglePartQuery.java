package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class SinglePartQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.SinglePartQuery");
  
  private SinglePartQuery () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Reading instance) ;
    
    R visit(Return instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SinglePartQuery instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Reading instance) {
      return otherwise((instance));
    }
    
    default R visit(Return instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Reading extends hydra.langs.cypher.openCypher.SinglePartQuery implements Serializable {
    public final hydra.langs.cypher.openCypher.ReadingQuery value;
    
    public Reading (hydra.langs.cypher.openCypher.ReadingQuery value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reading)) {
        return false;
      }
      Reading o = (Reading) (other);
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
  
  public static final class Return extends hydra.langs.cypher.openCypher.SinglePartQuery implements Serializable {
    public final hydra.langs.cypher.openCypher.UpdatingQuery value;
    
    public Return (hydra.langs.cypher.openCypher.UpdatingQuery value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Return)) {
        return false;
      }
      Return o = (Return) (other);
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