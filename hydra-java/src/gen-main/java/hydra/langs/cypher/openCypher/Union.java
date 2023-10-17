package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class Union implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Union");
  
  private Union () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(All instance) ;
    
    R visit(Distinct instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Union instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(All instance) {
      return otherwise((instance));
    }
    
    default R visit(Distinct instance) {
      return otherwise((instance));
    }
  }
  
  public static final class All extends hydra.langs.cypher.openCypher.Union implements Serializable {
    public final hydra.langs.cypher.openCypher.SingleQuery value;
    
    public All (hydra.langs.cypher.openCypher.SingleQuery value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) (other);
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
  
  public static final class Distinct extends hydra.langs.cypher.openCypher.Union implements Serializable {
    public final hydra.langs.cypher.openCypher.SingleQuery value;
    
    public Distinct (hydra.langs.cypher.openCypher.SingleQuery value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Distinct)) {
        return false;
      }
      Distinct o = (Distinct) (other);
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