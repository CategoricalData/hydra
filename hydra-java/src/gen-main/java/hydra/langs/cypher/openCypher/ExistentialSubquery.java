package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class ExistentialSubquery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ExistentialSubquery");
  
  private ExistentialSubquery () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Regular instance) ;
    
    R visit(Pattern instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExistentialSubquery instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Regular instance) {
      return otherwise((instance));
    }
    
    default R visit(Pattern instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Regular extends hydra.langs.cypher.openCypher.ExistentialSubquery implements Serializable {
    public final hydra.langs.cypher.openCypher.RegularQuery value;
    
    public Regular (hydra.langs.cypher.openCypher.RegularQuery value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Regular)) {
        return false;
      }
      Regular o = (Regular) (other);
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
  
  public static final class Pattern extends hydra.langs.cypher.openCypher.ExistentialSubquery implements Serializable {
    public final hydra.langs.cypher.openCypher.PatternWhere value;
    
    public Pattern (hydra.langs.cypher.openCypher.PatternWhere value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) (other);
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