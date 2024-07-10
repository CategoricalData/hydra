// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class PatternElement implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PatternElement");
  
  private PatternElement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Chained instance) ;
    
    R visit(Parenthesized instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PatternElement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Chained instance) {
      return otherwise((instance));
    }
    
    default R visit(Parenthesized instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Chained extends hydra.langs.cypher.openCypher.PatternElement implements Serializable {
    public final hydra.langs.cypher.openCypher.NodePatternChain value;
    
    public Chained (hydra.langs.cypher.openCypher.NodePatternChain value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Chained)) {
        return false;
      }
      Chained o = (Chained) (other);
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
  
  public static final class Parenthesized extends hydra.langs.cypher.openCypher.PatternElement implements Serializable {
    public final hydra.langs.cypher.openCypher.PatternElement value;
    
    public Parenthesized (hydra.langs.cypher.openCypher.PatternElement value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parenthesized)) {
        return false;
      }
      Parenthesized o = (Parenthesized) (other);
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