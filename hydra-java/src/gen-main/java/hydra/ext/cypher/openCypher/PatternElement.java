// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class PatternElement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.PatternElement");
  
  public static final hydra.core.Name FIELD_NAME_CHAINED = new hydra.core.Name("chained");
  
  public static final hydra.core.Name FIELD_NAME_PARENTHESIZED = new hydra.core.Name("parenthesized");
  
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
  
  public static final class Chained extends hydra.ext.cypher.openCypher.PatternElement implements Serializable {
    public final hydra.ext.cypher.openCypher.NodePatternChain value;
    
    public Chained (hydra.ext.cypher.openCypher.NodePatternChain value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Parenthesized extends hydra.ext.cypher.openCypher.PatternElement implements Serializable {
    public final hydra.ext.cypher.openCypher.PatternElement value;
    
    public Parenthesized (hydra.ext.cypher.openCypher.PatternElement value) {
      java.util.Objects.requireNonNull((value));
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