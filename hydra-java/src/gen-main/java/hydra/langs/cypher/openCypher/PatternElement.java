package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class PatternElement implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.PatternElement");
  
  private PatternElement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(NodePattern instance) ;
    
    R visit(Parens instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PatternElement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(NodePattern instance) {
      return otherwise((instance));
    }
    
    default R visit(Parens instance) {
      return otherwise((instance));
    }
  }
  
  public static final class NodePattern extends hydra.langs.cypher.openCypher.PatternElement implements Serializable {
    public final hydra.langs.cypher.openCypher.NodePatternExpression value;
    
    public NodePattern (hydra.langs.cypher.openCypher.NodePatternExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NodePattern)) {
        return false;
      }
      NodePattern o = (NodePattern) (other);
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
  
  public static final class Parens extends hydra.langs.cypher.openCypher.PatternElement implements Serializable {
    public final hydra.langs.cypher.openCypher.PatternElement value;
    
    public Parens (hydra.langs.cypher.openCypher.PatternElement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) (other);
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