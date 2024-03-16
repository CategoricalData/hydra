package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class RemoveItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RemoveItem");
  
  private RemoveItem () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(VariableLabels instance) ;
    
    R visit(Property instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RemoveItem instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(VariableLabels instance) {
      return otherwise((instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
  }
  
  public static final class VariableLabels extends hydra.langs.cypher.openCypher.RemoveItem implements Serializable {
    public final hydra.langs.cypher.openCypher.VariableAndNodeLabels value;
    
    public VariableLabels (hydra.langs.cypher.openCypher.VariableAndNodeLabels value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariableLabels)) {
        return false;
      }
      VariableLabels o = (VariableLabels) (other);
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
  
  public static final class Property extends hydra.langs.cypher.openCypher.RemoveItem implements Serializable {
    public final hydra.langs.cypher.openCypher.PropertyExpression value;
    
    public Property (hydra.langs.cypher.openCypher.PropertyExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
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