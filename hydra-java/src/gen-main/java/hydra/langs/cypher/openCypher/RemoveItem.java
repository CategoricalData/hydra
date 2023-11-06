package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class RemoveItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RemoveItem");
  
  private RemoveItem () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(VariableNodeLabels instance) ;
    
    R visit(PropertyExpression instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RemoveItem instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(VariableNodeLabels instance) {
      return otherwise((instance));
    }
    
    default R visit(PropertyExpression instance) {
      return otherwise((instance));
    }
  }
  
  public static final class VariableNodeLabels extends hydra.langs.cypher.openCypher.RemoveItem implements Serializable {
    public final hydra.langs.cypher.openCypher.VariableNodeLabels value;
    
    public VariableNodeLabels (hydra.langs.cypher.openCypher.VariableNodeLabels value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariableNodeLabels)) {
        return false;
      }
      VariableNodeLabels o = (VariableNodeLabels) (other);
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
  
  public static final class PropertyExpression extends hydra.langs.cypher.openCypher.RemoveItem implements Serializable {
    public final hydra.langs.cypher.openCypher.PropertyExpression value;
    
    public PropertyExpression (hydra.langs.cypher.openCypher.PropertyExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyExpression)) {
        return false;
      }
      PropertyExpression o = (PropertyExpression) (other);
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