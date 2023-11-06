package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class SetItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.SetItem");
  
  private SetItem () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PropertyEquals instance) ;
    
    R visit(VariableEquals instance) ;
    
    R visit(VariablePlusEquals instance) ;
    
    R visit(VariableNodeLabels instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SetItem instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(PropertyEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(VariableEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(VariablePlusEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(VariableNodeLabels instance) {
      return otherwise((instance));
    }
  }
  
  public static final class PropertyEquals extends hydra.langs.cypher.openCypher.SetItem implements Serializable {
    public final hydra.langs.cypher.openCypher.PropertyEquals value;
    
    public PropertyEquals (hydra.langs.cypher.openCypher.PropertyEquals value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyEquals)) {
        return false;
      }
      PropertyEquals o = (PropertyEquals) (other);
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
  
  public static final class VariableEquals extends hydra.langs.cypher.openCypher.SetItem implements Serializable {
    public final hydra.langs.cypher.openCypher.VariableEquals value;
    
    public VariableEquals (hydra.langs.cypher.openCypher.VariableEquals value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariableEquals)) {
        return false;
      }
      VariableEquals o = (VariableEquals) (other);
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
  
  public static final class VariablePlusEquals extends hydra.langs.cypher.openCypher.SetItem implements Serializable {
    public final hydra.langs.cypher.openCypher.VariablePlusEquals value;
    
    public VariablePlusEquals (hydra.langs.cypher.openCypher.VariablePlusEquals value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariablePlusEquals)) {
        return false;
      }
      VariablePlusEquals o = (VariablePlusEquals) (other);
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
  
  public static final class VariableNodeLabels extends hydra.langs.cypher.openCypher.SetItem implements Serializable {
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
}