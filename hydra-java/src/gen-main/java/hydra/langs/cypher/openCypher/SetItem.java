// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class SetItem implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.SetItem");
  
  private SetItem () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Property instance) ;
    
    R visit(VariableEqual instance) ;
    
    R visit(VariablePlusEqual instance) ;
    
    R visit(VariableLabels instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SetItem instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
    
    default R visit(VariableEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(VariablePlusEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(VariableLabels instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Property extends hydra.langs.cypher.openCypher.SetItem implements Serializable {
    public final hydra.langs.cypher.openCypher.PropertyEquals value;
    
    public Property (hydra.langs.cypher.openCypher.PropertyEquals value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
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
  
  public static final class VariableEqual extends hydra.langs.cypher.openCypher.SetItem implements Serializable {
    public final hydra.langs.cypher.openCypher.VariableEquals value;
    
    public VariableEqual (hydra.langs.cypher.openCypher.VariableEquals value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariableEqual)) {
        return false;
      }
      VariableEqual o = (VariableEqual) (other);
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
  
  public static final class VariablePlusEqual extends hydra.langs.cypher.openCypher.SetItem implements Serializable {
    public final hydra.langs.cypher.openCypher.VariablePlusEquals value;
    
    public VariablePlusEqual (hydra.langs.cypher.openCypher.VariablePlusEquals value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VariablePlusEqual)) {
        return false;
      }
      VariablePlusEqual o = (VariablePlusEqual) (other);
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
  
  public static final class VariableLabels extends hydra.langs.cypher.openCypher.SetItem implements Serializable {
    public final hydra.langs.cypher.openCypher.VariableAndNodeLabels value;
    
    public VariableLabels (hydra.langs.cypher.openCypher.VariableAndNodeLabels value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
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
}