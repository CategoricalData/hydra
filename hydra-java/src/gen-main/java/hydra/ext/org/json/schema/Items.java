// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public abstract class Items implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.Items");
  
  public static final hydra.core.Name FIELD_NAME_SAME_ITEMS = new hydra.core.Name("sameItems");
  
  public static final hydra.core.Name FIELD_NAME_VAR_ITEMS = new hydra.core.Name("varItems");
  
  private Items () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(SameItems instance) ;
    
    R visit(VarItems instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Items instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(SameItems instance) {
      return otherwise((instance));
    }
    
    default R visit(VarItems instance) {
      return otherwise((instance));
    }
  }
  
  public static final class SameItems extends hydra.ext.org.json.schema.Items implements Serializable {
    public final hydra.ext.org.json.schema.Schema value;
    
    public SameItems (hydra.ext.org.json.schema.Schema value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SameItems)) {
        return false;
      }
      SameItems o = (SameItems) (other);
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
  
  public static final class VarItems extends hydra.ext.org.json.schema.Items implements Serializable {
    public final java.util.List<hydra.ext.org.json.schema.Schema> value;
    
    public VarItems (java.util.List<hydra.ext.org.json.schema.Schema> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VarItems)) {
        return false;
      }
      VarItems o = (VarItems) (other);
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