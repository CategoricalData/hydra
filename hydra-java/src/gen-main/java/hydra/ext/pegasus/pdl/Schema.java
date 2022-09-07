package hydra.ext.pegasus.pdl;

public abstract class Schema {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.Schema");
  
  private Schema () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Array instance) ;
    
    R visit(Fixed instance) ;
    
    R visit(Inline instance) ;
    
    R visit(Map instance) ;
    
    R visit(Named instance) ;
    
    R visit(Null instance) ;
    
    R visit(Primitive instance) ;
    
    R visit(Union instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Schema instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
    
    default R visit(Fixed instance) {
      return otherwise((instance));
    }
    
    default R visit(Inline instance) {
      return otherwise((instance));
    }
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
    
    default R visit(Named instance) {
      return otherwise((instance));
    }
    
    default R visit(Null instance) {
      return otherwise((instance));
    }
    
    default R visit(Primitive instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Array extends hydra.ext.pegasus.pdl.Schema {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.Array");
    
    public final hydra.ext.pegasus.pdl.Schema value;
    
    public Array (hydra.ext.pegasus.pdl.Schema value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) (other);
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
  
  public static final class Fixed extends hydra.ext.pegasus.pdl.Schema {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.Fixed");
    
    public final Integer value;
    
    public Fixed (Integer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fixed)) {
        return false;
      }
      Fixed o = (Fixed) (other);
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
  
  public static final class Inline extends hydra.ext.pegasus.pdl.Schema {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.Inline");
    
    public final hydra.ext.pegasus.pdl.NamedSchema value;
    
    public Inline (hydra.ext.pegasus.pdl.NamedSchema value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inline)) {
        return false;
      }
      Inline o = (Inline) (other);
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
  
  public static final class Map extends hydra.ext.pegasus.pdl.Schema {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.Map");
    
    public final hydra.ext.pegasus.pdl.Schema value;
    
    public Map (hydra.ext.pegasus.pdl.Schema value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) (other);
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
  
  public static final class Named extends hydra.ext.pegasus.pdl.Schema {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.Named");
    
    public final hydra.ext.pegasus.pdl.QualifiedName value;
    
    public Named (hydra.ext.pegasus.pdl.QualifiedName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Named)) {
        return false;
      }
      Named o = (Named) (other);
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
  
  public static final class Null extends hydra.ext.pegasus.pdl.Schema {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.Null");
    
    public Null () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Null)) {
        return false;
      }
      Null o = (Null) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Primitive extends hydra.ext.pegasus.pdl.Schema {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.Primitive");
    
    public final hydra.ext.pegasus.pdl.PrimitiveType value;
    
    public Primitive (hydra.ext.pegasus.pdl.PrimitiveType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) (other);
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
  
  public static final class Union extends hydra.ext.pegasus.pdl.Schema {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/pegasus/pdl.Union");
    
    public final hydra.ext.pegasus.pdl.UnionSchema value;
    
    public Union (hydra.ext.pegasus.pdl.UnionSchema value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) (other);
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