package hydra.ext.avro.schema;

public abstract class NamedType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/avro/schema.NamedType");
  
  private NamedType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Reference instance) ;
    
    R visit(Enum_ instance) ;
    
    R visit(Fixed instance) ;
    
    R visit(Record instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NamedType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Reference instance) {
      return otherwise((instance));
    }
    
    default R visit(Enum_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Fixed instance) {
      return otherwise((instance));
    }
    
    default R visit(Record instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A reference to a previously defined type
   */
  public static final class Reference extends hydra.ext.avro.schema.NamedType {
    public Reference () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reference)) {
        return false;
      }
      Reference o = (Reference) (other);
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
  
  public static final class Enum_ extends hydra.ext.avro.schema.NamedType {
    public final hydra.ext.avro.schema.Enum_ value;
    
    public Enum_ (hydra.ext.avro.schema.Enum_ value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enum_)) {
        return false;
      }
      Enum_ o = (Enum_) (other);
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
  
  public static final class Fixed extends hydra.ext.avro.schema.NamedType {
    public final hydra.ext.avro.schema.Fixed value;
    
    public Fixed (hydra.ext.avro.schema.Fixed value) {
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
  
  public static final class Record extends hydra.ext.avro.schema.NamedType {
    public final hydra.ext.avro.schema.Record value;
    
    public Record (hydra.ext.avro.schema.Record value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) (other);
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