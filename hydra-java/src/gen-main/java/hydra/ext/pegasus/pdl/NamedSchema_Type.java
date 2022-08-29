package hydra.ext.pegasus.pdl;

public abstract class NamedSchema_Type {
  private NamedSchema_Type () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Record instance) ;
    
    R visit(Enum_ instance) ;
    
    R visit(Typeref instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NamedSchema_Type instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Record instance) {
      return otherwise((instance));
    }
    
    default R visit(Enum_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Typeref instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Record extends hydra.ext.pegasus.pdl.NamedSchema_Type {
    public final hydra.ext.pegasus.pdl.RecordSchema value;
    
    public Record (hydra.ext.pegasus.pdl.RecordSchema value) {
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
  
  public static final class Enum_ extends hydra.ext.pegasus.pdl.NamedSchema_Type {
    public final hydra.ext.pegasus.pdl.EnumSchema value;
    
    public Enum_ (hydra.ext.pegasus.pdl.EnumSchema value) {
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
  
  public static final class Typeref extends hydra.ext.pegasus.pdl.NamedSchema_Type {
    public final hydra.ext.pegasus.pdl.Schema value;
    
    public Typeref (hydra.ext.pegasus.pdl.Schema value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Typeref)) {
        return false;
      }
      Typeref o = (Typeref) (other);
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