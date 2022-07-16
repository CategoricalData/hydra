package hydra.ext.haskell.ast;

/**
 * A data constructor
 */
public abstract class Constructor {
  private Constructor () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ordinary instance) ;
    
    R visit(Record instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Constructor instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Ordinary instance) {
      return otherwise((instance));
    }
    
    default R visit(Record instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ordinary extends Constructor {
    public final Constructor_Ordinary value;
    
    public Ordinary (Constructor_Ordinary value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ordinary)) {
        return false;
      }
      Ordinary o = (Ordinary) (other);
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
  
  public static final class Record extends Constructor {
    public final Constructor_Record value;
    
    public Record (Constructor_Record value) {
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