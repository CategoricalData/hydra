package hydra.ext.protobuf.type;

/**
 * The syntax in which a protocol buffer element is defined.
 */
public abstract class Syntax {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/protobuf/type.Syntax");
  
  private Syntax () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Proto2 instance) ;
    
    R visit(Proto3 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Syntax instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Proto2 instance) {
      return otherwise((instance));
    }
    
    default R visit(Proto3 instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Syntax `proto2`.
   */
  public static final class Proto2 extends hydra.ext.protobuf.type.Syntax {
    public Proto2 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Proto2)) {
        return false;
      }
      Proto2 o = (Proto2) (other);
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
  
  /**
   * Syntax `proto3`.
   */
  public static final class Proto3 extends hydra.ext.protobuf.type.Syntax {
    public Proto3 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Proto3)) {
        return false;
      }
      Proto3 o = (Proto3) (other);
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
}
