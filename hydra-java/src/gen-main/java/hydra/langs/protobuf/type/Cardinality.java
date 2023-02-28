package hydra.langs.protobuf.type;

/**
 * Whether a field is optional, required, or repeated.
 */
public abstract class Cardinality {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/protobuf/type.Cardinality");
  
  private Cardinality () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Unknown instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Required instance) ;
    
    R visit(Repeated instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Cardinality instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Unknown instance) {
      return otherwise((instance));
    }
    
    default R visit(Optional instance) {
      return otherwise((instance));
    }
    
    default R visit(Required instance) {
      return otherwise((instance));
    }
    
    default R visit(Repeated instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * For fields with unknown cardinality.
   */
  public static final class Unknown extends hydra.langs.protobuf.type.Cardinality {
    public Unknown () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unknown)) {
        return false;
      }
      Unknown o = (Unknown) (other);
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
   * For optional fields.
   */
  public static final class Optional extends hydra.langs.protobuf.type.Cardinality {
    public Optional () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) (other);
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
   * For required fields. Proto2 syntax only.
   */
  public static final class Required extends hydra.langs.protobuf.type.Cardinality {
    public Required () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Required)) {
        return false;
      }
      Required o = (Required) (other);
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
   * For repeated fields.
   */
  public static final class Repeated extends hydra.langs.protobuf.type.Cardinality {
    public Repeated () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeated)) {
        return false;
      }
      Repeated o = (Repeated) (other);
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