package hydra.ext.shex.syntax;

public abstract class Predicate {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Predicate");
  
  private Predicate () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Iri instance) ;
    
    R visit(RdfType instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Predicate instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Iri instance) {
      return otherwise((instance));
    }
    
    default R visit(RdfType instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Iri extends hydra.ext.shex.syntax.Predicate {
    public final hydra.ext.shex.syntax.Iri value;
    
    public Iri (hydra.ext.shex.syntax.Iri value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Iri)) {
        return false;
      }
      Iri o = (Iri) (other);
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
  
  public static final class RdfType extends hydra.ext.shex.syntax.Predicate {
    public final hydra.ext.shex.syntax.RdfType value;
    
    public RdfType (hydra.ext.shex.syntax.RdfType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RdfType)) {
        return false;
      }
      RdfType o = (RdfType) (other);
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