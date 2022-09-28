package hydra.ext.owl.syntax;

public abstract class Individual {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.Individual");
  
  private Individual () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Named instance) ;
    
    R visit(Anonymous instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Individual instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Named instance) {
      return otherwise((instance));
    }
    
    default R visit(Anonymous instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Named extends hydra.ext.owl.syntax.Individual {
    public final hydra.ext.owl.syntax.NamedIndividual value;
    
    public Named (hydra.ext.owl.syntax.NamedIndividual value) {
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
  
  public static final class Anonymous extends hydra.ext.owl.syntax.Individual {
    public final hydra.ext.owl.syntax.AnonymousIndividual value;
    
    public Anonymous (hydra.ext.owl.syntax.AnonymousIndividual value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Anonymous)) {
        return false;
      }
      Anonymous o = (Anonymous) (other);
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