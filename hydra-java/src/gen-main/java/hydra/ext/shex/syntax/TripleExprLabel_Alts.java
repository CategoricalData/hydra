package hydra.ext.shex.syntax;

public abstract class TripleExprLabel_Alts {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.TripleExprLabel.Alts");
  
  private TripleExprLabel_Alts () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Iri instance) ;
    
    R visit(BlankNode instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TripleExprLabel_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Iri instance) {
      return otherwise((instance));
    }
    
    default R visit(BlankNode instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Iri extends hydra.ext.shex.syntax.TripleExprLabel_Alts {
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
  
  public static final class BlankNode extends hydra.ext.shex.syntax.TripleExprLabel_Alts {
    public final hydra.ext.shex.syntax.BlankNode value;
    
    public BlankNode (hydra.ext.shex.syntax.BlankNode value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BlankNode)) {
        return false;
      }
      BlankNode o = (BlankNode) (other);
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