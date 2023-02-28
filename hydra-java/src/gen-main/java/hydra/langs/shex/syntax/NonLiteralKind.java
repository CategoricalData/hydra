package hydra.langs.shex.syntax;

public abstract class NonLiteralKind {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shex/syntax.NonLiteralKind");
  
  private NonLiteralKind () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(IRI instance) ;
    
    R visit(BNODE instance) ;
    
    R visit(NONLITERAL instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NonLiteralKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(IRI instance) {
      return otherwise((instance));
    }
    
    default R visit(BNODE instance) {
      return otherwise((instance));
    }
    
    default R visit(NONLITERAL instance) {
      return otherwise((instance));
    }
  }
  
  public static final class IRI extends hydra.langs.shex.syntax.NonLiteralKind {
    public IRI () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IRI)) {
        return false;
      }
      IRI o = (IRI) (other);
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
  
  public static final class BNODE extends hydra.langs.shex.syntax.NonLiteralKind {
    public BNODE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BNODE)) {
        return false;
      }
      BNODE o = (BNODE) (other);
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
  
  public static final class NONLITERAL extends hydra.langs.shex.syntax.NonLiteralKind {
    public NONLITERAL () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NONLITERAL)) {
        return false;
      }
      NONLITERAL o = (NONLITERAL) (other);
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