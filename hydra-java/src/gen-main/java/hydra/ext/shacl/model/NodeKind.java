package hydra.ext.shacl.model;

public abstract class NodeKind {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.NodeKind");
  
  private NodeKind () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(BlankNode instance) ;
    
    R visit(Iri instance) ;
    
    R visit(Literal instance) ;
    
    R visit(BlankNodeOrIri instance) ;
    
    R visit(BlankNodeOrLiteral instance) ;
    
    R visit(IriOrLiteral instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NodeKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(BlankNode instance) {
      return otherwise((instance));
    }
    
    default R visit(Iri instance) {
      return otherwise((instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(BlankNodeOrIri instance) {
      return otherwise((instance));
    }
    
    default R visit(BlankNodeOrLiteral instance) {
      return otherwise((instance));
    }
    
    default R visit(IriOrLiteral instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A blank node
   */
  public static final class BlankNode extends hydra.ext.shacl.model.NodeKind {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.BlankNode");
    
    public BlankNode () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BlankNode)) {
        return false;
      }
      BlankNode o = (BlankNode) (other);
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
   * An IRI
   */
  public static final class Iri extends hydra.ext.shacl.model.NodeKind {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.Iri");
    
    public Iri () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Iri)) {
        return false;
      }
      Iri o = (Iri) (other);
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
   * A literal
   */
  public static final class Literal extends hydra.ext.shacl.model.NodeKind {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.Literal");
    
    public Literal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
   * A blank node or an IRI
   */
  public static final class BlankNodeOrIri extends hydra.ext.shacl.model.NodeKind {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.BlankNodeOrIri");
    
    public BlankNodeOrIri () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BlankNodeOrIri)) {
        return false;
      }
      BlankNodeOrIri o = (BlankNodeOrIri) (other);
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
   * A blank node or a literal
   */
  public static final class BlankNodeOrLiteral extends hydra.ext.shacl.model.NodeKind {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.BlankNodeOrLiteral");
    
    public BlankNodeOrLiteral () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BlankNodeOrLiteral)) {
        return false;
      }
      BlankNodeOrLiteral o = (BlankNodeOrLiteral) (other);
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
   * An IRI or a literal
   */
  public static final class IriOrLiteral extends hydra.ext.shacl.model.NodeKind {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.IriOrLiteral");
    
    public IriOrLiteral () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IriOrLiteral)) {
        return false;
      }
      IriOrLiteral o = (IriOrLiteral) (other);
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