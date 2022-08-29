package hydra.ext.shacl.model;

public abstract class NodeKind {
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
    /**
     * A blank node
     */
    public final java.lang.Void value;
    
    public BlankNode (java.lang.Void value) {
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
  
  /**
   * An IRI
   */
  public static final class Iri extends hydra.ext.shacl.model.NodeKind {
    /**
     * An IRI
     */
    public final java.lang.Void value;
    
    public Iri (java.lang.Void value) {
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
  
  /**
   * A literal
   */
  public static final class Literal extends hydra.ext.shacl.model.NodeKind {
    /**
     * A literal
     */
    public final java.lang.Void value;
    
    public Literal (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
  
  /**
   * A blank node or an IRI
   */
  public static final class BlankNodeOrIri extends hydra.ext.shacl.model.NodeKind {
    /**
     * A blank node or an IRI
     */
    public final java.lang.Void value;
    
    public BlankNodeOrIri (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BlankNodeOrIri)) {
        return false;
      }
      BlankNodeOrIri o = (BlankNodeOrIri) (other);
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
  
  /**
   * A blank node or a literal
   */
  public static final class BlankNodeOrLiteral extends hydra.ext.shacl.model.NodeKind {
    /**
     * A blank node or a literal
     */
    public final java.lang.Void value;
    
    public BlankNodeOrLiteral (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BlankNodeOrLiteral)) {
        return false;
      }
      BlankNodeOrLiteral o = (BlankNodeOrLiteral) (other);
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
  
  /**
   * An IRI or a literal
   */
  public static final class IriOrLiteral extends hydra.ext.shacl.model.NodeKind {
    /**
     * An IRI or a literal
     */
    public final java.lang.Void value;
    
    public IriOrLiteral (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IriOrLiteral)) {
        return false;
      }
      IriOrLiteral o = (IriOrLiteral) (other);
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