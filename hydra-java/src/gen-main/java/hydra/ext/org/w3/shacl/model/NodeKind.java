// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

public abstract class NodeKind implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.shacl.model.NodeKind");
  
  public static final hydra.core.Name FIELD_NAME_BLANK_NODE = new hydra.core.Name("blankNode");
  
  public static final hydra.core.Name FIELD_NAME_IRI = new hydra.core.Name("iri");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_BLANK_NODE_OR_IRI = new hydra.core.Name("blankNodeOrIri");
  
  public static final hydra.core.Name FIELD_NAME_BLANK_NODE_OR_LITERAL = new hydra.core.Name("blankNodeOrLiteral");
  
  public static final hydra.core.Name FIELD_NAME_IRI_OR_LITERAL = new hydra.core.Name("iriOrLiteral");
  
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
  public static final class BlankNode extends hydra.ext.org.w3.shacl.model.NodeKind implements Serializable {
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
  public static final class Iri extends hydra.ext.org.w3.shacl.model.NodeKind implements Serializable {
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
  public static final class Literal extends hydra.ext.org.w3.shacl.model.NodeKind implements Serializable {
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
  public static final class BlankNodeOrIri extends hydra.ext.org.w3.shacl.model.NodeKind implements Serializable {
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
  public static final class BlankNodeOrLiteral extends hydra.ext.org.w3.shacl.model.NodeKind implements Serializable {
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
  public static final class IriOrLiteral extends hydra.ext.org.w3.shacl.model.NodeKind implements Serializable {
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