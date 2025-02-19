// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import java.io.Serializable;

public abstract class Node implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Node");
  
  public static final hydra.core.Name FIELD_NAME_IRI = new hydra.core.Name("iri");
  
  public static final hydra.core.Name FIELD_NAME_BNODE = new hydra.core.Name("bnode");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  private Node () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Iri instance) ;
    
    R visit(Bnode instance) ;
    
    R visit(Literal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Node instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Iri instance) {
      return otherwise((instance));
    }
    
    default R visit(Bnode instance) {
      return otherwise((instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Iri extends hydra.ext.org.w3.rdf.syntax.Node implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Iri value;
    
    public Iri (hydra.ext.org.w3.rdf.syntax.Iri value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Bnode extends hydra.ext.org.w3.rdf.syntax.Node implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.BlankNode value;
    
    public Bnode (hydra.ext.org.w3.rdf.syntax.BlankNode value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bnode)) {
        return false;
      }
      Bnode o = (Bnode) (other);
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
  
  public static final class Literal extends hydra.ext.org.w3.rdf.syntax.Node implements Serializable {
    public final hydra.ext.org.w3.rdf.syntax.Literal value;
    
    public Literal (hydra.ext.org.w3.rdf.syntax.Literal value) {
      java.util.Objects.requireNonNull((value));
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
}