// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public abstract class AnnotationSubject implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationSubject");
  
  public static final hydra.core.Name FIELD_NAME_IRI = new hydra.core.Name("iri");
  
  public static final hydra.core.Name FIELD_NAME_ANONYMOUS_INDIVIDUAL = new hydra.core.Name("anonymousIndividual");
  
  private AnnotationSubject () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Iri instance) ;
    
    R visit(AnonymousIndividual instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AnnotationSubject instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Iri instance) {
      return otherwise((instance));
    }
    
    default R visit(AnonymousIndividual instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Iri extends hydra.ext.org.w3.owl.syntax.AnnotationSubject implements Serializable {
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
  
  public static final class AnonymousIndividual extends hydra.ext.org.w3.owl.syntax.AnnotationSubject implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.AnonymousIndividual value;
    
    public AnonymousIndividual (hydra.ext.org.w3.owl.syntax.AnonymousIndividual value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnonymousIndividual)) {
        return false;
      }
      AnonymousIndividual o = (AnonymousIndividual) (other);
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