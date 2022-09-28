package hydra.ext.owl.syntax;

public abstract class AnnotationValue {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.AnnotationValue");
  
  private AnnotationValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(AnonymousIndividual instance) ;
    
    R visit(Iri instance) ;
    
    R visit(Literal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AnnotationValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(AnonymousIndividual instance) {
      return otherwise((instance));
    }
    
    default R visit(Iri instance) {
      return otherwise((instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
  }
  
  public static final class AnonymousIndividual extends hydra.ext.owl.syntax.AnnotationValue {
    public final hydra.ext.owl.syntax.AnonymousIndividual value;
    
    public AnonymousIndividual (hydra.ext.owl.syntax.AnonymousIndividual value) {
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
  
  public static final class Iri extends hydra.ext.owl.syntax.AnnotationValue {
    public final hydra.ext.rdf.syntax.Iri value;
    
    public Iri (hydra.ext.rdf.syntax.Iri value) {
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
  
  public static final class Literal extends hydra.ext.owl.syntax.AnnotationValue {
    public final hydra.ext.rdf.syntax.Literal value;
    
    public Literal (hydra.ext.rdf.syntax.Literal value) {
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