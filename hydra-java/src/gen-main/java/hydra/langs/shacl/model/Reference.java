package hydra.langs.shacl.model;

/**
 * Either an instance of a type like sh:Shape or sh:NodeShape, or an IRI which refers to an instance of that type
 */
public abstract class Reference<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/shacl/model.Reference");
  
  private Reference () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Named instance) ;
    
    R visit(Anonymous instance) ;
    
    R visit(Definition instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Reference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Named instance) {
      return otherwise((instance));
    }
    
    default R visit(Anonymous instance) {
      return otherwise((instance));
    }
    
    default R visit(Definition instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Named<A> extends hydra.langs.shacl.model.Reference<A> {
    public final hydra.langs.rdf.syntax.Iri value;
    
    public Named (hydra.langs.rdf.syntax.Iri value) {
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
  
  /**
   * An anonymous instance
   */
  public static final class Anonymous<A> extends hydra.langs.shacl.model.Reference<A> {
    /**
     * An anonymous instance
     */
    public final A value;
    
    public Anonymous (A value) {
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
  
  /**
   * An inline definition
   */
  public static final class Definition<A> extends hydra.langs.shacl.model.Reference<A> {
    /**
     * An inline definition
     */
    public final hydra.langs.shacl.model.Definition<A> value;
    
    public Definition (hydra.langs.shacl.model.Definition<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Definition)) {
        return false;
      }
      Definition o = (Definition) (other);
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