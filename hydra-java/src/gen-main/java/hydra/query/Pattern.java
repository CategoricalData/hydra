package hydra.query;

/**
 * A query pattern
 */
public abstract class Pattern<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/query.Pattern");
  
  private Pattern () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Triple instance) ;
    
    R visit(Negation instance) ;
    
    R visit(Conjunction instance) ;
    
    R visit(Disjunction instance) ;
    
    R visit(Graph instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Triple instance) {
      return otherwise((instance));
    }
    
    default R visit(Negation instance) {
      return otherwise((instance));
    }
    
    default R visit(Conjunction instance) {
      return otherwise((instance));
    }
    
    default R visit(Disjunction instance) {
      return otherwise((instance));
    }
    
    default R visit(Graph instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A subject/predicate/object pattern
   */
  public static final class Triple<A> extends hydra.query.Pattern<A> {
    /**
     * A subject/predicate/object pattern
     */
    public final hydra.query.TriplePattern<A> value;
    
    public Triple (hydra.query.TriplePattern<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Triple)) {
        return false;
      }
      Triple o = (Triple) (other);
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
   * The negation of another pattern
   */
  public static final class Negation<A> extends hydra.query.Pattern<A> {
    /**
     * The negation of another pattern
     */
    public final hydra.query.Pattern<A> value;
    
    public Negation (hydra.query.Pattern<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Negation)) {
        return false;
      }
      Negation o = (Negation) (other);
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
   * The conjunction ('and') of several other patterns
   */
  public static final class Conjunction<A> extends hydra.query.Pattern<A> {
    /**
     * The conjunction ('and') of several other patterns
     */
    public final java.util.List<hydra.query.Pattern<A>> value;
    
    public Conjunction (java.util.List<hydra.query.Pattern<A>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Conjunction)) {
        return false;
      }
      Conjunction o = (Conjunction) (other);
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
   * The disjunction (inclusive 'or') of several other patterns
   */
  public static final class Disjunction<A> extends hydra.query.Pattern<A> {
    /**
     * The disjunction (inclusive 'or') of several other patterns
     */
    public final java.util.List<hydra.query.Pattern<A>> value;
    
    public Disjunction (java.util.List<hydra.query.Pattern<A>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Disjunction)) {
        return false;
      }
      Disjunction o = (Disjunction) (other);
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
   * A pattern which matches within a named subgraph
   */
  public static final class Graph<A> extends hydra.query.Pattern<A> {
    /**
     * A pattern which matches within a named subgraph
     */
    public final hydra.query.GraphPattern<A> value;
    
    public Graph (hydra.query.GraphPattern<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Graph)) {
        return false;
      }
      Graph o = (Graph) (other);
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