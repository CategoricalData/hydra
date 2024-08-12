// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A query pattern
 */
public abstract class Pattern implements Serializable {
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
  public static final class Triple extends hydra.query.Pattern implements Serializable {
    public final hydra.query.TriplePattern value;
    
    public Triple (hydra.query.TriplePattern value) {
      java.util.Objects.requireNonNull((value));
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
  public static final class Negation extends hydra.query.Pattern implements Serializable {
    public final hydra.query.Pattern value;
    
    public Negation (hydra.query.Pattern value) {
      java.util.Objects.requireNonNull((value));
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
  public static final class Conjunction extends hydra.query.Pattern implements Serializable {
    public final java.util.List<hydra.query.Pattern> value;
    
    public Conjunction (java.util.List<hydra.query.Pattern> value) {
      java.util.Objects.requireNonNull((value));
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
  public static final class Disjunction extends hydra.query.Pattern implements Serializable {
    public final java.util.List<hydra.query.Pattern> value;
    
    public Disjunction (java.util.List<hydra.query.Pattern> value) {
      java.util.Objects.requireNonNull((value));
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
  public static final class Graph extends hydra.query.Pattern implements Serializable {
    public final hydra.query.GraphPattern value;
    
    public Graph (hydra.query.GraphPattern value) {
      java.util.Objects.requireNonNull((value));
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