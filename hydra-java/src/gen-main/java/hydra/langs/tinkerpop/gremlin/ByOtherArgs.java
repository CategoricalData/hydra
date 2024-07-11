// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ByOtherArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.ByOtherArgs");
  
  private ByOtherArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Comparator instance) ;
    
    R visit(Other instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ByOtherArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Comparator instance) {
      return otherwise((instance));
    }
    
    default R visit(Other instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Comparator extends hydra.langs.tinkerpop.gremlin.ByOtherArgs implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalComparatorArgument> value;
    
    public Comparator (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalComparatorArgument> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Comparator)) {
        return false;
      }
      Comparator o = (Comparator) (other);
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
  
  public static final class Other extends hydra.langs.tinkerpop.gremlin.ByOtherArgs implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal> value;
    
    public Other (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Other)) {
        return false;
      }
      Other o = (Other) (other);
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