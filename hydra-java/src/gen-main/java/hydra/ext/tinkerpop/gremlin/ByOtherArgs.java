// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ByOtherArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.ByOtherArgs");
  
  public static final hydra.core.Name FIELD_NAME_COMPARATOR = new hydra.core.Name("comparator");
  
  public static final hydra.core.Name FIELD_NAME_OTHER = new hydra.core.Name("other");
  
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
  
  public static final class Comparator extends hydra.ext.tinkerpop.gremlin.ByOtherArgs implements Serializable {
    public final hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalComparatorArgument> value;
    
    public Comparator (hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalComparatorArgument> value) {
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
  
  public static final class Other extends hydra.ext.tinkerpop.gremlin.ByOtherArgs implements Serializable {
    public final hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal> value;
    
    public Other (hydra.util.Opt<hydra.ext.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal> value) {
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
