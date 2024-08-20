// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalTerminalMethod implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.TraversalTerminalMethod");
  
  public static final hydra.core.Name FIELD_NAME_EXPLAIN = new hydra.core.Name("explain");
  
  public static final hydra.core.Name FIELD_NAME_ITERATE = new hydra.core.Name("iterate");
  
  public static final hydra.core.Name FIELD_NAME_HAS_NEXT = new hydra.core.Name("hasNext");
  
  public static final hydra.core.Name FIELD_NAME_TRY_NEXT = new hydra.core.Name("tryNext");
  
  public static final hydra.core.Name FIELD_NAME_NEXT = new hydra.core.Name("next");
  
  public static final hydra.core.Name FIELD_NAME_TO_LIST = new hydra.core.Name("toList");
  
  public static final hydra.core.Name FIELD_NAME_TO_SET = new hydra.core.Name("toSet");
  
  public static final hydra.core.Name FIELD_NAME_TO_BULK_SET = new hydra.core.Name("toBulkSet");
  
  private TraversalTerminalMethod () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Explain instance) ;
    
    R visit(Iterate instance) ;
    
    R visit(HasNext instance) ;
    
    R visit(TryNext instance) ;
    
    R visit(Next instance) ;
    
    R visit(ToList instance) ;
    
    R visit(ToSet instance) ;
    
    R visit(ToBulkSet instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalTerminalMethod instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Explain instance) {
      return otherwise((instance));
    }
    
    default R visit(Iterate instance) {
      return otherwise((instance));
    }
    
    default R visit(HasNext instance) {
      return otherwise((instance));
    }
    
    default R visit(TryNext instance) {
      return otherwise((instance));
    }
    
    default R visit(Next instance) {
      return otherwise((instance));
    }
    
    default R visit(ToList instance) {
      return otherwise((instance));
    }
    
    default R visit(ToSet instance) {
      return otherwise((instance));
    }
    
    default R visit(ToBulkSet instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Explain extends hydra.ext.tinkerpop.gremlin.TraversalTerminalMethod implements Serializable {
    public Explain () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Explain)) {
        return false;
      }
      Explain o = (Explain) (other);
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
  
  public static final class Iterate extends hydra.ext.tinkerpop.gremlin.TraversalTerminalMethod implements Serializable {
    public Iterate () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Iterate)) {
        return false;
      }
      Iterate o = (Iterate) (other);
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
  
  public static final class HasNext extends hydra.ext.tinkerpop.gremlin.TraversalTerminalMethod implements Serializable {
    public HasNext () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasNext)) {
        return false;
      }
      HasNext o = (HasNext) (other);
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
  
  public static final class TryNext extends hydra.ext.tinkerpop.gremlin.TraversalTerminalMethod implements Serializable {
    public TryNext () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TryNext)) {
        return false;
      }
      TryNext o = (TryNext) (other);
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
  
  public static final class Next extends hydra.ext.tinkerpop.gremlin.TraversalTerminalMethod implements Serializable {
    public final hydra.util.Opt<hydra.ext.tinkerpop.gremlin.IntegerLiteral> value;
    
    public Next (hydra.util.Opt<hydra.ext.tinkerpop.gremlin.IntegerLiteral> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Next)) {
        return false;
      }
      Next o = (Next) (other);
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
  
  public static final class ToList extends hydra.ext.tinkerpop.gremlin.TraversalTerminalMethod implements Serializable {
    public ToList () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToList)) {
        return false;
      }
      ToList o = (ToList) (other);
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
  
  public static final class ToSet extends hydra.ext.tinkerpop.gremlin.TraversalTerminalMethod implements Serializable {
    public ToSet () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToSet)) {
        return false;
      }
      ToSet o = (ToSet) (other);
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
  
  public static final class ToBulkSet extends hydra.ext.tinkerpop.gremlin.TraversalTerminalMethod implements Serializable {
    public ToBulkSet () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToBulkSet)) {
        return false;
      }
      ToBulkSet o = (ToBulkSet) (other);
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
