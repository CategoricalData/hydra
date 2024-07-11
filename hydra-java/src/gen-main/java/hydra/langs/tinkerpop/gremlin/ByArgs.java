// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ByArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.ByArgs");
  
  private ByArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Order instance) ;
    
    R visit(Token instance) ;
    
    R visit(Other instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ByArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Order instance) {
      return otherwise((instance));
    }
    
    default R visit(Token instance) {
      return otherwise((instance));
    }
    
    default R visit(Other instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Order extends hydra.langs.tinkerpop.gremlin.ByArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalOrderArgument value;
    
    public Order (hydra.langs.tinkerpop.gremlin.TraversalOrderArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Order)) {
        return false;
      }
      Order o = (Order) (other);
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
  
  public static final class Token extends hydra.langs.tinkerpop.gremlin.ByArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalTokenArgument value;
    
    public Token (hydra.langs.tinkerpop.gremlin.TraversalTokenArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Token)) {
        return false;
      }
      Token o = (Token) (other);
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
  
  public static final class Other extends hydra.langs.tinkerpop.gremlin.ByArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.ByOtherArgs value;
    
    public Other (hydra.langs.tinkerpop.gremlin.ByOtherArgs value) {
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