// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalSourceSelfMethod implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalSourceSelfMethod");
  
  private TraversalSourceSelfMethod () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(WithBulk instance) ;
    
    R visit(WithPath instance) ;
    
    R visit(WithSack instance) ;
    
    R visit(WithSideEffect instance) ;
    
    R visit(WithStrategies instance) ;
    
    R visit(WithoutStrategies instance) ;
    
    R visit(With instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalSourceSelfMethod instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(WithBulk instance) {
      return otherwise((instance));
    }
    
    default R visit(WithPath instance) {
      return otherwise((instance));
    }
    
    default R visit(WithSack instance) {
      return otherwise((instance));
    }
    
    default R visit(WithSideEffect instance) {
      return otherwise((instance));
    }
    
    default R visit(WithStrategies instance) {
      return otherwise((instance));
    }
    
    default R visit(WithoutStrategies instance) {
      return otherwise((instance));
    }
    
    default R visit(With instance) {
      return otherwise((instance));
    }
  }
  
  public static final class WithBulk extends hydra.langs.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final Boolean value;
    
    public WithBulk (Boolean value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithBulk)) {
        return false;
      }
      WithBulk o = (WithBulk) (other);
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
  
  public static final class WithPath extends hydra.langs.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public WithPath () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithPath)) {
        return false;
      }
      WithPath o = (WithPath) (other);
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
  
  public static final class WithSack extends hydra.langs.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument value;
    
    public WithSack (hydra.langs.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithSack)) {
        return false;
      }
      WithSack o = (WithSack) (other);
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
  
  public static final class WithSideEffect extends hydra.langs.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument value;
    
    public WithSideEffect (hydra.langs.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithSideEffect)) {
        return false;
      }
      WithSideEffect o = (WithSideEffect) (other);
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
  
  public static final class WithStrategies extends hydra.langs.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.TraversalStrategy> value;
    
    public WithStrategies (java.util.List<hydra.langs.tinkerpop.gremlin.TraversalStrategy> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithStrategies)) {
        return false;
      }
      WithStrategies o = (WithStrategies) (other);
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
  
  public static final class WithoutStrategies extends hydra.langs.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.Identifier> value;
    
    public WithoutStrategies (java.util.List<hydra.langs.tinkerpop.gremlin.Identifier> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithoutStrategies)) {
        return false;
      }
      WithoutStrategies o = (WithoutStrategies) (other);
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
  
  public static final class With extends hydra.langs.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument value;
    
    public With (hydra.langs.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof With)) {
        return false;
      }
      With o = (With) (other);
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