// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalSourceSelfMethod implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod");
  
  public static final hydra.core.Name FIELD_NAME_WITH_BULK = new hydra.core.Name("withBulk");
  
  public static final hydra.core.Name FIELD_NAME_WITH_PATH = new hydra.core.Name("withPath");
  
  public static final hydra.core.Name FIELD_NAME_WITH_SACK = new hydra.core.Name("withSack");
  
  public static final hydra.core.Name FIELD_NAME_WITH_SIDE_EFFECT = new hydra.core.Name("withSideEffect");
  
  public static final hydra.core.Name FIELD_NAME_WITH_STRATEGIES = new hydra.core.Name("withStrategies");
  
  public static final hydra.core.Name FIELD_NAME_WITHOUT_STRATEGIES = new hydra.core.Name("withoutStrategies");
  
  public static final hydra.core.Name FIELD_NAME_WITH = new hydra.core.Name("with");
  
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
  
  public static final class WithBulk extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final Boolean value;
    
    public WithBulk (Boolean value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class WithPath extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
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
  
  public static final class WithSack extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument value;
    
    public WithSack (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class WithSideEffect extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument value;
    
    public WithSideEffect (hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class WithStrategies extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.TraversalStrategy> value;
    
    public WithStrategies (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.TraversalStrategy> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class WithoutStrategies extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.Identifier> value;
    
    public WithoutStrategies (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.Identifier> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class With extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument value;
    
    public With (hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument value) {
      java.util.Objects.requireNonNull((value));
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