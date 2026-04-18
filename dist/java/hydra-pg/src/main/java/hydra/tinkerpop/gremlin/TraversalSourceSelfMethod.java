// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalSourceSelfMethod implements Serializable, Comparable<TraversalSourceSelfMethod> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.TraversalSourceSelfMethod");

  public static final hydra.core.Name WITH_BULK = new hydra.core.Name("withBulk");

  public static final hydra.core.Name WITH_PATH = new hydra.core.Name("withPath");

  public static final hydra.core.Name WITH_SACK = new hydra.core.Name("withSack");

  public static final hydra.core.Name WITH_SIDE_EFFECT = new hydra.core.Name("withSideEffect");

  public static final hydra.core.Name WITH_STRATEGIES = new hydra.core.Name("withStrategies");

  public static final hydra.core.Name WITHOUT_STRATEGIES = new hydra.core.Name("withoutStrategies");

  public static final hydra.core.Name WITH = new hydra.core.Name("with");

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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(WithBulk instance) {
      return otherwise(instance);
    }

    default R visit(WithPath instance) {
      return otherwise(instance);
    }

    default R visit(WithSack instance) {
      return otherwise(instance);
    }

    default R visit(WithSideEffect instance) {
      return otherwise(instance);
    }

    default R visit(WithStrategies instance) {
      return otherwise(instance);
    }

    default R visit(WithoutStrategies instance) {
      return otherwise(instance);
    }

    default R visit(With instance) {
      return otherwise(instance);
    }
  }

  public static final class WithBulk extends hydra.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final Boolean value;

    public WithBulk (Boolean value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithBulk)) {
        return false;
      }
      WithBulk o = (WithBulk) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalSourceSelfMethod other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      WithBulk o = (WithBulk) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class WithPath extends hydra.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public WithPath () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithPath)) {
        return false;
      }
      WithPath o = (WithPath) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalSourceSelfMethod other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class WithSack extends hydra.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument value;

    public WithSack (hydra.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithSack)) {
        return false;
      }
      WithSack o = (WithSack) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalSourceSelfMethod other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      WithSack o = (WithSack) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class WithSideEffect extends hydra.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument value;

    public WithSideEffect (hydra.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithSideEffect)) {
        return false;
      }
      WithSideEffect o = (WithSideEffect) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalSourceSelfMethod other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      WithSideEffect o = (WithSideEffect) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class WithStrategies extends hydra.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final java.util.List<hydra.tinkerpop.gremlin.TraversalStrategy> value;

    public WithStrategies (java.util.List<hydra.tinkerpop.gremlin.TraversalStrategy> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithStrategies)) {
        return false;
      }
      WithStrategies o = (WithStrategies) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalSourceSelfMethod other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      WithStrategies o = (WithStrategies) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class WithoutStrategies extends hydra.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final java.util.List<hydra.tinkerpop.gremlin.Identifier> value;

    public WithoutStrategies (java.util.List<hydra.tinkerpop.gremlin.Identifier> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithoutStrategies)) {
        return false;
      }
      WithoutStrategies o = (WithoutStrategies) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalSourceSelfMethod other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      WithoutStrategies o = (WithoutStrategies) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class With extends hydra.tinkerpop.gremlin.TraversalSourceSelfMethod implements Serializable {
    public final hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument value;

    public With (hydra.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof With)) {
        return false;
      }
      With o = (With) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalSourceSelfMethod other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      With o = (With) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
