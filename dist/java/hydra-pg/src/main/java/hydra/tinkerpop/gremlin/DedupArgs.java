// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class DedupArgs implements Serializable, Comparable<DedupArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.DedupArgs");

  public static final hydra.core.Name SCOPE_STRING = new hydra.core.Name("scopeString");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  private DedupArgs () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ScopeString instance) ;

    R visit(String_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DedupArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ScopeString instance) {
      return otherwise(instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }
  }

  public static final class ScopeString extends hydra.tinkerpop.gremlin.DedupArgs implements Serializable {
    public final hydra.tinkerpop.gremlin.ScopeStringArgument value;

    public ScopeString (hydra.tinkerpop.gremlin.ScopeStringArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ScopeString)) {
        return false;
      }
      ScopeString o = (ScopeString) other;
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
    public int compareTo(DedupArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ScopeString o = (ScopeString) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class String_ extends hydra.tinkerpop.gremlin.DedupArgs implements Serializable {
    public final java.util.List<hydra.tinkerpop.gremlin.StringNullableArgument> value;

    public String_ (java.util.List<hydra.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) other;
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
    public int compareTo(DedupArgs other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
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
