// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class WithArgsKeys implements Serializable, Comparable<WithArgsKeys> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WithArgsKeys");

  public static final hydra.core.Name WITH_OPTION = new hydra.core.Name("withOption");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  private WithArgsKeys () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(WithOption instance) ;

    R visit(String_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(WithArgsKeys instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(WithOption instance) {
      return otherwise(instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }
  }

  public static final class WithOption extends hydra.ext.org.apache.tinkerpop.gremlin.WithArgsKeys implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys value;

    public WithOption (hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithOption)) {
        return false;
      }
      WithOption o = (WithOption) other;
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
    public int compareTo(WithArgsKeys other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      WithOption o = (WithOption) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class String_ extends hydra.ext.org.apache.tinkerpop.gremlin.WithArgsKeys implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;

    public String_ (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
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
    public int compareTo(WithArgsKeys other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
