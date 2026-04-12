// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case with an actual and expected string for comparison
 */
public abstract class TestCase implements Serializable, Comparable<TestCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.TestCase");

  public static final hydra.core.Name UNIVERSAL = new hydra.core.Name("universal");

  private TestCase () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Universal instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TestCase instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Universal instance) {
      return otherwise(instance);
    }
  }

  /**
   * A universal test case (string comparison)
   */
  public static final class Universal extends hydra.testing.TestCase implements Serializable {
    public final hydra.testing.UniversalTestCase value;

    public Universal (hydra.testing.UniversalTestCase value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Universal)) {
        return false;
      }
      Universal o = (Universal) other;
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
    public int compareTo(TestCase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Universal o = (Universal) other;
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
