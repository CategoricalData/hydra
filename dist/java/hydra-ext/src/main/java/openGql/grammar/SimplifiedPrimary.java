// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimplifiedPrimary implements Serializable, Comparable<SimplifiedPrimary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedPrimary");

  public static final hydra.core.Name LABEL_NAME = new hydra.core.Name("labelName");

  public static final hydra.core.Name PARENTHESIZED_CONTENTS = new hydra.core.Name("parenthesizedContents");

  private SimplifiedPrimary () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(LabelName instance) ;

    R visit(ParenthesizedContents instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimplifiedPrimary instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(LabelName instance) {
      return otherwise(instance);
    }

    default R visit(ParenthesizedContents instance) {
      return otherwise(instance);
    }
  }

  public static final class LabelName extends openGql.grammar.SimplifiedPrimary implements Serializable {
    public final String value;

    public LabelName (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LabelName)) {
        return false;
      }
      LabelName o = (LabelName) other;
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
    public int compareTo(SimplifiedPrimary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LabelName o = (LabelName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ParenthesizedContents extends openGql.grammar.SimplifiedPrimary implements Serializable {
    public final openGql.grammar.SimplifiedContents value;

    public ParenthesizedContents (openGql.grammar.SimplifiedContents value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParenthesizedContents)) {
        return false;
      }
      ParenthesizedContents o = (ParenthesizedContents) other;
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
    public int compareTo(SimplifiedPrimary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ParenthesizedContents o = (ParenthesizedContents) other;
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
