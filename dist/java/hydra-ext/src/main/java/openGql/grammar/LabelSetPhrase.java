// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class LabelSetPhrase implements Serializable, Comparable<LabelSetPhrase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LabelSetPhrase");

  public static final hydra.core.Name SINGLE_LABEL = new hydra.core.Name("singleLabel");

  public static final hydra.core.Name MULTIPLE_LABELS = new hydra.core.Name("multipleLabels");

  public static final hydra.core.Name IS_OR_COLON_WITH_LABELS = new hydra.core.Name("isOrColonWithLabels");

  private LabelSetPhrase () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(SingleLabel instance) ;

    R visit(MultipleLabels instance) ;

    R visit(IsOrColonWithLabels instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LabelSetPhrase instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(SingleLabel instance) {
      return otherwise(instance);
    }

    default R visit(MultipleLabels instance) {
      return otherwise(instance);
    }

    default R visit(IsOrColonWithLabels instance) {
      return otherwise(instance);
    }
  }

  public static final class SingleLabel extends openGql.grammar.LabelSetPhrase implements Serializable {
    public final String value;

    public SingleLabel (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SingleLabel)) {
        return false;
      }
      SingleLabel o = (SingleLabel) other;
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
    public int compareTo(LabelSetPhrase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SingleLabel o = (SingleLabel) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MultipleLabels extends openGql.grammar.LabelSetPhrase implements Serializable {
    public final java.util.List<String> value;

    public MultipleLabels (java.util.List<String> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultipleLabels)) {
        return false;
      }
      MultipleLabels o = (MultipleLabels) other;
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
    public int compareTo(LabelSetPhrase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MultipleLabels o = (MultipleLabels) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class IsOrColonWithLabels extends openGql.grammar.LabelSetPhrase implements Serializable {
    public final openGql.grammar.IsOrColonWithLabels value;

    public IsOrColonWithLabels (openGql.grammar.IsOrColonWithLabels value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IsOrColonWithLabels)) {
        return false;
      }
      IsOrColonWithLabels o = (IsOrColonWithLabels) other;
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
    public int compareTo(LabelSetPhrase other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      IsOrColonWithLabels o = (IsOrColonWithLabels) other;
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
