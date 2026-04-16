// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class OptionalOperand implements Serializable, Comparable<OptionalOperand> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OptionalOperand");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  public static final hydra.core.Name BRACE_BLOCK = new hydra.core.Name("braceBlock");

  public static final hydra.core.Name PAREN_BLOCK = new hydra.core.Name("parenBlock");

  private OptionalOperand () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Simple instance) ;

    R visit(BraceBlock instance) ;

    R visit(ParenBlock instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OptionalOperand instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Simple instance) {
      return otherwise(instance);
    }

    default R visit(BraceBlock instance) {
      return otherwise(instance);
    }

    default R visit(ParenBlock instance) {
      return otherwise(instance);
    }
  }

  public static final class Simple extends openGql.grammar.OptionalOperand implements Serializable {
    public final openGql.grammar.GraphPatternBindingTable value;

    public Simple (openGql.grammar.GraphPatternBindingTable value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) other;
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
    public int compareTo(OptionalOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Simple o = (Simple) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BraceBlock extends openGql.grammar.OptionalOperand implements Serializable {
    public final java.util.List<openGql.grammar.MatchStatement> value;

    public BraceBlock (java.util.List<openGql.grammar.MatchStatement> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BraceBlock)) {
        return false;
      }
      BraceBlock o = (BraceBlock) other;
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
    public int compareTo(OptionalOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BraceBlock o = (BraceBlock) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ParenBlock extends openGql.grammar.OptionalOperand implements Serializable {
    public final java.util.List<openGql.grammar.MatchStatement> value;

    public ParenBlock (java.util.List<openGql.grammar.MatchStatement> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ParenBlock)) {
        return false;
      }
      ParenBlock o = (ParenBlock) other;
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
    public int compareTo(OptionalOperand other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ParenBlock o = (ParenBlock) other;
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
