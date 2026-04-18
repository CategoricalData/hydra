// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CharacterOrByteStringFunction implements Serializable, Comparable<CharacterOrByteStringFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CharacterOrByteStringFunction");

  public static final hydra.core.Name SUB = new hydra.core.Name("sub");

  public static final hydra.core.Name TRIM_SINGLE = new hydra.core.Name("trimSingle");

  public static final hydra.core.Name FOLD = new hydra.core.Name("fold");

  public static final hydra.core.Name TRIM_MULTI_CHARACTER = new hydra.core.Name("trimMultiCharacter");

  public static final hydra.core.Name NORMALIZE = new hydra.core.Name("normalize");

  private CharacterOrByteStringFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Sub instance) ;

    R visit(TrimSingle instance) ;

    R visit(Fold instance) ;

    R visit(TrimMultiCharacter instance) ;

    R visit(Normalize instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CharacterOrByteStringFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Sub instance) {
      return otherwise(instance);
    }

    default R visit(TrimSingle instance) {
      return otherwise(instance);
    }

    default R visit(Fold instance) {
      return otherwise(instance);
    }

    default R visit(TrimMultiCharacter instance) {
      return otherwise(instance);
    }

    default R visit(Normalize instance) {
      return otherwise(instance);
    }
  }

  public static final class Sub extends openGql.grammar.CharacterOrByteStringFunction implements Serializable {
    public final openGql.grammar.SubCharacterOrByteString value;

    public Sub (openGql.grammar.SubCharacterOrByteString value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sub)) {
        return false;
      }
      Sub o = (Sub) other;
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
    public int compareTo(CharacterOrByteStringFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sub o = (Sub) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TrimSingle extends openGql.grammar.CharacterOrByteStringFunction implements Serializable {
    public final openGql.grammar.TrimOperands value;

    public TrimSingle (openGql.grammar.TrimOperands value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TrimSingle)) {
        return false;
      }
      TrimSingle o = (TrimSingle) other;
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
    public int compareTo(CharacterOrByteStringFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TrimSingle o = (TrimSingle) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Fold extends openGql.grammar.CharacterOrByteStringFunction implements Serializable {
    public final openGql.grammar.FoldCharacterString value;

    public Fold (openGql.grammar.FoldCharacterString value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fold)) {
        return false;
      }
      Fold o = (Fold) other;
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
    public int compareTo(CharacterOrByteStringFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Fold o = (Fold) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TrimMultiCharacter extends openGql.grammar.CharacterOrByteStringFunction implements Serializable {
    public final openGql.grammar.TrimMultiCharacterCharacterString value;

    public TrimMultiCharacter (openGql.grammar.TrimMultiCharacterCharacterString value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TrimMultiCharacter)) {
        return false;
      }
      TrimMultiCharacter o = (TrimMultiCharacter) other;
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
    public int compareTo(CharacterOrByteStringFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TrimMultiCharacter o = (TrimMultiCharacter) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Normalize extends openGql.grammar.CharacterOrByteStringFunction implements Serializable {
    public final openGql.grammar.NormalizeCharacterString value;

    public Normalize (openGql.grammar.NormalizeCharacterString value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Normalize)) {
        return false;
      }
      Normalize o = (Normalize) other;
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
    public int compareTo(CharacterOrByteStringFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Normalize o = (Normalize) other;
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
