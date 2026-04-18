// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * The types of types are called sorts.
 */
public abstract class Sort implements Serializable, Comparable<Sort> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Sort");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name PROP = new hydra.core.Name("prop");

  public static final hydra.core.Name S_PROP = new hydra.core.Name("sProp");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name TYPE_WITH_ANY_UNIVERSE = new hydra.core.Name("typeWithAnyUniverse");

  public static final hydra.core.Name TYPE_WITH_UNIVERSE = new hydra.core.Name("typeWithUniverse");

  private Sort () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Set instance) ;

    R visit(Prop instance) ;

    R visit(SProp instance) ;

    R visit(Type instance) ;

    R visit(TypeWithAnyUniverse instance) ;

    R visit(TypeWithUniverse instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Sort instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Set instance) {
      return otherwise(instance);
    }

    default R visit(Prop instance) {
      return otherwise(instance);
    }

    default R visit(SProp instance) {
      return otherwise(instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }

    default R visit(TypeWithAnyUniverse instance) {
      return otherwise(instance);
    }

    default R visit(TypeWithUniverse instance) {
      return otherwise(instance);
    }
  }

  /**
   * The sort 𝖲𝖾𝗍 intends to be the type of small sets.
   */
  public static final class Set extends hydra.coq.syntax.Sort implements Serializable {
    public Set () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Sort other) {
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

  /**
   * The sort 𝖯𝗋𝗈𝗉 intends to be the type of logical propositions.
   */
  public static final class Prop extends hydra.coq.syntax.Sort implements Serializable {
    public Prop () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Prop)) {
        return false;
      }
      Prop o = (Prop) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Sort other) {
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

  /**
   * The sort 𝖲𝖯𝗋𝗈𝗉 is like 𝖯𝗋𝗈𝗉 but the propositions in 𝖲𝖯𝗋𝗈𝗉 are known to have irrelevant proofs (all proofs are equal).
   */
  public static final class SProp extends hydra.coq.syntax.Sort implements Serializable {
    public SProp () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SProp)) {
        return false;
      }
      SProp o = (SProp) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Sort other) {
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

  public static final class Type extends hydra.coq.syntax.Sort implements Serializable {
    public Type () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Sort other) {
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

  public static final class TypeWithAnyUniverse extends hydra.coq.syntax.Sort implements Serializable {
    public TypeWithAnyUniverse () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeWithAnyUniverse)) {
        return false;
      }
      TypeWithAnyUniverse o = (TypeWithAnyUniverse) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Sort other) {
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

  public static final class TypeWithUniverse extends hydra.coq.syntax.Sort implements Serializable {
    public final hydra.coq.syntax.Universe value;

    public TypeWithUniverse (hydra.coq.syntax.Universe value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeWithUniverse)) {
        return false;
      }
      TypeWithUniverse o = (TypeWithUniverse) other;
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
    public int compareTo(Sort other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeWithUniverse o = (TypeWithUniverse) other;
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
