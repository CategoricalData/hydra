// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Data_Ranges
 */
public abstract class DataRange implements Serializable, Comparable<DataRange> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.DataRange");

  public static final hydra.core.Name DATA_COMPLEMENT_OF = new hydra.core.Name("dataComplementOf");

  public static final hydra.core.Name DATA_INTERSECTION_OF = new hydra.core.Name("dataIntersectionOf");

  public static final hydra.core.Name DATA_ONE_OF = new hydra.core.Name("dataOneOf");

  public static final hydra.core.Name DATA_UNION_OF = new hydra.core.Name("dataUnionOf");

  public static final hydra.core.Name DATATYPE = new hydra.core.Name("datatype");

  public static final hydra.core.Name DATATYPE_RESTRICTION = new hydra.core.Name("datatypeRestriction");

  private DataRange () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DataComplementOf instance) ;

    R visit(DataIntersectionOf instance) ;

    R visit(DataOneOf instance) ;

    R visit(DataUnionOf instance) ;

    R visit(Datatype instance) ;

    R visit(DatatypeRestriction instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DataRange instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DataComplementOf instance) {
      return otherwise(instance);
    }

    default R visit(DataIntersectionOf instance) {
      return otherwise(instance);
    }

    default R visit(DataOneOf instance) {
      return otherwise(instance);
    }

    default R visit(DataUnionOf instance) {
      return otherwise(instance);
    }

    default R visit(Datatype instance) {
      return otherwise(instance);
    }

    default R visit(DatatypeRestriction instance) {
      return otherwise(instance);
    }
  }

  public static final class DataComplementOf extends hydra.owl.syntax.DataRange implements Serializable {
    public final hydra.owl.syntax.DataComplementOf value;

    public DataComplementOf (hydra.owl.syntax.DataComplementOf value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataComplementOf)) {
        return false;
      }
      DataComplementOf o = (DataComplementOf) other;
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
    public int compareTo(DataRange other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataComplementOf o = (DataComplementOf) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DataIntersectionOf extends hydra.owl.syntax.DataRange implements Serializable {
    public final hydra.owl.syntax.DataIntersectionOf value;

    public DataIntersectionOf (hydra.owl.syntax.DataIntersectionOf value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataIntersectionOf)) {
        return false;
      }
      DataIntersectionOf o = (DataIntersectionOf) other;
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
    public int compareTo(DataRange other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataIntersectionOf o = (DataIntersectionOf) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DataOneOf extends hydra.owl.syntax.DataRange implements Serializable {
    public final hydra.owl.syntax.DataOneOf value;

    public DataOneOf (hydra.owl.syntax.DataOneOf value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataOneOf)) {
        return false;
      }
      DataOneOf o = (DataOneOf) other;
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
    public int compareTo(DataRange other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataOneOf o = (DataOneOf) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DataUnionOf extends hydra.owl.syntax.DataRange implements Serializable {
    public final hydra.owl.syntax.DataUnionOf value;

    public DataUnionOf (hydra.owl.syntax.DataUnionOf value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataUnionOf)) {
        return false;
      }
      DataUnionOf o = (DataUnionOf) other;
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
    public int compareTo(DataRange other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataUnionOf o = (DataUnionOf) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Datatype extends hydra.owl.syntax.DataRange implements Serializable {
    public final hydra.owl.syntax.Datatype value;

    public Datatype (hydra.owl.syntax.Datatype value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Datatype)) {
        return false;
      }
      Datatype o = (Datatype) other;
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
    public int compareTo(DataRange other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Datatype o = (Datatype) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DatatypeRestriction extends hydra.owl.syntax.DataRange implements Serializable {
    public final hydra.owl.syntax.DatatypeRestriction value;

    public DatatypeRestriction (hydra.owl.syntax.DatatypeRestriction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DatatypeRestriction)) {
        return false;
      }
      DatatypeRestriction o = (DatatypeRestriction) other;
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
    public int compareTo(DataRange other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DatatypeRestriction o = (DatatypeRestriction) other;
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
