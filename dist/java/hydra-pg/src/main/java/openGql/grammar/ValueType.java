// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ValueType implements Serializable, Comparable<ValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ValueType");

  public static final hydra.core.Name PREDEFINED_TYPE = new hydra.core.Name("predefinedType");

  public static final hydra.core.Name PATH_VALUE_TYPE = new hydra.core.Name("pathValueType");

  public static final hydra.core.Name LIST_VALUE_TYPE_ALT1 = new hydra.core.Name("listValueTypeAlt1");

  public static final hydra.core.Name LIST_VALUE_TYPE_ALT2 = new hydra.core.Name("listValueTypeAlt2");

  public static final hydra.core.Name LIST_VALUE_TYPE_ALT3 = new hydra.core.Name("listValueTypeAlt3");

  public static final hydra.core.Name RECORD_TYPE = new hydra.core.Name("recordType");

  public static final hydra.core.Name OPEN_DYNAMIC_UNION_TYPE = new hydra.core.Name("openDynamicUnionType");

  public static final hydra.core.Name DYNAMIC_PROPERTY_VALUE_TYPE = new hydra.core.Name("dynamicPropertyValueType");

  public static final hydra.core.Name CLOSED_DYNAMIC_UNION_TYPE_ALT1 = new hydra.core.Name("closedDynamicUnionTypeAlt1");

  public static final hydra.core.Name CLOSED_DYNAMIC_UNION_TYPE_ALT2 = new hydra.core.Name("closedDynamicUnionTypeAlt2");

  private ValueType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PredefinedType instance) ;

    R visit(PathValueType instance) ;

    R visit(ListValueTypeAlt1 instance) ;

    R visit(ListValueTypeAlt2 instance) ;

    R visit(ListValueTypeAlt3 instance) ;

    R visit(RecordType instance) ;

    R visit(OpenDynamicUnionType instance) ;

    R visit(DynamicPropertyValueType instance) ;

    R visit(ClosedDynamicUnionTypeAlt1 instance) ;

    R visit(ClosedDynamicUnionTypeAlt2 instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ValueType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PredefinedType instance) {
      return otherwise(instance);
    }

    default R visit(PathValueType instance) {
      return otherwise(instance);
    }

    default R visit(ListValueTypeAlt1 instance) {
      return otherwise(instance);
    }

    default R visit(ListValueTypeAlt2 instance) {
      return otherwise(instance);
    }

    default R visit(ListValueTypeAlt3 instance) {
      return otherwise(instance);
    }

    default R visit(RecordType instance) {
      return otherwise(instance);
    }

    default R visit(OpenDynamicUnionType instance) {
      return otherwise(instance);
    }

    default R visit(DynamicPropertyValueType instance) {
      return otherwise(instance);
    }

    default R visit(ClosedDynamicUnionTypeAlt1 instance) {
      return otherwise(instance);
    }

    default R visit(ClosedDynamicUnionTypeAlt2 instance) {
      return otherwise(instance);
    }
  }

  public static final class PredefinedType extends openGql.grammar.ValueType implements Serializable {
    public final openGql.grammar.PredefinedType value;

    public PredefinedType (openGql.grammar.PredefinedType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PredefinedType)) {
        return false;
      }
      PredefinedType o = (PredefinedType) other;
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
    public int compareTo(ValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PredefinedType o = (PredefinedType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PathValueType extends openGql.grammar.ValueType implements Serializable {
    public final openGql.grammar.PathValueType value;

    public PathValueType (openGql.grammar.PathValueType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PathValueType)) {
        return false;
      }
      PathValueType o = (PathValueType) other;
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
    public int compareTo(ValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PathValueType o = (PathValueType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ListValueTypeAlt1 extends openGql.grammar.ValueType implements Serializable {
    public final openGql.grammar.ListValueTypeAlt1 value;

    public ListValueTypeAlt1 (openGql.grammar.ListValueTypeAlt1 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListValueTypeAlt1)) {
        return false;
      }
      ListValueTypeAlt1 o = (ListValueTypeAlt1) other;
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
    public int compareTo(ValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ListValueTypeAlt1 o = (ListValueTypeAlt1) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ListValueTypeAlt2 extends openGql.grammar.ValueType implements Serializable {
    public final openGql.grammar.ListValueTypeAlt2 value;

    public ListValueTypeAlt2 (openGql.grammar.ListValueTypeAlt2 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListValueTypeAlt2)) {
        return false;
      }
      ListValueTypeAlt2 o = (ListValueTypeAlt2) other;
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
    public int compareTo(ValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ListValueTypeAlt2 o = (ListValueTypeAlt2) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ListValueTypeAlt3 extends openGql.grammar.ValueType implements Serializable {
    public final openGql.grammar.ListValueTypeAlt3 value;

    public ListValueTypeAlt3 (openGql.grammar.ListValueTypeAlt3 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListValueTypeAlt3)) {
        return false;
      }
      ListValueTypeAlt3 o = (ListValueTypeAlt3) other;
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
    public int compareTo(ValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ListValueTypeAlt3 o = (ListValueTypeAlt3) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class RecordType extends openGql.grammar.ValueType implements Serializable {
    public final openGql.grammar.RecordType value;

    public RecordType (openGql.grammar.RecordType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RecordType)) {
        return false;
      }
      RecordType o = (RecordType) other;
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
    public int compareTo(ValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RecordType o = (RecordType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OpenDynamicUnionType extends openGql.grammar.ValueType implements Serializable {
    public final openGql.grammar.OpenDynamicUnionType value;

    public OpenDynamicUnionType (openGql.grammar.OpenDynamicUnionType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OpenDynamicUnionType)) {
        return false;
      }
      OpenDynamicUnionType o = (OpenDynamicUnionType) other;
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
    public int compareTo(ValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OpenDynamicUnionType o = (OpenDynamicUnionType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DynamicPropertyValueType extends openGql.grammar.ValueType implements Serializable {
    public final openGql.grammar.DynamicPropertyValueType value;

    public DynamicPropertyValueType (openGql.grammar.DynamicPropertyValueType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DynamicPropertyValueType)) {
        return false;
      }
      DynamicPropertyValueType o = (DynamicPropertyValueType) other;
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
    public int compareTo(ValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DynamicPropertyValueType o = (DynamicPropertyValueType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ClosedDynamicUnionTypeAlt1 extends openGql.grammar.ValueType implements Serializable {
    public final openGql.grammar.ClosedDynamicUnionTypeAlt1 value;

    public ClosedDynamicUnionTypeAlt1 (openGql.grammar.ClosedDynamicUnionTypeAlt1 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClosedDynamicUnionTypeAlt1)) {
        return false;
      }
      ClosedDynamicUnionTypeAlt1 o = (ClosedDynamicUnionTypeAlt1) other;
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
    public int compareTo(ValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ClosedDynamicUnionTypeAlt1 o = (ClosedDynamicUnionTypeAlt1) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ClosedDynamicUnionTypeAlt2 extends openGql.grammar.ValueType implements Serializable {
    public final openGql.grammar.ClosedDynamicUnionTypeAlt2 value;

    public ClosedDynamicUnionTypeAlt2 (openGql.grammar.ClosedDynamicUnionTypeAlt2 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClosedDynamicUnionTypeAlt2)) {
        return false;
      }
      ClosedDynamicUnionTypeAlt2 o = (ClosedDynamicUnionTypeAlt2) other;
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
    public int compareTo(ValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ClosedDynamicUnionTypeAlt2 o = (ClosedDynamicUnionTypeAlt2) other;
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
