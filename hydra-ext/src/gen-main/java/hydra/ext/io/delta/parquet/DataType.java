// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.delta.parquet;

import java.io.Serializable;

public abstract class DataType implements Serializable, Comparable<DataType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.delta.parquet.DataType");
  
  public static final hydra.core.Name ARRAY = new hydra.core.Name("array");
  
  public static final hydra.core.Name BASE = new hydra.core.Name("base");
  
  public static final hydra.core.Name DECIMAL = new hydra.core.Name("decimal");
  
  public static final hydra.core.Name MAP = new hydra.core.Name("map");
  
  public static final hydra.core.Name STRUCT = new hydra.core.Name("struct");
  
  private DataType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Array instance) ;
    
    R visit(Base instance) ;
    
    R visit(Decimal instance) ;
    
    R visit(Map instance) ;
    
    R visit(Struct instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DataType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Array instance) {
      return otherwise(instance);
    }
    
    default R visit(Base instance) {
      return otherwise(instance);
    }
    
    default R visit(Decimal instance) {
      return otherwise(instance);
    }
    
    default R visit(Map instance) {
      return otherwise(instance);
    }
    
    default R visit(Struct instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * Represent array data type.
   */
  public static final class Array extends hydra.ext.io.delta.parquet.DataType implements Serializable {
    public final hydra.ext.io.delta.parquet.ArrayType value;
    
    public Array (hydra.ext.io.delta.parquet.ArrayType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) other;
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
    public int compareTo(DataType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Array o = (Array) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Base class for all primitive types DataType.
   */
  public static final class Base extends hydra.ext.io.delta.parquet.DataType implements Serializable {
    public final hydra.ext.io.delta.parquet.BasePrimitiveType value;
    
    public Base (hydra.ext.io.delta.parquet.BasePrimitiveType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Base)) {
        return false;
      }
      Base o = (Base) other;
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
    public int compareTo(DataType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Base o = (Base) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A decimal data type.
   */
  public static final class Decimal extends hydra.ext.io.delta.parquet.DataType implements Serializable {
    public final hydra.ext.io.delta.parquet.DecimalType value;
    
    public Decimal (hydra.ext.io.delta.parquet.DecimalType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decimal)) {
        return false;
      }
      Decimal o = (Decimal) other;
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
    public int compareTo(DataType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Decimal o = (Decimal) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Data type representing a map type.
   */
  public static final class Map extends hydra.ext.io.delta.parquet.DataType implements Serializable {
    public final hydra.ext.io.delta.parquet.MapType value;
    
    public Map (hydra.ext.io.delta.parquet.MapType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) other;
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
    public int compareTo(DataType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Map o = (Map) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Struct type which contains one or more columns.
   */
  public static final class Struct extends hydra.ext.io.delta.parquet.DataType implements Serializable {
    public final hydra.ext.io.delta.parquet.StructType value;
    
    public Struct (hydra.ext.io.delta.parquet.StructType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Struct)) {
        return false;
      }
      Struct o = (Struct) other;
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
    public int compareTo(DataType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Struct o = (Struct) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
