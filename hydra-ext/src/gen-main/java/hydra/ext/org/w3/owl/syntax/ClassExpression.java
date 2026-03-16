// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public abstract class ClassExpression implements Serializable, Comparable<ClassExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ClassExpression");
  
  public static final hydra.core.Name CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name DATA_SOME_VALUES_FROM = new hydra.core.Name("dataSomeValuesFrom");
  
  public static final hydra.core.Name DATA_ALL_VALUES_FROM = new hydra.core.Name("dataAllValuesFrom");
  
  public static final hydra.core.Name DATA_HAS_VALUE = new hydra.core.Name("dataHasValue");
  
  public static final hydra.core.Name DATA_MIN_CARDINALITY = new hydra.core.Name("dataMinCardinality");
  
  public static final hydra.core.Name DATA_MAX_CARDINALITY = new hydra.core.Name("dataMaxCardinality");
  
  public static final hydra.core.Name DATA_EXACT_CARDINALITY = new hydra.core.Name("dataExactCardinality");
  
  public static final hydra.core.Name OBJECT_ALL_VALUES_FROM = new hydra.core.Name("objectAllValuesFrom");
  
  public static final hydra.core.Name OBJECT_EXACT_CARDINALITY = new hydra.core.Name("objectExactCardinality");
  
  public static final hydra.core.Name OBJECT_HAS_SELF = new hydra.core.Name("objectHasSelf");
  
  public static final hydra.core.Name OBJECT_HAS_VALUE = new hydra.core.Name("objectHasValue");
  
  public static final hydra.core.Name OBJECT_INTERSECTION_OF = new hydra.core.Name("objectIntersectionOf");
  
  public static final hydra.core.Name OBJECT_MAX_CARDINALITY = new hydra.core.Name("objectMaxCardinality");
  
  public static final hydra.core.Name OBJECT_MIN_CARDINALITY = new hydra.core.Name("objectMinCardinality");
  
  public static final hydra.core.Name OBJECT_ONE_OF = new hydra.core.Name("objectOneOf");
  
  public static final hydra.core.Name OBJECT_SOME_VALUES_FROM = new hydra.core.Name("objectSomeValuesFrom");
  
  public static final hydra.core.Name OBJECT_UNION_OF = new hydra.core.Name("objectUnionOf");
  
  private ClassExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Class_ instance) ;
    
    R visit(DataSomeValuesFrom instance) ;
    
    R visit(DataAllValuesFrom instance) ;
    
    R visit(DataHasValue instance) ;
    
    R visit(DataMinCardinality instance) ;
    
    R visit(DataMaxCardinality instance) ;
    
    R visit(DataExactCardinality instance) ;
    
    R visit(ObjectAllValuesFrom instance) ;
    
    R visit(ObjectExactCardinality instance) ;
    
    R visit(ObjectHasSelf instance) ;
    
    R visit(ObjectHasValue instance) ;
    
    R visit(ObjectIntersectionOf instance) ;
    
    R visit(ObjectMaxCardinality instance) ;
    
    R visit(ObjectMinCardinality instance) ;
    
    R visit(ObjectOneOf instance) ;
    
    R visit(ObjectSomeValuesFrom instance) ;
    
    R visit(ObjectUnionOf instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Class_ instance) {
      return otherwise(instance);
    }
    
    default R visit(DataSomeValuesFrom instance) {
      return otherwise(instance);
    }
    
    default R visit(DataAllValuesFrom instance) {
      return otherwise(instance);
    }
    
    default R visit(DataHasValue instance) {
      return otherwise(instance);
    }
    
    default R visit(DataMinCardinality instance) {
      return otherwise(instance);
    }
    
    default R visit(DataMaxCardinality instance) {
      return otherwise(instance);
    }
    
    default R visit(DataExactCardinality instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectAllValuesFrom instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectExactCardinality instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectHasSelf instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectHasValue instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectIntersectionOf instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectMaxCardinality instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectMinCardinality instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectOneOf instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectSomeValuesFrom instance) {
      return otherwise(instance);
    }
    
    default R visit(ObjectUnionOf instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Class_ extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.Class_ value;
    
    public Class_ (hydra.ext.org.w3.owl.syntax.Class_ value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Class_ o = (Class_) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DataSomeValuesFrom extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom value;
    
    public DataSomeValuesFrom (hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataSomeValuesFrom)) {
        return false;
      }
      DataSomeValuesFrom o = (DataSomeValuesFrom) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataSomeValuesFrom o = (DataSomeValuesFrom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DataAllValuesFrom extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DataAllValuesFrom value;
    
    public DataAllValuesFrom (hydra.ext.org.w3.owl.syntax.DataAllValuesFrom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataAllValuesFrom)) {
        return false;
      }
      DataAllValuesFrom o = (DataAllValuesFrom) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataAllValuesFrom o = (DataAllValuesFrom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DataHasValue extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DataHasValue value;
    
    public DataHasValue (hydra.ext.org.w3.owl.syntax.DataHasValue value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataHasValue)) {
        return false;
      }
      DataHasValue o = (DataHasValue) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataHasValue o = (DataHasValue) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DataMinCardinality extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DataMinCardinality value;
    
    public DataMinCardinality (hydra.ext.org.w3.owl.syntax.DataMinCardinality value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataMinCardinality)) {
        return false;
      }
      DataMinCardinality o = (DataMinCardinality) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataMinCardinality o = (DataMinCardinality) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DataMaxCardinality extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DataMaxCardinality value;
    
    public DataMaxCardinality (hydra.ext.org.w3.owl.syntax.DataMaxCardinality value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataMaxCardinality)) {
        return false;
      }
      DataMaxCardinality o = (DataMaxCardinality) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataMaxCardinality o = (DataMaxCardinality) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DataExactCardinality extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DataExactCardinality value;
    
    public DataExactCardinality (hydra.ext.org.w3.owl.syntax.DataExactCardinality value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataExactCardinality)) {
        return false;
      }
      DataExactCardinality o = (DataExactCardinality) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataExactCardinality o = (DataExactCardinality) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectAllValuesFrom extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom value;
    
    public ObjectAllValuesFrom (hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectAllValuesFrom)) {
        return false;
      }
      ObjectAllValuesFrom o = (ObjectAllValuesFrom) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectAllValuesFrom o = (ObjectAllValuesFrom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectExactCardinality extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectExactCardinality value;
    
    public ObjectExactCardinality (hydra.ext.org.w3.owl.syntax.ObjectExactCardinality value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectExactCardinality)) {
        return false;
      }
      ObjectExactCardinality o = (ObjectExactCardinality) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectExactCardinality o = (ObjectExactCardinality) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectHasSelf extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectHasSelf value;
    
    public ObjectHasSelf (hydra.ext.org.w3.owl.syntax.ObjectHasSelf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectHasSelf)) {
        return false;
      }
      ObjectHasSelf o = (ObjectHasSelf) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectHasSelf o = (ObjectHasSelf) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectHasValue extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectHasValue value;
    
    public ObjectHasValue (hydra.ext.org.w3.owl.syntax.ObjectHasValue value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectHasValue)) {
        return false;
      }
      ObjectHasValue o = (ObjectHasValue) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectHasValue o = (ObjectHasValue) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectIntersectionOf extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectIntersectionOf value;
    
    public ObjectIntersectionOf (hydra.ext.org.w3.owl.syntax.ObjectIntersectionOf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectIntersectionOf)) {
        return false;
      }
      ObjectIntersectionOf o = (ObjectIntersectionOf) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectIntersectionOf o = (ObjectIntersectionOf) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectMaxCardinality extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality value;
    
    public ObjectMaxCardinality (hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectMaxCardinality)) {
        return false;
      }
      ObjectMaxCardinality o = (ObjectMaxCardinality) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectMaxCardinality o = (ObjectMaxCardinality) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectMinCardinality extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectMinCardinality value;
    
    public ObjectMinCardinality (hydra.ext.org.w3.owl.syntax.ObjectMinCardinality value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectMinCardinality)) {
        return false;
      }
      ObjectMinCardinality o = (ObjectMinCardinality) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectMinCardinality o = (ObjectMinCardinality) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectOneOf extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectOneOf value;
    
    public ObjectOneOf (hydra.ext.org.w3.owl.syntax.ObjectOneOf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectOneOf)) {
        return false;
      }
      ObjectOneOf o = (ObjectOneOf) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectOneOf o = (ObjectOneOf) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectSomeValuesFrom extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectSomeValuesFrom value;
    
    public ObjectSomeValuesFrom (hydra.ext.org.w3.owl.syntax.ObjectSomeValuesFrom value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectSomeValuesFrom)) {
        return false;
      }
      ObjectSomeValuesFrom o = (ObjectSomeValuesFrom) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectSomeValuesFrom o = (ObjectSomeValuesFrom) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ObjectUnionOf extends hydra.ext.org.w3.owl.syntax.ClassExpression implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectUnionOf value;
    
    public ObjectUnionOf (hydra.ext.org.w3.owl.syntax.ObjectUnionOf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectUnionOf)) {
        return false;
      }
      ObjectUnionOf o = (ObjectUnionOf) other;
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
    public int compareTo(ClassExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectUnionOf o = (ObjectUnionOf) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
