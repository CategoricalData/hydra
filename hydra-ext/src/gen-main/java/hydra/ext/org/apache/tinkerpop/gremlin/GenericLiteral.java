// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class GenericLiteral implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral");
  
  public static final hydra.core.Name FIELD_NAME_NUMERIC = new hydra.core.Name("numeric");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_DATE = new hydra.core.Name("date");
  
  public static final hydra.core.Name FIELD_NAME_NULL = new hydra.core.Name("null");
  
  public static final hydra.core.Name FIELD_NAME_NAN = new hydra.core.Name("nan");
  
  public static final hydra.core.Name FIELD_NAME_INF = new hydra.core.Name("inf");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL_TOKEN = new hydra.core.Name("traversalToken");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL_CARDINALITY = new hydra.core.Name("traversalCardinality");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL_DIRECTION = new hydra.core.Name("traversalDirection");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL_MERGE = new hydra.core.Name("traversalMerge");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL_PICK = new hydra.core.Name("traversalPick");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL_D_T = new hydra.core.Name("traversalDT");
  
  public static final hydra.core.Name FIELD_NAME_STRUCTURE_VERTEX = new hydra.core.Name("structureVertex");
  
  public static final hydra.core.Name FIELD_NAME_GENERIC_LITERAL_SET = new hydra.core.Name("genericLiteralSet");
  
  public static final hydra.core.Name FIELD_NAME_GENERIC_LITERAL_COLLECTION = new hydra.core.Name("genericLiteralCollection");
  
  public static final hydra.core.Name FIELD_NAME_GENERIC_LITERAL_RANGE = new hydra.core.Name("genericLiteralRange");
  
  public static final hydra.core.Name FIELD_NAME_NESTED_TRAVERSAL = new hydra.core.Name("nestedTraversal");
  
  public static final hydra.core.Name FIELD_NAME_TERMINATED_TRAVERSAL = new hydra.core.Name("terminatedTraversal");
  
  public static final hydra.core.Name FIELD_NAME_GENERIC_LITERAL_MAP = new hydra.core.Name("genericLiteralMap");
  
  private GenericLiteral () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Numeric instance) ;
    
    R visit(Boolean_ instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Date instance) ;
    
    R visit(Null instance) ;
    
    R visit(Nan instance) ;
    
    R visit(Inf instance) ;
    
    R visit(TraversalToken instance) ;
    
    R visit(TraversalCardinality instance) ;
    
    R visit(TraversalDirection instance) ;
    
    R visit(TraversalMerge instance) ;
    
    R visit(TraversalPick instance) ;
    
    R visit(TraversalDT instance) ;
    
    R visit(StructureVertex instance) ;
    
    R visit(GenericLiteralSet instance) ;
    
    R visit(GenericLiteralCollection instance) ;
    
    R visit(GenericLiteralRange instance) ;
    
    R visit(NestedTraversal instance) ;
    
    R visit(TerminatedTraversal instance) ;
    
    R visit(GenericLiteralMap instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GenericLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Numeric instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Date instance) {
      return otherwise((instance));
    }
    
    default R visit(Null instance) {
      return otherwise((instance));
    }
    
    default R visit(Nan instance) {
      return otherwise((instance));
    }
    
    default R visit(Inf instance) {
      return otherwise((instance));
    }
    
    default R visit(TraversalToken instance) {
      return otherwise((instance));
    }
    
    default R visit(TraversalCardinality instance) {
      return otherwise((instance));
    }
    
    default R visit(TraversalDirection instance) {
      return otherwise((instance));
    }
    
    default R visit(TraversalMerge instance) {
      return otherwise((instance));
    }
    
    default R visit(TraversalPick instance) {
      return otherwise((instance));
    }
    
    default R visit(TraversalDT instance) {
      return otherwise((instance));
    }
    
    default R visit(StructureVertex instance) {
      return otherwise((instance));
    }
    
    default R visit(GenericLiteralSet instance) {
      return otherwise((instance));
    }
    
    default R visit(GenericLiteralCollection instance) {
      return otherwise((instance));
    }
    
    default R visit(GenericLiteralRange instance) {
      return otherwise((instance));
    }
    
    default R visit(NestedTraversal instance) {
      return otherwise((instance));
    }
    
    default R visit(TerminatedTraversal instance) {
      return otherwise((instance));
    }
    
    default R visit(GenericLiteralMap instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Numeric extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NumericLiteral value;
    
    public Numeric (hydra.ext.org.apache.tinkerpop.gremlin.NumericLiteral value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Numeric)) {
        return false;
      }
      Numeric o = (Numeric) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Boolean_ extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final Boolean value;
    
    public Boolean_ (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Boolean_)) {
        return false;
      }
      Boolean_ o = (Boolean_) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class String_ extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final String value;
    
    public String_ (String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Date extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.DateLiteral value;
    
    public Date (hydra.ext.org.apache.tinkerpop.gremlin.DateLiteral value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Date)) {
        return false;
      }
      Date o = (Date) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Null extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public Null () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Null)) {
        return false;
      }
      Null o = (Null) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Nan extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public Nan () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nan)) {
        return false;
      }
      Nan o = (Nan) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Inf extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public Inf () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inf)) {
        return false;
      }
      Inf o = (Inf) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TraversalToken extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken value;
    
    public TraversalToken (hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalToken)) {
        return false;
      }
      TraversalToken o = (TraversalToken) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TraversalCardinality extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality value;
    
    public TraversalCardinality (hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalCardinality)) {
        return false;
      }
      TraversalCardinality o = (TraversalCardinality) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TraversalDirection extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirection value;
    
    public TraversalDirection (hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirection value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalDirection)) {
        return false;
      }
      TraversalDirection o = (TraversalDirection) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TraversalMerge extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalMerge value;
    
    public TraversalMerge (hydra.ext.org.apache.tinkerpop.gremlin.TraversalMerge value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalMerge)) {
        return false;
      }
      TraversalMerge o = (TraversalMerge) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TraversalPick extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPick value;
    
    public TraversalPick (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPick value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalPick)) {
        return false;
      }
      TraversalPick o = (TraversalPick) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TraversalDT extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalDT value;
    
    public TraversalDT (hydra.ext.org.apache.tinkerpop.gremlin.TraversalDT value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TraversalDT)) {
        return false;
      }
      TraversalDT o = (TraversalDT) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class StructureVertex extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StructureVertex value;
    
    public StructureVertex (hydra.ext.org.apache.tinkerpop.gremlin.StructureVertex value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StructureVertex)) {
        return false;
      }
      StructureVertex o = (StructureVertex) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GenericLiteralSet extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralSet value;
    
    public GenericLiteralSet (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralSet value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GenericLiteralSet)) {
        return false;
      }
      GenericLiteralSet o = (GenericLiteralSet) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GenericLiteralCollection extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralCollection value;
    
    public GenericLiteralCollection (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralCollection value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GenericLiteralCollection)) {
        return false;
      }
      GenericLiteralCollection o = (GenericLiteralCollection) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GenericLiteralRange extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralRange value;
    
    public GenericLiteralRange (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralRange value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GenericLiteralRange)) {
        return false;
      }
      GenericLiteralRange o = (GenericLiteralRange) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NestedTraversal extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;
    
    public NestedTraversal (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NestedTraversal)) {
        return false;
      }
      NestedTraversal o = (NestedTraversal) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TerminatedTraversal extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TerminatedTraversal value;
    
    public TerminatedTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TerminatedTraversal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TerminatedTraversal)) {
        return false;
      }
      TerminatedTraversal o = (TerminatedTraversal) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GenericLiteralMap extends hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMap value;
    
    public GenericLiteralMap (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMap value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GenericLiteralMap)) {
        return false;
      }
      GenericLiteralMap o = (GenericLiteralMap) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}