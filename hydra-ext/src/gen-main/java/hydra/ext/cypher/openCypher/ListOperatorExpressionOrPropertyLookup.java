// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class ListOperatorExpressionOrPropertyLookup implements Serializable, Comparable<ListOperatorExpressionOrPropertyLookup> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup");
  
  public static final hydra.core.Name LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");
  
  private ListOperatorExpressionOrPropertyLookup () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(List instance) ;
    
    R visit(Property instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ListOperatorExpressionOrPropertyLookup instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(List instance) {
      return otherwise(instance);
    }
    
    default R visit(Property instance) {
      return otherwise(instance);
    }
  }
  
  public static final class List extends hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup implements Serializable {
    public final hydra.ext.cypher.openCypher.ListOperatorExpression value;
    
    public List (hydra.ext.cypher.openCypher.ListOperatorExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) other;
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
    public int compareTo(ListOperatorExpressionOrPropertyLookup other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      List o = (List) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Property extends hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup implements Serializable {
    public final hydra.ext.cypher.openCypher.PropertyLookup value;
    
    public Property (hydra.ext.cypher.openCypher.PropertyLookup value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) other;
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
    public int compareTo(ListOperatorExpressionOrPropertyLookup other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Property o = (Property) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
