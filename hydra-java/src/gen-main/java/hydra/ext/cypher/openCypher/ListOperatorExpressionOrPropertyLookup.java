// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class ListOperatorExpressionOrPropertyLookup implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  private ListOperatorExpressionOrPropertyLookup () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(List instance) ;
    
    R visit(Property instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ListOperatorExpressionOrPropertyLookup instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
  }
  
  public static final class List extends hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup implements Serializable {
    public final hydra.ext.cypher.openCypher.ListOperatorExpression value;
    
    public List (hydra.ext.cypher.openCypher.ListOperatorExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  public static final class Property extends hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup implements Serializable {
    public final hydra.ext.cypher.openCypher.PropertyLookup value;
    
    public Property (hydra.ext.cypher.openCypher.PropertyLookup value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
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