// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class StringListNullPredicateRightHandSide implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StringListNullPredicateRightHandSide");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name FIELD_NAME_NULL = new hydra.core.Name("null");
  
  private StringListNullPredicateRightHandSide () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(String_ instance) ;
    
    R visit(List instance) ;
    
    R visit(Null instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringListNullPredicateRightHandSide instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Null instance) {
      return otherwise((instance));
    }
  }
  
  public static final class String_ extends hydra.langs.cypher.openCypher.StringListNullPredicateRightHandSide implements Serializable {
    public final hydra.langs.cypher.openCypher.StringPredicateExpression value;
    
    public String_ (hydra.langs.cypher.openCypher.StringPredicateExpression value) {
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
  
  public static final class List extends hydra.langs.cypher.openCypher.StringListNullPredicateRightHandSide implements Serializable {
    public final hydra.langs.cypher.openCypher.ListPredicateExpression value;
    
    public List (hydra.langs.cypher.openCypher.ListPredicateExpression value) {
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
  
  public static final class Null extends hydra.langs.cypher.openCypher.StringListNullPredicateRightHandSide implements Serializable {
    public final hydra.langs.cypher.openCypher.NullPredicateExpression value;
    
    public Null (hydra.langs.cypher.openCypher.NullPredicateExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Null)) {
        return false;
      }
      Null o = (Null) (other);
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