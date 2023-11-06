package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class StringListNullPredicateSuffix implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StringListNullPredicateSuffix");
  
  private StringListNullPredicateSuffix () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(String_ instance) ;
    
    R visit(List instance) ;
    
    R visit(Null instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringListNullPredicateSuffix instance) {
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
  
  public static final class String_ extends hydra.langs.cypher.openCypher.StringListNullPredicateSuffix implements Serializable {
    public final hydra.langs.cypher.openCypher.StringPredicateExpression value;
    
    public String_ (hydra.langs.cypher.openCypher.StringPredicateExpression value) {
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
  
  public static final class List extends hydra.langs.cypher.openCypher.StringListNullPredicateSuffix implements Serializable {
    public final hydra.langs.cypher.openCypher.AddOrSubtractExpression value;
    
    public List (hydra.langs.cypher.openCypher.AddOrSubtractExpression value) {
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
  
  /**
   * True if NULL, false if NOT NULL
   */
  public static final class Null extends hydra.langs.cypher.openCypher.StringListNullPredicateSuffix implements Serializable {
    /**
     * True if NULL, false if NOT NULL
     */
    public final Boolean value;
    
    public Null (Boolean value) {
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