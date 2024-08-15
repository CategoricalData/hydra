// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class ListOperatorExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ListOperatorExpression");
  
  public static final hydra.core.Name FIELD_NAME_SINGLE = new hydra.core.Name("single");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  private ListOperatorExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Single instance) ;
    
    R visit(Range instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ListOperatorExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Single instance) {
      return otherwise((instance));
    }
    
    default R visit(Range instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Single extends hydra.langs.cypher.openCypher.ListOperatorExpression implements Serializable {
    public final hydra.langs.cypher.openCypher.Expression value;
    
    public Single (hydra.langs.cypher.openCypher.Expression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Single)) {
        return false;
      }
      Single o = (Single) (other);
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
  
  public static final class Range extends hydra.langs.cypher.openCypher.ListOperatorExpression implements Serializable {
    public final hydra.langs.cypher.openCypher.RangeExpression value;
    
    public Range (hydra.langs.cypher.openCypher.RangeExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Range)) {
        return false;
      }
      Range o = (Range) (other);
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