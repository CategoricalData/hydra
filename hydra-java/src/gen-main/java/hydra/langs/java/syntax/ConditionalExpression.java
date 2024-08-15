// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public abstract class ConditionalExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.ConditionalExpression");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  public static final hydra.core.Name FIELD_NAME_TERNARY_COND = new hydra.core.Name("ternaryCond");
  
  public static final hydra.core.Name FIELD_NAME_TERNARY_LAMBDA = new hydra.core.Name("ternaryLambda");
  
  private ConditionalExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Simple instance) ;
    
    R visit(TernaryCond instance) ;
    
    R visit(TernaryLambda instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ConditionalExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
    
    default R visit(TernaryCond instance) {
      return otherwise((instance));
    }
    
    default R visit(TernaryLambda instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Simple extends hydra.langs.java.syntax.ConditionalExpression implements Serializable {
    public final hydra.langs.java.syntax.ConditionalOrExpression value;
    
    public Simple (hydra.langs.java.syntax.ConditionalOrExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) (other);
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
  
  public static final class TernaryCond extends hydra.langs.java.syntax.ConditionalExpression implements Serializable {
    public final hydra.langs.java.syntax.ConditionalExpression_TernaryCond value;
    
    public TernaryCond (hydra.langs.java.syntax.ConditionalExpression_TernaryCond value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TernaryCond)) {
        return false;
      }
      TernaryCond o = (TernaryCond) (other);
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
  
  public static final class TernaryLambda extends hydra.langs.java.syntax.ConditionalExpression implements Serializable {
    public final hydra.langs.java.syntax.ConditionalExpression_TernaryLambda value;
    
    public TernaryLambda (hydra.langs.java.syntax.ConditionalExpression_TernaryLambda value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TernaryLambda)) {
        return false;
      }
      TernaryLambda o = (TernaryLambda) (other);
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