// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Pattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Pattern");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  private Pattern () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Pattern_ instance) ;
    
    R visit(Term instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Pattern_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Term instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Pattern_ extends hydra.ext.fr.inria.coq.syntax.Pattern implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Pattern10 value;
    
    public Pattern_ (hydra.ext.fr.inria.coq.syntax.Pattern10 value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern_)) {
        return false;
      }
      Pattern_ o = (Pattern_) (other);
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
  
  public static final class Term extends hydra.ext.fr.inria.coq.syntax.Pattern implements Serializable {
    public final hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Term> value;
    
    public Term (hydra.util.Opt<hydra.ext.fr.inria.coq.syntax.Term> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term)) {
        return false;
      }
      Term o = (Term) (other);
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