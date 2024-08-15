// Note: this is an automatically generated file. Do not edit.

package hydra.langs.shex.syntax;

import java.io.Serializable;

public abstract class UnaryTripleExpr implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/shex/syntax.UnaryTripleExpr");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public static final hydra.core.Name FIELD_NAME_INCLUDE = new hydra.core.Name("include");
  
  private UnaryTripleExpr () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Sequence instance) ;
    
    R visit(Include instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnaryTripleExpr instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
    
    default R visit(Include instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Sequence extends hydra.langs.shex.syntax.UnaryTripleExpr implements Serializable {
    public final hydra.langs.shex.syntax.UnaryTripleExpr_Sequence value;
    
    public Sequence (hydra.langs.shex.syntax.UnaryTripleExpr_Sequence value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) (other);
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
  
  public static final class Include extends hydra.langs.shex.syntax.UnaryTripleExpr implements Serializable {
    public final hydra.langs.shex.syntax.Include value;
    
    public Include (hydra.langs.shex.syntax.Include value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Include)) {
        return false;
      }
      Include o = (Include) (other);
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