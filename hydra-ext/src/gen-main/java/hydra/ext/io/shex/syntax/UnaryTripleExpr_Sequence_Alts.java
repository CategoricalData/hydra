// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class UnaryTripleExpr_Sequence_Alts implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence_Alts");
  
  public static final hydra.core.Name FIELD_NAME_TRIPLE_CONSTRAINT = new hydra.core.Name("tripleConstraint");
  
  public static final hydra.core.Name FIELD_NAME_BRACKETED_TRIPLE_EXPR = new hydra.core.Name("bracketedTripleExpr");
  
  private UnaryTripleExpr_Sequence_Alts () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(TripleConstraint instance) ;
    
    R visit(BracketedTripleExpr instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnaryTripleExpr_Sequence_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(TripleConstraint instance) {
      return otherwise((instance));
    }
    
    default R visit(BracketedTripleExpr instance) {
      return otherwise((instance));
    }
  }
  
  public static final class TripleConstraint extends hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.TripleConstraint value;
    
    public TripleConstraint (hydra.ext.io.shex.syntax.TripleConstraint value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TripleConstraint)) {
        return false;
      }
      TripleConstraint o = (TripleConstraint) (other);
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
  
  public static final class BracketedTripleExpr extends hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.BracketedTripleExpr value;
    
    public BracketedTripleExpr (hydra.ext.io.shex.syntax.BracketedTripleExpr value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BracketedTripleExpr)) {
        return false;
      }
      BracketedTripleExpr o = (BracketedTripleExpr) (other);
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