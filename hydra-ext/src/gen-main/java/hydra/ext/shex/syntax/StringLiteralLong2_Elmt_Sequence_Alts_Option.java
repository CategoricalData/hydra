// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public abstract class StringLiteralLong2_Elmt_Sequence_Alts_Option implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.StringLiteralLong2.Elmt.Sequence.Alts.Option");
  
  public static final hydra.core.Name FIELD_NAME_QUOT = new hydra.core.Name("quot");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  private StringLiteralLong2_Elmt_Sequence_Alts_Option () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Quot instance) ;
    
    R visit(Sequence instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringLiteralLong2_Elmt_Sequence_Alts_Option instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Quot instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Quot extends hydra.ext.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option implements Serializable {
    public Quot () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Quot)) {
        return false;
      }
      Quot o = (Quot) (other);
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
  
  public static final class Sequence extends hydra.ext.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option implements Serializable {
    public final hydra.ext.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence value;
    
    public Sequence (hydra.ext.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence value) {
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
}