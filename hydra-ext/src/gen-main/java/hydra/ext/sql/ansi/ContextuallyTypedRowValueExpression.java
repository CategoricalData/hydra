// Note: this is an automatically generated file. Do not edit.

package hydra.ext.sql.ansi;

import java.io.Serializable;

public abstract class ContextuallyTypedRowValueExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/sql/ansi.ContextuallyTypedRowValueExpression");
  
  public static final hydra.core.Name FIELD_NAME_SPECIAL_CASE = new hydra.core.Name("specialCase");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRUCTOR = new hydra.core.Name("constructor");
  
  private ContextuallyTypedRowValueExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(SpecialCase instance) ;
    
    R visit(Constructor instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ContextuallyTypedRowValueExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(SpecialCase instance) {
      return otherwise((instance));
    }
    
    default R visit(Constructor instance) {
      return otherwise((instance));
    }
  }
  
  public static final class SpecialCase extends hydra.ext.sql.ansi.ContextuallyTypedRowValueExpression implements Serializable {
    public final hydra.ext.sql.ansi.RowValueSpecialCase value;
    
    public SpecialCase (hydra.ext.sql.ansi.RowValueSpecialCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SpecialCase)) {
        return false;
      }
      SpecialCase o = (SpecialCase) (other);
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
  
  public static final class Constructor extends hydra.ext.sql.ansi.ContextuallyTypedRowValueExpression implements Serializable {
    public final hydra.ext.sql.ansi.ContextuallyTypedRowValueConstructor value;
    
    public Constructor (hydra.ext.sql.ansi.ContextuallyTypedRowValueConstructor value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constructor)) {
        return false;
      }
      Constructor o = (Constructor) (other);
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
