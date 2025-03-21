// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public abstract class CaseTree implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.CaseTree");
  
  public static final hydra.core.Name FIELD_NAME_CASE = new hydra.core.Name("case");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_CASE = new hydra.core.Name("typeCase");
  
  private CaseTree () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Case instance) ;
    
    R visit(TypeCase instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CaseTree instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Case instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeCase instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Case extends hydra.ext.scala.meta.CaseTree implements Serializable {
    public final hydra.ext.scala.meta.Case value;
    
    public Case (hydra.ext.scala.meta.Case value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Case)) {
        return false;
      }
      Case o = (Case) (other);
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
  
  public static final class TypeCase extends hydra.ext.scala.meta.CaseTree implements Serializable {
    public final hydra.ext.scala.meta.TypeCase value;
    
    public TypeCase (hydra.ext.scala.meta.TypeCase value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeCase)) {
        return false;
      }
      TypeCase o = (TypeCase) (other);
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