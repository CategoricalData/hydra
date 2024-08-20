// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class ShapeDefinition_ListOfAlts_Elmt implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.ShapeDefinition.ListOfAlts.Elmt");
  
  public static final hydra.core.Name FIELD_NAME_INCLUDE_SET = new hydra.core.Name("includeSet");
  
  public static final hydra.core.Name FIELD_NAME_EXTRA_PROPERTY_SET = new hydra.core.Name("extraPropertySet");
  
  public static final hydra.core.Name FIELD_NAME_C_L_O_S_E_D = new hydra.core.Name("cLOSED");
  
  private ShapeDefinition_ListOfAlts_Elmt () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(IncludeSet instance) ;
    
    R visit(ExtraPropertySet instance) ;
    
    R visit(CLOSED instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShapeDefinition_ListOfAlts_Elmt instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(IncludeSet instance) {
      return otherwise((instance));
    }
    
    default R visit(ExtraPropertySet instance) {
      return otherwise((instance));
    }
    
    default R visit(CLOSED instance) {
      return otherwise((instance));
    }
  }
  
  public static final class IncludeSet extends hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt implements Serializable {
    public final hydra.ext.io.shex.syntax.IncludeSet value;
    
    public IncludeSet (hydra.ext.io.shex.syntax.IncludeSet value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IncludeSet)) {
        return false;
      }
      IncludeSet o = (IncludeSet) (other);
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
  
  public static final class ExtraPropertySet extends hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt implements Serializable {
    public final hydra.ext.io.shex.syntax.ExtraPropertySet value;
    
    public ExtraPropertySet (hydra.ext.io.shex.syntax.ExtraPropertySet value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExtraPropertySet)) {
        return false;
      }
      ExtraPropertySet o = (ExtraPropertySet) (other);
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
  
  public static final class CLOSED extends hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt implements Serializable {
    public CLOSED () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CLOSED)) {
        return false;
      }
      CLOSED o = (CLOSED) (other);
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
}