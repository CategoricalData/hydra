// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Data_Ranges
 */
public abstract class DataRange implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.DataRange");
  
  public static final hydra.core.Name FIELD_NAME_DATA_COMPLEMENT_OF = new hydra.core.Name("dataComplementOf");
  
  public static final hydra.core.Name FIELD_NAME_DATA_INTERSECTION_OF = new hydra.core.Name("dataIntersectionOf");
  
  public static final hydra.core.Name FIELD_NAME_DATA_ONE_OF = new hydra.core.Name("dataOneOf");
  
  public static final hydra.core.Name FIELD_NAME_DATA_UNION_OF = new hydra.core.Name("dataUnionOf");
  
  public static final hydra.core.Name FIELD_NAME_DATATYPE = new hydra.core.Name("datatype");
  
  public static final hydra.core.Name FIELD_NAME_DATATYPE_RESTRICTION = new hydra.core.Name("datatypeRestriction");
  
  private DataRange () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(DataComplementOf instance) ;
    
    R visit(DataIntersectionOf instance) ;
    
    R visit(DataOneOf instance) ;
    
    R visit(DataUnionOf instance) ;
    
    R visit(Datatype instance) ;
    
    R visit(DatatypeRestriction instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DataRange instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(DataComplementOf instance) {
      return otherwise((instance));
    }
    
    default R visit(DataIntersectionOf instance) {
      return otherwise((instance));
    }
    
    default R visit(DataOneOf instance) {
      return otherwise((instance));
    }
    
    default R visit(DataUnionOf instance) {
      return otherwise((instance));
    }
    
    default R visit(Datatype instance) {
      return otherwise((instance));
    }
    
    default R visit(DatatypeRestriction instance) {
      return otherwise((instance));
    }
  }
  
  public static final class DataComplementOf extends hydra.ext.owl.syntax.DataRange implements Serializable {
    public final hydra.ext.owl.syntax.DataComplementOf value;
    
    public DataComplementOf (hydra.ext.owl.syntax.DataComplementOf value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataComplementOf)) {
        return false;
      }
      DataComplementOf o = (DataComplementOf) (other);
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
  
  public static final class DataIntersectionOf extends hydra.ext.owl.syntax.DataRange implements Serializable {
    public final hydra.ext.owl.syntax.DataIntersectionOf value;
    
    public DataIntersectionOf (hydra.ext.owl.syntax.DataIntersectionOf value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataIntersectionOf)) {
        return false;
      }
      DataIntersectionOf o = (DataIntersectionOf) (other);
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
  
  public static final class DataOneOf extends hydra.ext.owl.syntax.DataRange implements Serializable {
    public final hydra.ext.owl.syntax.DataOneOf value;
    
    public DataOneOf (hydra.ext.owl.syntax.DataOneOf value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataOneOf)) {
        return false;
      }
      DataOneOf o = (DataOneOf) (other);
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
  
  public static final class DataUnionOf extends hydra.ext.owl.syntax.DataRange implements Serializable {
    public final hydra.ext.owl.syntax.DataUnionOf value;
    
    public DataUnionOf (hydra.ext.owl.syntax.DataUnionOf value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataUnionOf)) {
        return false;
      }
      DataUnionOf o = (DataUnionOf) (other);
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
  
  public static final class Datatype extends hydra.ext.owl.syntax.DataRange implements Serializable {
    public final hydra.ext.owl.syntax.Datatype value;
    
    public Datatype (hydra.ext.owl.syntax.Datatype value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Datatype)) {
        return false;
      }
      Datatype o = (Datatype) (other);
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
  
  public static final class DatatypeRestriction extends hydra.ext.owl.syntax.DataRange implements Serializable {
    public final hydra.ext.owl.syntax.DatatypeRestriction value;
    
    public DatatypeRestriction (hydra.ext.owl.syntax.DatatypeRestriction value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DatatypeRestriction)) {
        return false;
      }
      DatatypeRestriction o = (DatatypeRestriction) (other);
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
