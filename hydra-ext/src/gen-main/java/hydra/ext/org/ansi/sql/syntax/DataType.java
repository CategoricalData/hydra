// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class DataType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.DataType");
  
  public static final hydra.core.Name FIELD_NAME_PREDEFINED = new hydra.core.Name("predefined");
  
  public static final hydra.core.Name FIELD_NAME_ROW = new hydra.core.Name("row");
  
  public static final hydra.core.Name FIELD_NAME_NAMED = new hydra.core.Name("named");
  
  public static final hydra.core.Name FIELD_NAME_REFERENCE = new hydra.core.Name("reference");
  
  public static final hydra.core.Name FIELD_NAME_COLLECTION = new hydra.core.Name("collection");
  
  private DataType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Predefined instance) ;
    
    R visit(Row instance) ;
    
    R visit(Named instance) ;
    
    R visit(Reference instance) ;
    
    R visit(Collection instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DataType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Predefined instance) {
      return otherwise((instance));
    }
    
    default R visit(Row instance) {
      return otherwise((instance));
    }
    
    default R visit(Named instance) {
      return otherwise((instance));
    }
    
    default R visit(Reference instance) {
      return otherwise((instance));
    }
    
    default R visit(Collection instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Predefined extends hydra.ext.org.ansi.sql.syntax.DataType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.PredefinedType value;
    
    public Predefined (hydra.ext.org.ansi.sql.syntax.PredefinedType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Predefined)) {
        return false;
      }
      Predefined o = (Predefined) (other);
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
  
  public static final class Row extends hydra.ext.org.ansi.sql.syntax.DataType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.RowType value;
    
    public Row (hydra.ext.org.ansi.sql.syntax.RowType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Row)) {
        return false;
      }
      Row o = (Row) (other);
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
  
  public static final class Named extends hydra.ext.org.ansi.sql.syntax.DataType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.PathResolvedUserDefinedTypeName value;
    
    public Named (hydra.ext.org.ansi.sql.syntax.PathResolvedUserDefinedTypeName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Named)) {
        return false;
      }
      Named o = (Named) (other);
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
  
  public static final class Reference extends hydra.ext.org.ansi.sql.syntax.DataType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.ReferenceType value;
    
    public Reference (hydra.ext.org.ansi.sql.syntax.ReferenceType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reference)) {
        return false;
      }
      Reference o = (Reference) (other);
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
  
  public static final class Collection extends hydra.ext.org.ansi.sql.syntax.DataType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.CollectionType value;
    
    public Collection (hydra.ext.org.ansi.sql.syntax.CollectionType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Collection)) {
        return false;
      }
      Collection o = (Collection) (other);
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