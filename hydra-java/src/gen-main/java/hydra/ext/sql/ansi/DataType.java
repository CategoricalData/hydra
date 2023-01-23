package hydra.ext.sql.ansi;

public abstract class DataType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.DataType");
  
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
  
  public static final class Predefined extends hydra.ext.sql.ansi.DataType {
    public final hydra.ext.sql.ansi.PredefinedType value;
    
    public Predefined (hydra.ext.sql.ansi.PredefinedType value) {
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
  
  public static final class Row extends hydra.ext.sql.ansi.DataType {
    public final hydra.ext.sql.ansi.RowType value;
    
    public Row (hydra.ext.sql.ansi.RowType value) {
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
  
  public static final class Named extends hydra.ext.sql.ansi.DataType {
    public final hydra.ext.sql.ansi.PathResolvedUserDefinedTypeName value;
    
    public Named (hydra.ext.sql.ansi.PathResolvedUserDefinedTypeName value) {
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
  
  public static final class Reference extends hydra.ext.sql.ansi.DataType {
    public final hydra.ext.sql.ansi.ReferenceType value;
    
    public Reference (hydra.ext.sql.ansi.ReferenceType value) {
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
  
  public static final class Collection extends hydra.ext.sql.ansi.DataType {
    public final hydra.ext.sql.ansi.CollectionType value;
    
    public Collection (hydra.ext.sql.ansi.CollectionType value) {
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