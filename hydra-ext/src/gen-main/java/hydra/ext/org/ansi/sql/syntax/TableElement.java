// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class TableElement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.TableElement");
  
  public static final hydra.core.Name FIELD_NAME_COLUMN = new hydra.core.Name("column");
  
  public static final hydra.core.Name FIELD_NAME_TABLE_CONSTRAINT = new hydra.core.Name("tableConstraint");
  
  public static final hydra.core.Name FIELD_NAME_LIKE = new hydra.core.Name("like");
  
  public static final hydra.core.Name FIELD_NAME_SELF_REFERENCING_COLUMN = new hydra.core.Name("selfReferencingColumn");
  
  public static final hydra.core.Name FIELD_NAME_COLUM_OPTIONS = new hydra.core.Name("columOptions");
  
  private TableElement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Column instance) ;
    
    R visit(TableConstraint instance) ;
    
    R visit(Like instance) ;
    
    R visit(SelfReferencingColumn instance) ;
    
    R visit(ColumOptions instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TableElement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Column instance) {
      return otherwise((instance));
    }
    
    default R visit(TableConstraint instance) {
      return otherwise((instance));
    }
    
    default R visit(Like instance) {
      return otherwise((instance));
    }
    
    default R visit(SelfReferencingColumn instance) {
      return otherwise((instance));
    }
    
    default R visit(ColumOptions instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Column extends hydra.ext.org.ansi.sql.syntax.TableElement implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.ColumnDefinition value;
    
    public Column (hydra.ext.org.ansi.sql.syntax.ColumnDefinition value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Column)) {
        return false;
      }
      Column o = (Column) (other);
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
  
  public static final class TableConstraint extends hydra.ext.org.ansi.sql.syntax.TableElement implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.TableConstraintDefinition value;
    
    public TableConstraint (hydra.ext.org.ansi.sql.syntax.TableConstraintDefinition value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TableConstraint)) {
        return false;
      }
      TableConstraint o = (TableConstraint) (other);
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
  
  public static final class Like extends hydra.ext.org.ansi.sql.syntax.TableElement implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.LikeClause value;
    
    public Like (hydra.ext.org.ansi.sql.syntax.LikeClause value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Like)) {
        return false;
      }
      Like o = (Like) (other);
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
  
  public static final class SelfReferencingColumn extends hydra.ext.org.ansi.sql.syntax.TableElement implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.SelfReferencingColumnSpecification value;
    
    public SelfReferencingColumn (hydra.ext.org.ansi.sql.syntax.SelfReferencingColumnSpecification value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SelfReferencingColumn)) {
        return false;
      }
      SelfReferencingColumn o = (SelfReferencingColumn) (other);
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
  
  public static final class ColumOptions extends hydra.ext.org.ansi.sql.syntax.TableElement implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.ColumnOptions value;
    
    public ColumOptions (hydra.ext.org.ansi.sql.syntax.ColumnOptions value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ColumOptions)) {
        return false;
      }
      ColumOptions o = (ColumOptions) (other);
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