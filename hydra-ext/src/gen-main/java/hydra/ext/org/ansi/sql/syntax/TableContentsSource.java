// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class TableContentsSource implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.TableContentsSource");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name FIELD_NAME_SUBTABLE = new hydra.core.Name("subtable");
  
  public static final hydra.core.Name FIELD_NAME_SUBQUERY = new hydra.core.Name("subquery");
  
  private TableContentsSource () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(List instance) ;
    
    R visit(Subtable instance) ;
    
    R visit(Subquery instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TableContentsSource instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Subtable instance) {
      return otherwise((instance));
    }
    
    default R visit(Subquery instance) {
      return otherwise((instance));
    }
  }
  
  public static final class List extends hydra.ext.org.ansi.sql.syntax.TableContentsSource implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.TableElementList value;
    
    public List (hydra.ext.org.ansi.sql.syntax.TableElementList value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  public static final class Subtable extends hydra.ext.org.ansi.sql.syntax.TableContentsSource implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable value;
    
    public Subtable (hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subtable)) {
        return false;
      }
      Subtable o = (Subtable) (other);
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
  
  public static final class Subquery extends hydra.ext.org.ansi.sql.syntax.TableContentsSource implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.AsSubqueryClause value;
    
    public Subquery (hydra.ext.org.ansi.sql.syntax.AsSubqueryClause value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subquery)) {
        return false;
      }
      Subquery o = (Subquery) (other);
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