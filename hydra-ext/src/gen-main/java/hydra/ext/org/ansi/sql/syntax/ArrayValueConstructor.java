// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class ArrayValueConstructor implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.ArrayValueConstructor");
  
  public static final hydra.core.Name FIELD_NAME_ENUMERATION = new hydra.core.Name("enumeration");
  
  public static final hydra.core.Name FIELD_NAME_QUERY = new hydra.core.Name("query");
  
  private ArrayValueConstructor () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Enumeration instance) ;
    
    R visit(Query instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ArrayValueConstructor instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Enumeration instance) {
      return otherwise((instance));
    }
    
    default R visit(Query instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Enumeration extends hydra.ext.org.ansi.sql.syntax.ArrayValueConstructor implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration value;
    
    public Enumeration (hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enumeration)) {
        return false;
      }
      Enumeration o = (Enumeration) (other);
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
  
  public static final class Query extends hydra.ext.org.ansi.sql.syntax.ArrayValueConstructor implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByQuery value;
    
    public Query (hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByQuery value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Query)) {
        return false;
      }
      Query o = (Query) (other);
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