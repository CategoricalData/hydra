// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class BuiltInFunction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/kusto/kql.BuiltInFunction");
  
  public static final hydra.core.Name FIELD_NAME_AGO = new hydra.core.Name("ago");
  
  public static final hydra.core.Name FIELD_NAME_BIN = new hydra.core.Name("bin");
  
  public static final hydra.core.Name FIELD_NAME_COUNT = new hydra.core.Name("count");
  
  public static final hydra.core.Name FIELD_NAME_DCOUNT = new hydra.core.Name("dcount");
  
  public static final hydra.core.Name FIELD_NAME_ENDOFDAY = new hydra.core.Name("endofday");
  
  public static final hydra.core.Name FIELD_NAME_EXTRACT = new hydra.core.Name("extract");
  
  public static final hydra.core.Name FIELD_NAME_FORMAT_DATETIME = new hydra.core.Name("format_datetime");
  
  public static final hydra.core.Name FIELD_NAME_MATERIALIZE = new hydra.core.Name("materialize");
  
  public static final hydra.core.Name FIELD_NAME_NOW = new hydra.core.Name("now");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public static final hydra.core.Name FIELD_NAME_STARTOFDAY = new hydra.core.Name("startofday");
  
  public static final hydra.core.Name FIELD_NAME_STRCAT = new hydra.core.Name("strcat");
  
  public static final hydra.core.Name FIELD_NAME_TODYNAMIC = new hydra.core.Name("todynamic");
  
  private BuiltInFunction () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ago instance) ;
    
    R visit(Bin instance) ;
    
    R visit(Count instance) ;
    
    R visit(Dcount instance) ;
    
    R visit(Endofday instance) ;
    
    R visit(Extract instance) ;
    
    R visit(Format_datetime instance) ;
    
    R visit(Materialize instance) ;
    
    R visit(Now instance) ;
    
    R visit(Range instance) ;
    
    R visit(Startofday instance) ;
    
    R visit(Strcat instance) ;
    
    R visit(Todynamic instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BuiltInFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Ago instance) {
      return otherwise((instance));
    }
    
    default R visit(Bin instance) {
      return otherwise((instance));
    }
    
    default R visit(Count instance) {
      return otherwise((instance));
    }
    
    default R visit(Dcount instance) {
      return otherwise((instance));
    }
    
    default R visit(Endofday instance) {
      return otherwise((instance));
    }
    
    default R visit(Extract instance) {
      return otherwise((instance));
    }
    
    default R visit(Format_datetime instance) {
      return otherwise((instance));
    }
    
    default R visit(Materialize instance) {
      return otherwise((instance));
    }
    
    default R visit(Now instance) {
      return otherwise((instance));
    }
    
    default R visit(Range instance) {
      return otherwise((instance));
    }
    
    default R visit(Startofday instance) {
      return otherwise((instance));
    }
    
    default R visit(Strcat instance) {
      return otherwise((instance));
    }
    
    default R visit(Todynamic instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ago extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Ago () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ago)) {
        return false;
      }
      Ago o = (Ago) (other);
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
  
  public static final class Bin extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Bin () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bin)) {
        return false;
      }
      Bin o = (Bin) (other);
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
  
  public static final class Count extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Count () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Count)) {
        return false;
      }
      Count o = (Count) (other);
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
  
  public static final class Dcount extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Dcount () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dcount)) {
        return false;
      }
      Dcount o = (Dcount) (other);
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
  
  public static final class Endofday extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Endofday () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Endofday)) {
        return false;
      }
      Endofday o = (Endofday) (other);
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
  
  public static final class Extract extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Extract () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Extract)) {
        return false;
      }
      Extract o = (Extract) (other);
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
  
  public static final class Format_datetime extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Format_datetime () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Format_datetime)) {
        return false;
      }
      Format_datetime o = (Format_datetime) (other);
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
  
  public static final class Materialize extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Materialize () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Materialize)) {
        return false;
      }
      Materialize o = (Materialize) (other);
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
  
  public static final class Now extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Now () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Now)) {
        return false;
      }
      Now o = (Now) (other);
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
  
  public static final class Range extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Range () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Range)) {
        return false;
      }
      Range o = (Range) (other);
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
  
  public static final class Startofday extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Startofday () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Startofday)) {
        return false;
      }
      Startofday o = (Startofday) (other);
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
  
  public static final class Strcat extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Strcat () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Strcat)) {
        return false;
      }
      Strcat o = (Strcat) (other);
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
  
  public static final class Todynamic extends hydra.langs.kusto.kql.BuiltInFunction implements Serializable {
    public Todynamic () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Todynamic)) {
        return false;
      }
      Todynamic o = (Todynamic) (other);
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