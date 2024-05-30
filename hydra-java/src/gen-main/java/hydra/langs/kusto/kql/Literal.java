package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class Literal implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Literal");
  
  private Literal () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Duration instance) ;
    
    R visit(Datetime instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Int instance) ;
    
    R visit(Long_ instance) ;
    
    R visit(Double_ instance) ;
    
    R visit(Boolean_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Literal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Duration instance) {
      return otherwise((instance));
    }
    
    default R visit(Datetime instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Int instance) {
      return otherwise((instance));
    }
    
    default R visit(Long_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Duration extends hydra.langs.kusto.kql.Literal implements Serializable {
    public final hydra.langs.kusto.kql.Duration value;
    
    public Duration (hydra.langs.kusto.kql.Duration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Duration)) {
        return false;
      }
      Duration o = (Duration) (other);
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
  
  public static final class Datetime extends hydra.langs.kusto.kql.Literal implements Serializable {
    public final hydra.langs.kusto.kql.Datetime value;
    
    public Datetime (hydra.langs.kusto.kql.Datetime value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Datetime)) {
        return false;
      }
      Datetime o = (Datetime) (other);
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
  
  public static final class String_ extends hydra.langs.kusto.kql.Literal implements Serializable {
    public final String value;
    
    public String_ (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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
  
  public static final class Int extends hydra.langs.kusto.kql.Literal implements Serializable {
    public final Integer value;
    
    public Int (Integer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) (other);
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
  
  public static final class Long_ extends hydra.langs.kusto.kql.Literal implements Serializable {
    public final Long value;
    
    public Long_ (Long value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Long_)) {
        return false;
      }
      Long_ o = (Long_) (other);
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
  
  public static final class Double_ extends hydra.langs.kusto.kql.Literal implements Serializable {
    public final Double value;
    
    public Double_ (Double value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Double_)) {
        return false;
      }
      Double_ o = (Double_) (other);
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
  
  public static final class Boolean_ extends hydra.langs.kusto.kql.Literal implements Serializable {
    public final Boolean value;
    
    public Boolean_ (Boolean value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Boolean_)) {
        return false;
      }
      Boolean_ o = (Boolean_) (other);
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