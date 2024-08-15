// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public abstract class JoinKind implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/kusto/kql.JoinKind");
  
  public static final hydra.core.Name FIELD_NAME_LEFTOUTER = new hydra.core.Name("leftouter");
  
  public static final hydra.core.Name FIELD_NAME_LEFTSEMI = new hydra.core.Name("leftsemi");
  
  public static final hydra.core.Name FIELD_NAME_LEFTANTI = new hydra.core.Name("leftanti");
  
  public static final hydra.core.Name FIELD_NAME_FULLOUTER = new hydra.core.Name("fullouter");
  
  public static final hydra.core.Name FIELD_NAME_INNER = new hydra.core.Name("inner");
  
  public static final hydra.core.Name FIELD_NAME_INNERUNIQUE = new hydra.core.Name("innerunique");
  
  public static final hydra.core.Name FIELD_NAME_RIGHTOUTER = new hydra.core.Name("rightouter");
  
  public static final hydra.core.Name FIELD_NAME_RIGHTSEMI = new hydra.core.Name("rightsemi");
  
  public static final hydra.core.Name FIELD_NAME_RIGHTANTI = new hydra.core.Name("rightanti");
  
  private JoinKind () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Leftouter instance) ;
    
    R visit(Leftsemi instance) ;
    
    R visit(Leftanti instance) ;
    
    R visit(Fullouter instance) ;
    
    R visit(Inner instance) ;
    
    R visit(Innerunique instance) ;
    
    R visit(Rightouter instance) ;
    
    R visit(Rightsemi instance) ;
    
    R visit(Rightanti instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(JoinKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Leftouter instance) {
      return otherwise((instance));
    }
    
    default R visit(Leftsemi instance) {
      return otherwise((instance));
    }
    
    default R visit(Leftanti instance) {
      return otherwise((instance));
    }
    
    default R visit(Fullouter instance) {
      return otherwise((instance));
    }
    
    default R visit(Inner instance) {
      return otherwise((instance));
    }
    
    default R visit(Innerunique instance) {
      return otherwise((instance));
    }
    
    default R visit(Rightouter instance) {
      return otherwise((instance));
    }
    
    default R visit(Rightsemi instance) {
      return otherwise((instance));
    }
    
    default R visit(Rightanti instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Leftouter extends hydra.langs.kusto.kql.JoinKind implements Serializable {
    public Leftouter () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Leftouter)) {
        return false;
      }
      Leftouter o = (Leftouter) (other);
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
  
  public static final class Leftsemi extends hydra.langs.kusto.kql.JoinKind implements Serializable {
    public Leftsemi () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Leftsemi)) {
        return false;
      }
      Leftsemi o = (Leftsemi) (other);
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
  
  public static final class Leftanti extends hydra.langs.kusto.kql.JoinKind implements Serializable {
    public Leftanti () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Leftanti)) {
        return false;
      }
      Leftanti o = (Leftanti) (other);
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
  
  public static final class Fullouter extends hydra.langs.kusto.kql.JoinKind implements Serializable {
    public Fullouter () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fullouter)) {
        return false;
      }
      Fullouter o = (Fullouter) (other);
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
  
  public static final class Inner extends hydra.langs.kusto.kql.JoinKind implements Serializable {
    public Inner () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inner)) {
        return false;
      }
      Inner o = (Inner) (other);
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
  
  public static final class Innerunique extends hydra.langs.kusto.kql.JoinKind implements Serializable {
    public Innerunique () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Innerunique)) {
        return false;
      }
      Innerunique o = (Innerunique) (other);
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
  
  public static final class Rightouter extends hydra.langs.kusto.kql.JoinKind implements Serializable {
    public Rightouter () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rightouter)) {
        return false;
      }
      Rightouter o = (Rightouter) (other);
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
  
  public static final class Rightsemi extends hydra.langs.kusto.kql.JoinKind implements Serializable {
    public Rightsemi () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rightsemi)) {
        return false;
      }
      Rightsemi o = (Rightsemi) (other);
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
  
  public static final class Rightanti extends hydra.langs.kusto.kql.JoinKind implements Serializable {
    public Rightanti () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rightanti)) {
        return false;
      }
      Rightanti o = (Rightanti) (other);
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