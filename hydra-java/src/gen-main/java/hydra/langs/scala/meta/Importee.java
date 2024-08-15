// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public abstract class Importee implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Importee");
  
  public static final hydra.core.Name FIELD_NAME_WILDCARD = new hydra.core.Name("wildcard");
  
  public static final hydra.core.Name FIELD_NAME_GIVEN = new hydra.core.Name("given");
  
  public static final hydra.core.Name FIELD_NAME_GIVEN_ALL = new hydra.core.Name("givenAll");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_RENAME = new hydra.core.Name("rename");
  
  public static final hydra.core.Name FIELD_NAME_UNIMPORT = new hydra.core.Name("unimport");
  
  private Importee () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Wildcard instance) ;
    
    R visit(Given instance) ;
    
    R visit(GivenAll instance) ;
    
    R visit(Name instance) ;
    
    R visit(Rename instance) ;
    
    R visit(Unimport instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Importee instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Wildcard instance) {
      return otherwise((instance));
    }
    
    default R visit(Given instance) {
      return otherwise((instance));
    }
    
    default R visit(GivenAll instance) {
      return otherwise((instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(Rename instance) {
      return otherwise((instance));
    }
    
    default R visit(Unimport instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Wildcard extends hydra.langs.scala.meta.Importee implements Serializable {
    public Wildcard () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wildcard)) {
        return false;
      }
      Wildcard o = (Wildcard) (other);
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
  
  public static final class Given extends hydra.langs.scala.meta.Importee implements Serializable {
    public final hydra.langs.scala.meta.Importee_Given value;
    
    public Given (hydra.langs.scala.meta.Importee_Given value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Given)) {
        return false;
      }
      Given o = (Given) (other);
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
  
  public static final class GivenAll extends hydra.langs.scala.meta.Importee implements Serializable {
    public GivenAll () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GivenAll)) {
        return false;
      }
      GivenAll o = (GivenAll) (other);
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
  
  public static final class Name extends hydra.langs.scala.meta.Importee implements Serializable {
    public final hydra.langs.scala.meta.Importee_Name value;
    
    public Name (hydra.langs.scala.meta.Importee_Name value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) (other);
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
  
  public static final class Rename extends hydra.langs.scala.meta.Importee implements Serializable {
    public final hydra.langs.scala.meta.Importee_Rename value;
    
    public Rename (hydra.langs.scala.meta.Importee_Rename value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rename)) {
        return false;
      }
      Rename o = (Rename) (other);
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
  
  public static final class Unimport extends hydra.langs.scala.meta.Importee implements Serializable {
    public final hydra.langs.scala.meta.Importee_Unimport value;
    
    public Unimport (hydra.langs.scala.meta.Importee_Unimport value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unimport)) {
        return false;
      }
      Unimport o = (Unimport) (other);
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