// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.stacspec.items;

import java.io.Serializable;

public abstract class StacRelationType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/stacspec/items.StacRelationType");
  
  public static final hydra.core.Name FIELD_NAME_SELF = new hydra.core.Name("self");
  
  public static final hydra.core.Name FIELD_NAME_ROOT = new hydra.core.Name("root");
  
  public static final hydra.core.Name FIELD_NAME_PARENT = new hydra.core.Name("parent");
  
  public static final hydra.core.Name FIELD_NAME_COLLECTION = new hydra.core.Name("collection");
  
  public static final hydra.core.Name FIELD_NAME_DERIVED_FROM = new hydra.core.Name("derivedFrom");
  
  private StacRelationType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Self instance) ;
    
    R visit(Root instance) ;
    
    R visit(Parent instance) ;
    
    R visit(Collection instance) ;
    
    R visit(DerivedFrom instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StacRelationType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Self instance) {
      return otherwise((instance));
    }
    
    default R visit(Root instance) {
      return otherwise((instance));
    }
    
    default R visit(Parent instance) {
      return otherwise((instance));
    }
    
    default R visit(Collection instance) {
      return otherwise((instance));
    }
    
    default R visit(DerivedFrom instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * STRONGLY RECOMMENDED. Absolute URL to the Item if it is available at a public URL. This is particularly useful when in a download package that includes metadata, so that the downstream user can know where the data has come from.
   */
  public static final class Self extends hydra.ext.org.stacspec.items.StacRelationType implements Serializable {
    public Self () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Self)) {
        return false;
      }
      Self o = (Self) (other);
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
  
  /**
   * URL to the root STAC entity (Catalog or Collection).
   */
  public static final class Root extends hydra.ext.org.stacspec.items.StacRelationType implements Serializable {
    public Root () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Root)) {
        return false;
      }
      Root o = (Root) (other);
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
  
  /**
   * URL to the parent STAC entity (Catalog or Collection).
   */
  public static final class Parent extends hydra.ext.org.stacspec.items.StacRelationType implements Serializable {
    public Parent () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parent)) {
        return false;
      }
      Parent o = (Parent) (other);
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
  
  /**
   * STRONGLY RECOMMENDED. URL to a Collection. Absolute URLs should be used whenever possible. The referenced Collection is STRONGLY RECOMMENDED to implement the same STAC version as the Item. A link with this rel type is required if the collection field in properties is present.
   */
  public static final class Collection extends hydra.ext.org.stacspec.items.StacRelationType implements Serializable {
    public Collection () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Collection)) {
        return false;
      }
      Collection o = (Collection) (other);
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
  
  /**
   * URL to a STAC Item that was used as input data in the creation of this Item.
   */
  public static final class DerivedFrom extends hydra.ext.org.stacspec.items.StacRelationType implements Serializable {
    public DerivedFrom () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DerivedFrom)) {
        return false;
      }
      DerivedFrom o = (DerivedFrom) (other);
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