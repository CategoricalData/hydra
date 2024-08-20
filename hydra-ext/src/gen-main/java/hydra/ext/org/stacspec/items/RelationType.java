// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.stacspec.items;

import java.io.Serializable;

/**
 * STAC Items use a variety of rel types in the link object, to describe the exact nature of the link between this Item and the entity it is linking to. It is recommended to use the official IANA Link Relation Types where possible. The following table explains places where STAC use custom rel types are used with Items. This happens where there is not a clear official option, or where STAC uses an official type but adds additional meaning for the STAC context.
 */
public abstract class RelationType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/stacspec/items.RelationType");
  
  public static final hydra.core.Name FIELD_NAME_IANA = new hydra.core.Name("iana");
  
  public static final hydra.core.Name FIELD_NAME_STAC = new hydra.core.Name("stac");
  
  public static final hydra.core.Name FIELD_NAME_OTHER = new hydra.core.Name("other");
  
  private RelationType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Iana instance) ;
    
    R visit(Stac instance) ;
    
    R visit(Other instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RelationType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Iana instance) {
      return otherwise((instance));
    }
    
    default R visit(Stac instance) {
      return otherwise((instance));
    }
    
    default R visit(Other instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Iana extends hydra.ext.org.stacspec.items.RelationType implements Serializable {
    public final hydra.ext.org.iana.linkrelations.LinkRelationType value;
    
    public Iana (hydra.ext.org.iana.linkrelations.LinkRelationType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Iana)) {
        return false;
      }
      Iana o = (Iana) (other);
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
  
  public static final class Stac extends hydra.ext.org.stacspec.items.RelationType implements Serializable {
    public final hydra.ext.org.stacspec.items.StacRelationType value;
    
    public Stac (hydra.ext.org.stacspec.items.StacRelationType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Stac)) {
        return false;
      }
      Stac o = (Stac) (other);
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
  
  public static final class Other extends hydra.ext.org.stacspec.items.RelationType implements Serializable {
    public final String value;
    
    public Other (String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Other)) {
        return false;
      }
      Other o = (Other) (other);
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