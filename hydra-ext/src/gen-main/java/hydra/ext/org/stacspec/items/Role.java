// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.stacspec.items;

import java.io.Serializable;

/**
 * The roles field is used to describe the purpose of each asset. It is recommended to include one for every asset, to give users a sense of why they might want to make use of the asset. There are some emerging standards that enable clients to take particular action when they encounter particular roles, listed below. But implementors are encouraged to come up with their own terms to describe the role.
 */
public abstract class Role implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.stacspec.items.Role");
  
  public static final hydra.core.Name FIELD_NAME_THUMBNAIL = new hydra.core.Name("thumbnail");
  
  public static final hydra.core.Name FIELD_NAME_OVERVIEW = new hydra.core.Name("overview");
  
  public static final hydra.core.Name FIELD_NAME_DATA = new hydra.core.Name("data");
  
  public static final hydra.core.Name FIELD_NAME_METADATA = new hydra.core.Name("metadata");
  
  public static final hydra.core.Name FIELD_NAME_OTHER = new hydra.core.Name("other");
  
  private Role () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Thumbnail instance) ;
    
    R visit(Overview instance) ;
    
    R visit(Data instance) ;
    
    R visit(Metadata instance) ;
    
    R visit(Other instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Role instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Thumbnail instance) {
      return otherwise((instance));
    }
    
    default R visit(Overview instance) {
      return otherwise((instance));
    }
    
    default R visit(Data instance) {
      return otherwise((instance));
    }
    
    default R visit(Metadata instance) {
      return otherwise((instance));
    }
    
    default R visit(Other instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * An asset that represents a thumbnail of the Item, typically a true color image (for Items with assets in the visible wavelengths), lower-resolution (typically smaller 600x600 pixels), and typically a JPEG or PNG (suitable for display in a web browser). Multiple assets may have this purpose, but it recommended that the type and roles be unique tuples. For example, Sentinel-2 L2A provides thumbnail images in both JPEG and JPEG2000 formats, and would be distinguished by their media types.
   */
  public static final class Thumbnail extends hydra.ext.org.stacspec.items.Role implements Serializable {
    public Thumbnail () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Thumbnail)) {
        return false;
      }
      Thumbnail o = (Thumbnail) (other);
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
   * An asset that represents a possibly larger view than the thumbnail of the Item, for example, a true color composite of multi-band data.
   */
  public static final class Overview extends hydra.ext.org.stacspec.items.Role implements Serializable {
    public Overview () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Overview)) {
        return false;
      }
      Overview o = (Overview) (other);
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
   * The data itself. This is a suggestion for a common role for data files to be used in case data providers don't come up with their own names and semantics.
   */
  public static final class Data extends hydra.ext.org.stacspec.items.Role implements Serializable {
    public Data () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Data)) {
        return false;
      }
      Data o = (Data) (other);
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
   * A metadata sidecar file describing the data in this Item, for example the Landsat-8 MTL file.
   */
  public static final class Metadata extends hydra.ext.org.stacspec.items.Role implements Serializable {
    public Metadata () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Metadata)) {
        return false;
      }
      Metadata o = (Metadata) (other);
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
  
  public static final class Other extends hydra.ext.org.stacspec.items.Role implements Serializable {
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