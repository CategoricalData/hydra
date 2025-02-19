// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.stacspec.items;

import java.io.Serializable;

/**
 * This object describes a STAC Item. The fields id, type, bbox, geometry and properties are inherited from GeoJSON.
 */
public class Item implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.stacspec.items.Item");
  
  public static final hydra.core.Name FIELD_NAME_FEATURE = new hydra.core.Name("feature");
  
  public static final hydra.core.Name FIELD_NAME_STAC_VERSION = new hydra.core.Name("stacVersion");
  
  public static final hydra.core.Name FIELD_NAME_STAC_EXTENSIONS = new hydra.core.Name("stacExtensions");
  
  public static final hydra.core.Name FIELD_NAME_LINKS = new hydra.core.Name("links");
  
  public static final hydra.core.Name FIELD_NAME_ASSETS = new hydra.core.Name("assets");
  
  public static final hydra.core.Name FIELD_NAME_COLLECTION = new hydra.core.Name("collection");
  
  public final hydra.ext.org.geojson.model.Feature feature;
  
  /**
   * The STAC version the Item implements
   */
  public final hydra.ext.org.stacspec.items.StacVersion stacVersion;
  
  /**
   * A list of extensions the Item implements
   */
  public final java.util.List<hydra.ext.org.stacspec.items.Url> stacExtensions;
  
  /**
   * List of link objects to resources and related URLs. A link with the rel set to self is strongly recommended.
   */
  public final java.util.List<hydra.ext.org.stacspec.items.Link> links;
  
  /**
   * Dictionary of asset objects that can be downloaded, each with a unique key.
   */
  public final java.util.Map<String, hydra.ext.org.stacspec.items.Asset> assets;
  
  /**
   * The id of the STAC Collection this Item references to (see collection relation type). This field is required if such a relation type is present and is not allowed otherwise. This field provides an easy way for a user to search for any Items that belong in a specified Collection. Must be a non-empty string.
   */
  public final hydra.util.Opt<hydra.ext.org.geojson.model.Id> collection;
  
  public Item (hydra.ext.org.geojson.model.Feature feature, hydra.ext.org.stacspec.items.StacVersion stacVersion, java.util.List<hydra.ext.org.stacspec.items.Url> stacExtensions, java.util.List<hydra.ext.org.stacspec.items.Link> links, java.util.Map<String, hydra.ext.org.stacspec.items.Asset> assets, hydra.util.Opt<hydra.ext.org.geojson.model.Id> collection) {
    java.util.Objects.requireNonNull((feature));
    java.util.Objects.requireNonNull((stacVersion));
    java.util.Objects.requireNonNull((stacExtensions));
    java.util.Objects.requireNonNull((links));
    java.util.Objects.requireNonNull((assets));
    java.util.Objects.requireNonNull((collection));
    this.feature = feature;
    this.stacVersion = stacVersion;
    this.stacExtensions = stacExtensions;
    this.links = links;
    this.assets = assets;
    this.collection = collection;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Item)) {
      return false;
    }
    Item o = (Item) (other);
    return feature.equals(o.feature) && stacVersion.equals(o.stacVersion) && stacExtensions.equals(o.stacExtensions) && links.equals(o.links) && assets.equals(o.assets) && collection.equals(o.collection);
  }
  
  @Override
  public int hashCode() {
    return 2 * feature.hashCode() + 3 * stacVersion.hashCode() + 5 * stacExtensions.hashCode() + 7 * links.hashCode() + 11 * assets.hashCode() + 13 * collection.hashCode();
  }
  
  public Item withFeature(hydra.ext.org.geojson.model.Feature feature) {
    java.util.Objects.requireNonNull((feature));
    return new Item(feature, stacVersion, stacExtensions, links, assets, collection);
  }
  
  public Item withStacVersion(hydra.ext.org.stacspec.items.StacVersion stacVersion) {
    java.util.Objects.requireNonNull((stacVersion));
    return new Item(feature, stacVersion, stacExtensions, links, assets, collection);
  }
  
  public Item withStacExtensions(java.util.List<hydra.ext.org.stacspec.items.Url> stacExtensions) {
    java.util.Objects.requireNonNull((stacExtensions));
    return new Item(feature, stacVersion, stacExtensions, links, assets, collection);
  }
  
  public Item withLinks(java.util.List<hydra.ext.org.stacspec.items.Link> links) {
    java.util.Objects.requireNonNull((links));
    return new Item(feature, stacVersion, stacExtensions, links, assets, collection);
  }
  
  public Item withAssets(java.util.Map<String, hydra.ext.org.stacspec.items.Asset> assets) {
    java.util.Objects.requireNonNull((assets));
    return new Item(feature, stacVersion, stacExtensions, links, assets, collection);
  }
  
  public Item withCollection(hydra.util.Opt<hydra.ext.org.geojson.model.Id> collection) {
    java.util.Objects.requireNonNull((collection));
    return new Item(feature, stacVersion, stacExtensions, links, assets, collection);
  }
}