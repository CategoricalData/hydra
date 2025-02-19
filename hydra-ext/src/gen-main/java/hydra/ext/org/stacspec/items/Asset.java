// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.stacspec.items;

import java.io.Serializable;

/**
 * An Asset is an object that contains a URI to data associated with the Item that can be downloaded or streamed. It is allowed to add additional fields.
 */
public class Asset implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.stacspec.items.Asset");
  
  public static final hydra.core.Name FIELD_NAME_HREF = new hydra.core.Name("href");
  
  public static final hydra.core.Name FIELD_NAME_TITLE = new hydra.core.Name("title");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ROLES = new hydra.core.Name("roles");
  
  /**
   * URI to the asset object. Relative and absolute URI are both allowed.
   */
  public final hydra.ext.org.stacspec.items.Uri href;
  
  /**
   * The displayed title for clients and users.
   */
  public final hydra.util.Opt<String> title;
  
  /**
   * A description of the Asset providing additional details, such as how it was processed or created. CommonMark 0.29 syntax MAY be used for rich text representation.
   */
  public final hydra.util.Opt<String> description;
  
  /**
   * Media type of the asset. See the common media types in the best practice doc for commonly used asset types.
   */
  public final hydra.util.Opt<hydra.ext.org.stacspec.items.MediaType> type;
  
  /**
   * The semantic roles of the asset, similar to the use of rel in links.
   */
  public final java.util.List<hydra.ext.org.stacspec.items.Role> roles;
  
  public Asset (hydra.ext.org.stacspec.items.Uri href, hydra.util.Opt<String> title, hydra.util.Opt<String> description, hydra.util.Opt<hydra.ext.org.stacspec.items.MediaType> type, java.util.List<hydra.ext.org.stacspec.items.Role> roles) {
    java.util.Objects.requireNonNull((href));
    java.util.Objects.requireNonNull((title));
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((roles));
    this.href = href;
    this.title = title;
    this.description = description;
    this.type = type;
    this.roles = roles;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Asset)) {
      return false;
    }
    Asset o = (Asset) (other);
    return href.equals(o.href) && title.equals(o.title) && description.equals(o.description) && type.equals(o.type) && roles.equals(o.roles);
  }
  
  @Override
  public int hashCode() {
    return 2 * href.hashCode() + 3 * title.hashCode() + 5 * description.hashCode() + 7 * type.hashCode() + 11 * roles.hashCode();
  }
  
  public Asset withHref(hydra.ext.org.stacspec.items.Uri href) {
    java.util.Objects.requireNonNull((href));
    return new Asset(href, title, description, type, roles);
  }
  
  public Asset withTitle(hydra.util.Opt<String> title) {
    java.util.Objects.requireNonNull((title));
    return new Asset(href, title, description, type, roles);
  }
  
  public Asset withDescription(hydra.util.Opt<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Asset(href, title, description, type, roles);
  }
  
  public Asset withType(hydra.util.Opt<hydra.ext.org.stacspec.items.MediaType> type) {
    java.util.Objects.requireNonNull((type));
    return new Asset(href, title, description, type, roles);
  }
  
  public Asset withRoles(java.util.List<hydra.ext.org.stacspec.items.Role> roles) {
    java.util.Objects.requireNonNull((roles));
    return new Asset(href, title, description, type, roles);
  }
}