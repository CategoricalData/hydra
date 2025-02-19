// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.stacspec.items;

import java.io.Serializable;

/**
 * This object describes a relationship with another entity. Data providers are advised to be liberal with the links section, to describe things like the Catalog an Item is in, related Items, parent or child Items (modeled in different ways, like an 'acquisition' or derived data). It is allowed to add additional fields such as a title and type.
 */
public class Link implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.stacspec.items.Link");
  
  public static final hydra.core.Name FIELD_NAME_HREF = new hydra.core.Name("href");
  
  public static final hydra.core.Name FIELD_NAME_REL = new hydra.core.Name("rel");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_TITLE = new hydra.core.Name("title");
  
  /**
   * The actual link in the format of an URL. Relative and absolute links are both allowed.
   */
  public final hydra.ext.org.stacspec.items.Url href;
  
  /**
   * Relationship between the current document and the linked document. See chapter "Relation types" for more information.
   */
  public final hydra.ext.org.stacspec.items.RelationType rel;
  
  /**
   * Media type of the referenced entity.
   */
  public final hydra.util.Opt<hydra.ext.org.stacspec.items.MediaType> type;
  
  /**
   * A human readable title to be used in rendered displays of the link.
   */
  public final hydra.util.Opt<String> title;
  
  public Link (hydra.ext.org.stacspec.items.Url href, hydra.ext.org.stacspec.items.RelationType rel, hydra.util.Opt<hydra.ext.org.stacspec.items.MediaType> type, hydra.util.Opt<String> title) {
    java.util.Objects.requireNonNull((href));
    java.util.Objects.requireNonNull((rel));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((title));
    this.href = href;
    this.rel = rel;
    this.type = type;
    this.title = title;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Link)) {
      return false;
    }
    Link o = (Link) (other);
    return href.equals(o.href) && rel.equals(o.rel) && type.equals(o.type) && title.equals(o.title);
  }
  
  @Override
  public int hashCode() {
    return 2 * href.hashCode() + 3 * rel.hashCode() + 5 * type.hashCode() + 7 * title.hashCode();
  }
  
  public Link withHref(hydra.ext.org.stacspec.items.Url href) {
    java.util.Objects.requireNonNull((href));
    return new Link(href, rel, type, title);
  }
  
  public Link withRel(hydra.ext.org.stacspec.items.RelationType rel) {
    java.util.Objects.requireNonNull((rel));
    return new Link(href, rel, type, title);
  }
  
  public Link withType(hydra.util.Opt<hydra.ext.org.stacspec.items.MediaType> type) {
    java.util.Objects.requireNonNull((type));
    return new Link(href, rel, type, title);
  }
  
  public Link withTitle(hydra.util.Opt<String> title) {
    java.util.Objects.requireNonNull((title));
    return new Link(href, rel, type, title);
  }
}