// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

import java.io.Serializable;

/**
 * A graph element, having a name, data term (value), and schema term (type)
 */
public class Element implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.graph.Element");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DATA = new hydra.core.Name("data");
  
  public final hydra.core.Name name;
  
  public final hydra.core.Term data;
  
  public Element (hydra.core.Name name, hydra.core.Term data) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((data));
    this.name = name;
    this.data = data;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Element)) {
      return false;
    }
    Element o = (Element) (other);
    return name.equals(o.name) && data.equals(o.data);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * data.hashCode();
  }
  
  public Element withName(hydra.core.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Element(name, data);
  }
  
  public Element withData(hydra.core.Term data) {
    java.util.Objects.requireNonNull((data));
    return new Element(name, data);
  }
}