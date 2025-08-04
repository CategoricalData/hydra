// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

import java.io.Serializable;

/**
 * A graph element, having a name, data term (value), and schema term (type)
 */
public class Element implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.graph.Element");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public final hydra.core.Name name;
  
  public final hydra.core.Term term;
  
  public final hydra.util.Opt<hydra.core.TypeScheme> type;
  
  public Element (hydra.core.Name name, hydra.core.Term term, hydra.util.Opt<hydra.core.TypeScheme> type) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((type));
    this.name = name;
    this.term = term;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Element)) {
      return false;
    }
    Element o = (Element) (other);
    return name.equals(o.name) && term.equals(o.term) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * term.hashCode() + 5 * type.hashCode();
  }
  
  public Element withName(hydra.core.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Element(name, term, type);
  }
  
  public Element withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new Element(name, term, type);
  }
  
  public Element withType(hydra.util.Opt<hydra.core.TypeScheme> type) {
    java.util.Objects.requireNonNull((type));
    return new Element(name, term, type);
  }
}
