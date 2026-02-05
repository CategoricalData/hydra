// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A term-level definition, including a name, a term, and the type scheme of the term
 */
public class TermDefinition implements Serializable, Comparable<TermDefinition> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.module.TermDefinition");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TERM = new hydra.core.Name("term");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  /**
   * The name of the term
   */
  public final hydra.core.Name name;
  
  /**
   * The term being defined
   */
  public final hydra.core.Term term;
  
  /**
   * The type scheme of the term, including any class constraints
   */
  public final hydra.core.TypeScheme type;
  
  public TermDefinition (hydra.core.Name name, hydra.core.Term term, hydra.core.TypeScheme type) {
    this.name = name;
    this.term = term;
    this.type = type;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermDefinition)) {
      return false;
    }
    TermDefinition o = (TermDefinition) (other);
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.term,
      o.term) && java.util.Objects.equals(
      this.type,
      o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(term) + 5 * java.util.Objects.hashCode(type);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TermDefinition other) {
    int cmp = 0;
    cmp = ((Comparable) (name)).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) (term)).compareTo(other.term);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (type)).compareTo(other.type);
  }
  
  public TermDefinition withName(hydra.core.Name name) {
    return new TermDefinition(name, term, type);
  }
  
  public TermDefinition withTerm(hydra.core.Term term) {
    return new TermDefinition(name, term, type);
  }
  
  public TermDefinition withType(hydra.core.TypeScheme type) {
    return new TermDefinition(name, term, type);
  }
}
