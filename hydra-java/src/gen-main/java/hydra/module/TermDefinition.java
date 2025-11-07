// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A term-level definition, including a name, a term, and the type of the term
 */
public class TermDefinition implements Serializable {
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
   * The type of the term
   */
  public final hydra.core.Type type;
  
  public TermDefinition (hydra.core.Name name, hydra.core.Term term, hydra.core.Type type) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((term));
    java.util.Objects.requireNonNull((type));
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
    return name.equals(o.name) && term.equals(o.term) && type.equals(o.type);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * term.hashCode() + 5 * type.hashCode();
  }
  
  public TermDefinition withName(hydra.core.Name name) {
    java.util.Objects.requireNonNull((name));
    return new TermDefinition(name, term, type);
  }
  
  public TermDefinition withTerm(hydra.core.Term term) {
    java.util.Objects.requireNonNull((term));
    return new TermDefinition(name, term, type);
  }
  
  public TermDefinition withType(hydra.core.Type type) {
    java.util.Objects.requireNonNull((type));
    return new TermDefinition(name, term, type);
  }
}
