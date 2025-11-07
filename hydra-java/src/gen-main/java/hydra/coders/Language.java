// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

import java.io.Serializable;

/**
 * A named language together with language-specific constraints
 */
public class Language implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.coders.Language");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRAINTS = new hydra.core.Name("constraints");
  
  /**
   * The unique name of the language
   */
  public final hydra.coders.LanguageName name;
  
  /**
   * The constraints which characterize the language
   */
  public final hydra.coders.LanguageConstraints constraints;
  
  public Language (hydra.coders.LanguageName name, hydra.coders.LanguageConstraints constraints) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((constraints));
    this.name = name;
    this.constraints = constraints;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Language)) {
      return false;
    }
    Language o = (Language) (other);
    return name.equals(o.name) && constraints.equals(o.constraints);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * constraints.hashCode();
  }
  
  public Language withName(hydra.coders.LanguageName name) {
    java.util.Objects.requireNonNull((name));
    return new Language(name, constraints);
  }
  
  public Language withConstraints(hydra.coders.LanguageConstraints constraints) {
    java.util.Objects.requireNonNull((constraints));
    return new Language(name, constraints);
  }
}
