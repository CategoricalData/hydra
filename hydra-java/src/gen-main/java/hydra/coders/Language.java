// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

/**
 * A named language together with language-specific constraints
 */
public class Language<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/coders.Language");
  
  public final hydra.coders.LanguageName name;
  
  public final hydra.coders.LanguageConstraints<A> constraints;
  
  public Language (hydra.coders.LanguageName name, hydra.coders.LanguageConstraints<A> constraints) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (constraints == null) {
      throw new IllegalArgumentException("null value for 'constraints' argument");
    }
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
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new Language(name, constraints);
  }
  
  public Language withConstraints(hydra.coders.LanguageConstraints<A> constraints) {
    if (constraints == null) {
      throw new IllegalArgumentException("null value for 'constraints' argument");
    }
    return new Language(name, constraints);
  }
}