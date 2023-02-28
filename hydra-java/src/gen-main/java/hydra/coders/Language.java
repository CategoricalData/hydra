package hydra.coders;

/**
 * A named language together with language-specific constraints
 */
public class Language<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/coders.Language");
  
  public final hydra.coders.LanguageName name;
  
  public final hydra.coders.LanguageConstraints<M> constraints;
  
  public Language (hydra.coders.LanguageName name, hydra.coders.LanguageConstraints<M> constraints) {
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
    return new Language(name, constraints);
  }
  
  public Language withConstraints(hydra.coders.LanguageConstraints<M> constraints) {
    return new Language(name, constraints);
  }
}