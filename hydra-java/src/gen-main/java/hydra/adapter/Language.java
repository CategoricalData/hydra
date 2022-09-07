package hydra.adapter;

public class Language<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/adapter.Language");
  
  public final hydra.adapter.LanguageName name;
  
  public final hydra.adapter.LanguageConstraints<M> constraints;
  
  public Language (hydra.adapter.LanguageName name, hydra.adapter.LanguageConstraints<M> constraints) {
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
  
  public Language withName(hydra.adapter.LanguageName name) {
    return new Language(name, constraints);
  }
  
  public Language withConstraints(hydra.adapter.LanguageConstraints<M> constraints) {
    return new Language(name, constraints);
  }
}