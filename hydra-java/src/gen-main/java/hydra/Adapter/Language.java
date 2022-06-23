package hydra.adapter;

public class Language<M> {
  public final LanguageName name;
  
  public final LanguageConstraints<M> constraints;
  
  public Language (LanguageName name, LanguageConstraints<M> constraints) {
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
  
  public Language withName(LanguageName name) {
    return new Language(name, constraints);
  }
  
  public Language withConstraints(LanguageConstraints<M> constraints) {
    return new Language(name, constraints);
  }
}