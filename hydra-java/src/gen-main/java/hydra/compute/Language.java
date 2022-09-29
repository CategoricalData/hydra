package hydra.compute;

public class Language<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/compute.Language");
  
  public final hydra.compute.LanguageName name;
  
  public final hydra.compute.LanguageConstraints<M> constraints;
  
  public Language (hydra.compute.LanguageName name, hydra.compute.LanguageConstraints<M> constraints) {
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
  
  public Language withName(hydra.compute.LanguageName name) {
    return new Language(name, constraints);
  }
  
  public Language withConstraints(hydra.compute.LanguageConstraints<M> constraints) {
    return new Language(name, constraints);
  }
}