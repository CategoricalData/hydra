// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class SameIndividual implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.SameIndividual");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.langs.owl.syntax.Individual> individuals;
  
  public SameIndividual (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, java.util.List<hydra.langs.owl.syntax.Individual> individuals) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (individuals == null) {
      throw new IllegalArgumentException("null value for 'individuals' argument");
    }
    this.annotations = annotations;
    this.individuals = individuals;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SameIndividual)) {
      return false;
    }
    SameIndividual o = (SameIndividual) (other);
    return annotations.equals(o.annotations) && individuals.equals(o.individuals);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * individuals.hashCode();
  }
  
  public SameIndividual withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new SameIndividual(annotations, individuals);
  }
  
  public SameIndividual withIndividuals(java.util.List<hydra.langs.owl.syntax.Individual> individuals) {
    if (individuals == null) {
      throw new IllegalArgumentException("null value for 'individuals' argument");
    }
    return new SameIndividual(annotations, individuals);
  }
}