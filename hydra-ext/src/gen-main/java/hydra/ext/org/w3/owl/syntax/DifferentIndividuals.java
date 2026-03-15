// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DifferentIndividuals implements Serializable, Comparable<DifferentIndividuals> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DifferentIndividuals");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name INDIVIDUALS = new hydra.core.Name("individuals");
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Individual> individuals;
  
  public DifferentIndividuals (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Individual> individuals) {
    this.annotations = annotations;
    this.individuals = individuals;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DifferentIndividuals)) {
      return false;
    }
    DifferentIndividuals o = (DifferentIndividuals) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.individuals,
      o.individuals);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(individuals);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DifferentIndividuals other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      individuals.hashCode(),
      other.individuals.hashCode());
  }
  
  public DifferentIndividuals withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new DifferentIndividuals(annotations, individuals);
  }
  
  public DifferentIndividuals withIndividuals(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Individual> individuals) {
    return new DifferentIndividuals(annotations, individuals);
  }
}
