// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class SameIndividual implements Serializable, Comparable<SameIndividual> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.SameIndividual");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name INDIVIDUALS = new hydra.core.Name("individuals");
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Individual> individuals;
  
  public SameIndividual (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Individual> individuals) {
    this.annotations = annotations;
    this.individuals = individuals;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SameIndividual)) {
      return false;
    }
    SameIndividual o = (SameIndividual) other;
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
  public int compareTo(SameIndividual other) {
    int cmp = 0;
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) individuals).compareTo(other.individuals);
  }
  
  public SameIndividual withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new SameIndividual(annotations, individuals);
  }
  
  public SameIndividual withIndividuals(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Individual> individuals) {
    return new SameIndividual(annotations, individuals);
  }
}
