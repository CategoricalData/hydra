// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class DifferentIndividuals implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.DifferentIndividuals");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_INDIVIDUALS = new hydra.core.Name("individuals");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.ext.owl.syntax.Individual> individuals;
  
  public DifferentIndividuals (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, java.util.List<hydra.ext.owl.syntax.Individual> individuals) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((individuals));
    this.annotations = annotations;
    this.individuals = individuals;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DifferentIndividuals)) {
      return false;
    }
    DifferentIndividuals o = (DifferentIndividuals) (other);
    return annotations.equals(o.annotations) && individuals.equals(o.individuals);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * individuals.hashCode();
  }
  
  public DifferentIndividuals withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new DifferentIndividuals(annotations, individuals);
  }
  
  public DifferentIndividuals withIndividuals(java.util.List<hydra.ext.owl.syntax.Individual> individuals) {
    java.util.Objects.requireNonNull((individuals));
    return new DifferentIndividuals(annotations, individuals);
  }
}
