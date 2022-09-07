package hydra.evaluation;

/**
 * Settings which determine how terms are evaluated
 */
public class EvaluationStrategy {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/evaluation.EvaluationStrategy");
  
  public final java.util.Set<hydra.core.TermVariant> opaqueTermVariants;
  
  public EvaluationStrategy (java.util.Set<hydra.core.TermVariant> opaqueTermVariants) {
    this.opaqueTermVariants = opaqueTermVariants;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EvaluationStrategy)) {
      return false;
    }
    EvaluationStrategy o = (EvaluationStrategy) (other);
    return opaqueTermVariants.equals(o.opaqueTermVariants);
  }
  
  @Override
  public int hashCode() {
    return 2 * opaqueTermVariants.hashCode();
  }
}