package hydra.compute;

public class AdapterContext<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/compute.AdapterContext");
  
  public final hydra.compute.Context<M> evaluation;
  
  public final hydra.compute.Language<M> source;
  
  public final hydra.compute.Language<M> target;
  
  public AdapterContext (hydra.compute.Context<M> evaluation, hydra.compute.Language<M> source, hydra.compute.Language<M> target) {
    this.evaluation = evaluation;
    this.source = source;
    this.target = target;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdapterContext)) {
      return false;
    }
    AdapterContext o = (AdapterContext) (other);
    return evaluation.equals(o.evaluation) && source.equals(o.source) && target.equals(o.target);
  }
  
  @Override
  public int hashCode() {
    return 2 * evaluation.hashCode() + 3 * source.hashCode() + 5 * target.hashCode();
  }
  
  public AdapterContext withEvaluation(hydra.compute.Context<M> evaluation) {
    return new AdapterContext(evaluation, source, target);
  }
  
  public AdapterContext withSource(hydra.compute.Language<M> source) {
    return new AdapterContext(evaluation, source, target);
  }
  
  public AdapterContext withTarget(hydra.compute.Language<M> target) {
    return new AdapterContext(evaluation, source, target);
  }
}