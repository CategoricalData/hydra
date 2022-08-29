package hydra.adapter;

public class AdapterContext<M> {
  public final hydra.evaluation.Context<M> evaluation;
  
  public final hydra.adapter.Language<M> source;
  
  public final hydra.adapter.Language<M> target;
  
  public AdapterContext (hydra.evaluation.Context<M> evaluation, hydra.adapter.Language<M> source, hydra.adapter.Language<M> target) {
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
  
  public AdapterContext withEvaluation(hydra.evaluation.Context<M> evaluation) {
    return new AdapterContext(evaluation, source, target);
  }
  
  public AdapterContext withSource(hydra.adapter.Language<M> source) {
    return new AdapterContext(evaluation, source, target);
  }
  
  public AdapterContext withTarget(hydra.adapter.Language<M> target) {
    return new AdapterContext(evaluation, source, target);
  }
}