package hydra.evaluation;

/**
 * A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms
 */
public class TermCoder<M, A> {
  public final hydra.core.Type<M> type;
  
  public final Coder<Context<M>, hydra.core.Term<M>, A> coder;
  
  public TermCoder (hydra.core.Type<M> type, Coder<Context<M>, hydra.core.Term<M>, A> coder) {
    this.type = type;
    this.coder = coder;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TermCoder)) {
      return false;
    }
    TermCoder o = (TermCoder) (other);
    return type.equals(o.type) && coder.equals(o.coder);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * coder.hashCode();
  }
  
  public TermCoder withType(hydra.core.Type<M> type) {
    return new TermCoder(type, coder);
  }
  
  public TermCoder withCoder(Coder<Context<M>, hydra.core.Term<M>, A> coder) {
    return new TermCoder(type, coder);
  }
}