package hydra.core;

/**
 * A case statement for matching optional terms
 */
public class OptionalCases<M> {
  /**
   * A term provided if the optional value is nothing
   */
  public final hydra.core.Term<M> nothing;
  
  /**
   * A function which is applied of the optional value is non-nothing
   */
  public final hydra.core.Term<M> just;
  
  public OptionalCases (hydra.core.Term<M> nothing, hydra.core.Term<M> just) {
    this.nothing = nothing;
    this.just = just;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OptionalCases)) {
      return false;
    }
    OptionalCases o = (OptionalCases) (other);
    return nothing.equals(o.nothing) && just.equals(o.just);
  }
  
  @Override
  public int hashCode() {
    return 2 * nothing.hashCode() + 3 * just.hashCode();
  }
  
  public OptionalCases withNothing(hydra.core.Term<M> nothing) {
    return new OptionalCases(nothing, just);
  }
  
  public OptionalCases withJust(hydra.core.Term<M> just) {
    return new OptionalCases(nothing, just);
  }
}