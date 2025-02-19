// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A case statement for matching optional terms
 */
public class OptionalCases implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.OptionalCases");
  
  public static final hydra.core.Name FIELD_NAME_NOTHING = new hydra.core.Name("nothing");
  
  public static final hydra.core.Name FIELD_NAME_JUST = new hydra.core.Name("just");
  
  /**
   * A term provided if the optional value is nothing
   */
  public final hydra.core.Term nothing;
  
  /**
   * A function which is applied if the optional value is non-nothing
   */
  public final hydra.core.Term just;
  
  public OptionalCases (hydra.core.Term nothing, hydra.core.Term just) {
    java.util.Objects.requireNonNull((nothing));
    java.util.Objects.requireNonNull((just));
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
  
  public OptionalCases withNothing(hydra.core.Term nothing) {
    java.util.Objects.requireNonNull((nothing));
    return new OptionalCases(nothing, just);
  }
  
  public OptionalCases withJust(hydra.core.Term just) {
    java.util.Objects.requireNonNull((just));
    return new OptionalCases(nothing, just);
  }
}