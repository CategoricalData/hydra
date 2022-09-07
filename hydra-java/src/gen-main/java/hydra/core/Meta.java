package hydra.core;

/**
 * A built-in metadata container for terms
 */
public class Meta {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Meta");
  
  /**
   * A map of annotation names to annotation values
   */
  public final java.util.Map<String, hydra.core.Term<hydra.core.Meta>> annotations;
  
  public Meta (java.util.Map<String, hydra.core.Term<hydra.core.Meta>> annotations) {
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Meta)) {
      return false;
    }
    Meta o = (Meta) (other);
    return annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode();
  }
}