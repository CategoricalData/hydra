package hydra.ext.java.syntax;

public class Wildcard {
  public final java.util.List<Annotation> annotations;
  
  public final java.util.Optional<WildcardBounds> wildcard;
  
  public Wildcard (java.util.List<Annotation> annotations, java.util.Optional<WildcardBounds> wildcard) {
    this.annotations = annotations;
    this.wildcard = wildcard;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Wildcard)) {
      return false;
    }
    Wildcard o = (Wildcard) (other);
    return annotations.equals(o.annotations) && wildcard.equals(o.wildcard);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * wildcard.hashCode();
  }
  
  public Wildcard withAnnotations(java.util.List<Annotation> annotations) {
    return new Wildcard(annotations, wildcard);
  }
  
  public Wildcard withWildcard(java.util.Optional<WildcardBounds> wildcard) {
    return new Wildcard(annotations, wildcard);
  }
}