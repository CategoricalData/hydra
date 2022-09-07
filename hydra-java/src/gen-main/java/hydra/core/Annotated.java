package hydra.core;

public class Annotated<A, M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Annotated");
  
  public final A subject;
  
  public final M annotation;
  
  public Annotated (A subject, M annotation) {
    this.subject = subject;
    this.annotation = annotation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Annotated)) {
      return false;
    }
    Annotated o = (Annotated) (other);
    return subject.equals(o.subject) && annotation.equals(o.annotation);
  }
  
  @Override
  public int hashCode() {
    return 2 * subject.hashCode() + 3 * annotation.hashCode();
  }
  
  public Annotated withSubject(A subject) {
    return new Annotated(subject, annotation);
  }
  
  public Annotated withAnnotation(M annotation) {
    return new Annotated(subject, annotation);
  }
}