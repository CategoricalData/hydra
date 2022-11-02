package hydra.ext.shex.syntax;

public class Annotation {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.Annotation");
  
  public final hydra.ext.shex.syntax.Predicate predicate;
  
  public final hydra.ext.shex.syntax.Annotation_Alts alts;
  
  public Annotation (hydra.ext.shex.syntax.Predicate predicate, hydra.ext.shex.syntax.Annotation_Alts alts) {
    this.predicate = predicate;
    this.alts = alts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Annotation)) {
      return false;
    }
    Annotation o = (Annotation) (other);
    return predicate.equals(o.predicate) && alts.equals(o.alts);
  }
  
  @Override
  public int hashCode() {
    return 2 * predicate.hashCode() + 3 * alts.hashCode();
  }
  
  public Annotation withPredicate(hydra.ext.shex.syntax.Predicate predicate) {
    return new Annotation(predicate, alts);
  }
  
  public Annotation withAlts(hydra.ext.shex.syntax.Annotation_Alts alts) {
    return new Annotation(predicate, alts);
  }
}