// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class Annotation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.Annotation");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE = new hydra.core.Name("predicate");
  
  public static final hydra.core.Name FIELD_NAME_ALTS = new hydra.core.Name("alts");
  
  public final hydra.ext.io.shex.syntax.Predicate predicate;
  
  public final hydra.ext.io.shex.syntax.Annotation_Alts alts;
  
  public Annotation (hydra.ext.io.shex.syntax.Predicate predicate, hydra.ext.io.shex.syntax.Annotation_Alts alts) {
    java.util.Objects.requireNonNull((predicate));
    java.util.Objects.requireNonNull((alts));
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
  
  public Annotation withPredicate(hydra.ext.io.shex.syntax.Predicate predicate) {
    java.util.Objects.requireNonNull((predicate));
    return new Annotation(predicate, alts);
  }
  
  public Annotation withAlts(hydra.ext.io.shex.syntax.Annotation_Alts alts) {
    java.util.Objects.requireNonNull((alts));
    return new Annotation(predicate, alts);
  }
}