// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class Annotation implements Serializable, Comparable<Annotation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Annotation");

  public static final hydra.core.Name PREDICATE = new hydra.core.Name("Predicate");

  public static final hydra.core.Name ALTS = new hydra.core.Name("alts");

  public final hydra.ext.io.shex.syntax.Predicate Predicate;

  public final hydra.ext.io.shex.syntax.Annotation_Alts alts;

  public Annotation (hydra.ext.io.shex.syntax.Predicate Predicate, hydra.ext.io.shex.syntax.Annotation_Alts alts) {
    this.Predicate = Predicate;
    this.alts = alts;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Annotation)) {
      return false;
    }
    Annotation o = (Annotation) other;
    return java.util.Objects.equals(
      this.Predicate,
      o.Predicate) && java.util.Objects.equals(
      this.alts,
      o.alts);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Predicate) + 3 * java.util.Objects.hashCode(alts);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Annotation other) {
    int cmp = 0;
    cmp = ((Comparable) Predicate).compareTo(other.Predicate);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) alts).compareTo(other.alts);
  }

  public Annotation withPredicate(hydra.ext.io.shex.syntax.Predicate Predicate) {
    return new Annotation(Predicate, alts);
  }

  public Annotation withAlts(hydra.ext.io.shex.syntax.Annotation_Alts alts) {
    return new Annotation(Predicate, alts);
  }
}
