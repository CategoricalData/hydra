// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Type_Annotate implements Serializable, Comparable<Type_Annotate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_Annotate");

  public static final hydra.core.Name TPE = new hydra.core.Name("tpe");

  public static final hydra.core.Name ANNOTS = new hydra.core.Name("annots");

  public final hydra.scala.syntax.Type tpe;

  public final java.util.List<hydra.scala.syntax.Mod_Annot> annots;

  public Type_Annotate (hydra.scala.syntax.Type tpe, java.util.List<hydra.scala.syntax.Mod_Annot> annots) {
    this.tpe = tpe;
    this.annots = annots;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Annotate)) {
      return false;
    }
    Type_Annotate o = (Type_Annotate) other;
    return java.util.Objects.equals(
      this.tpe,
      o.tpe) && java.util.Objects.equals(
      this.annots,
      o.annots);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(tpe) + 3 * java.util.Objects.hashCode(annots);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Annotate other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      tpe,
      other.tpe);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      annots,
      other.annots);
  }

  public Type_Annotate withTpe(hydra.scala.syntax.Type tpe) {
    return new Type_Annotate(tpe, annots);
  }

  public Type_Annotate withAnnots(java.util.List<hydra.scala.syntax.Mod_Annot> annots) {
    return new Type_Annotate(tpe, annots);
  }
}
