// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Annotate implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Type_Annotate");
  
  public static final hydra.core.Name FIELD_NAME_TPE = new hydra.core.Name("tpe");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTS = new hydra.core.Name("annots");
  
  public final hydra.ext.scala.meta.Type tpe;
  
  public final java.util.List<hydra.ext.scala.meta.Mod_Annot> annots;
  
  public Type_Annotate (hydra.ext.scala.meta.Type tpe, java.util.List<hydra.ext.scala.meta.Mod_Annot> annots) {
    java.util.Objects.requireNonNull((tpe));
    java.util.Objects.requireNonNull((annots));
    this.tpe = tpe;
    this.annots = annots;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Annotate)) {
      return false;
    }
    Type_Annotate o = (Type_Annotate) (other);
    return tpe.equals(o.tpe) && annots.equals(o.annots);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode() + 3 * annots.hashCode();
  }
  
  public Type_Annotate withTpe(hydra.ext.scala.meta.Type tpe) {
    java.util.Objects.requireNonNull((tpe));
    return new Type_Annotate(tpe, annots);
  }
  
  public Type_Annotate withAnnots(java.util.List<hydra.ext.scala.meta.Mod_Annot> annots) {
    java.util.Objects.requireNonNull((annots));
    return new Type_Annotate(tpe, annots);
  }
}