package hydra.ext.scala.meta;

public class Type_Annotate {
  public final hydra.ext.scala.meta.Type tpe;
  
  public final java.util.List<hydra.ext.scala.meta.Mod_Annot> annots;
  
  public Type_Annotate (hydra.ext.scala.meta.Type tpe, java.util.List<hydra.ext.scala.meta.Mod_Annot> annots) {
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
    return new Type_Annotate(tpe, annots);
  }
  
  public Type_Annotate withAnnots(java.util.List<hydra.ext.scala.meta.Mod_Annot> annots) {
    return new Type_Annotate(tpe, annots);
  }
}