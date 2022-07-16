package hydra.ext.scala.meta;

public class Data_Annotate {
  public final Data expr;
  
  public final java.util.List<Mod_Annot> annots;
  
  public Data_Annotate (Data expr, java.util.List<Mod_Annot> annots) {
    this.expr = expr;
    this.annots = annots;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Annotate)) {
      return false;
    }
    Data_Annotate o = (Data_Annotate) (other);
    return expr.equals(o.expr) && annots.equals(o.annots);
  }
  
  @Override
  public int hashCode() {
    return 2 * expr.hashCode() + 3 * annots.hashCode();
  }
  
  public Data_Annotate withExpr(Data expr) {
    return new Data_Annotate(expr, annots);
  }
  
  public Data_Annotate withAnnots(java.util.List<Mod_Annot> annots) {
    return new Data_Annotate(expr, annots);
  }
}