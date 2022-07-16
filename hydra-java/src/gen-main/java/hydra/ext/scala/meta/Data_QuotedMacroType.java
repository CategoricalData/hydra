package hydra.ext.scala.meta;

public class Data_QuotedMacroType {
  public final Type tpe;
  
  public Data_QuotedMacroType (Type tpe) {
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_QuotedMacroType)) {
      return false;
    }
    Data_QuotedMacroType o = (Data_QuotedMacroType) (other);
    return tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode();
  }
}