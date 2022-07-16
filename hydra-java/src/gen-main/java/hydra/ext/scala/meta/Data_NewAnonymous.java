package hydra.ext.scala.meta;

public class Data_NewAnonymous {
  public final Template templ;
  
  public Data_NewAnonymous (Template templ) {
    this.templ = templ;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_NewAnonymous)) {
      return false;
    }
    Data_NewAnonymous o = (Data_NewAnonymous) (other);
    return templ.equals(o.templ);
  }
  
  @Override
  public int hashCode() {
    return 2 * templ.hashCode();
  }
}