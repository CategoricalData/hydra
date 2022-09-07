package hydra.ext.scala.meta;

public class Data_Select {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Data.Select");
  
  public final hydra.ext.scala.meta.Data qual;
  
  public final hydra.ext.scala.meta.Data_Name name;
  
  public Data_Select (hydra.ext.scala.meta.Data qual, hydra.ext.scala.meta.Data_Name name) {
    this.qual = qual;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Select)) {
      return false;
    }
    Data_Select o = (Data_Select) (other);
    return qual.equals(o.qual) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * qual.hashCode() + 3 * name.hashCode();
  }
  
  public Data_Select withQual(hydra.ext.scala.meta.Data qual) {
    return new Data_Select(qual, name);
  }
  
  public Data_Select withName(hydra.ext.scala.meta.Data_Name name) {
    return new Data_Select(qual, name);
  }
}