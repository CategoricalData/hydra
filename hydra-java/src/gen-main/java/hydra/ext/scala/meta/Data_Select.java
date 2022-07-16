package hydra.ext.scala.meta;

public class Data_Select {
  public final Data qual;
  
  public final Data_Name name;
  
  public Data_Select (Data qual, Data_Name name) {
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
  
  public Data_Select withQual(Data qual) {
    return new Data_Select(qual, name);
  }
  
  public Data_Select withName(Data_Name name) {
    return new Data_Select(qual, name);
  }
}