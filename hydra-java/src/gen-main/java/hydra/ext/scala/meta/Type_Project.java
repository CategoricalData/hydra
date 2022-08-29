package hydra.ext.scala.meta;

public class Type_Project {
  public final hydra.ext.scala.meta.Type qual;
  
  public final hydra.ext.scala.meta.Type_Name name;
  
  public Type_Project (hydra.ext.scala.meta.Type qual, hydra.ext.scala.meta.Type_Name name) {
    this.qual = qual;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Project)) {
      return false;
    }
    Type_Project o = (Type_Project) (other);
    return qual.equals(o.qual) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * qual.hashCode() + 3 * name.hashCode();
  }
  
  public Type_Project withQual(hydra.ext.scala.meta.Type qual) {
    return new Type_Project(qual, name);
  }
  
  public Type_Project withName(hydra.ext.scala.meta.Type_Name name) {
    return new Type_Project(qual, name);
  }
}