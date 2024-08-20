// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Project implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Project");
  
  public static final hydra.core.Name FIELD_NAME_QUAL = new hydra.core.Name("qual");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.ext.scala.meta.Type qual;
  
  public final hydra.ext.scala.meta.Type_Name name;
  
  public Type_Project (hydra.ext.scala.meta.Type qual, hydra.ext.scala.meta.Type_Name name) {
    java.util.Objects.requireNonNull((qual));
    java.util.Objects.requireNonNull((name));
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
    java.util.Objects.requireNonNull((qual));
    return new Type_Project(qual, name);
  }
  
  public Type_Project withName(hydra.ext.scala.meta.Type_Name name) {
    java.util.Objects.requireNonNull((name));
    return new Type_Project(qual, name);
  }
}
