// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Select implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Select");
  
  public static final hydra.core.Name FIELD_NAME_QUAL = new hydra.core.Name("qual");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.ext.scala.meta.Data_Ref qual;
  
  public final hydra.ext.scala.meta.Type_Name name;
  
  public Type_Select (hydra.ext.scala.meta.Data_Ref qual, hydra.ext.scala.meta.Type_Name name) {
    java.util.Objects.requireNonNull((qual));
    java.util.Objects.requireNonNull((name));
    this.qual = qual;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Select)) {
      return false;
    }
    Type_Select o = (Type_Select) (other);
    return qual.equals(o.qual) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * qual.hashCode() + 3 * name.hashCode();
  }
  
  public Type_Select withQual(hydra.ext.scala.meta.Data_Ref qual) {
    java.util.Objects.requireNonNull((qual));
    return new Type_Select(qual, name);
  }
  
  public Type_Select withName(hydra.ext.scala.meta.Type_Name name) {
    java.util.Objects.requireNonNull((name));
    return new Type_Select(qual, name);
  }
}
