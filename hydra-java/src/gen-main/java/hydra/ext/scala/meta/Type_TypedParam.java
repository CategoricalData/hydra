// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_TypedParam implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.TypedParam");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYP = new hydra.core.Name("typ");
  
  public final hydra.ext.scala.meta.Name name;
  
  public final hydra.ext.scala.meta.Type typ;
  
  public Type_TypedParam (hydra.ext.scala.meta.Name name, hydra.ext.scala.meta.Type typ) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((typ));
    this.name = name;
    this.typ = typ;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_TypedParam)) {
      return false;
    }
    Type_TypedParam o = (Type_TypedParam) (other);
    return name.equals(o.name) && typ.equals(o.typ);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * typ.hashCode();
  }
  
  public Type_TypedParam withName(hydra.ext.scala.meta.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Type_TypedParam(name, typ);
  }
  
  public Type_TypedParam withTyp(hydra.ext.scala.meta.Type typ) {
    java.util.Objects.requireNonNull((typ));
    return new Type_TypedParam(name, typ);
  }
}
