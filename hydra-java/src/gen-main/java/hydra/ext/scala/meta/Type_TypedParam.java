// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_TypedParam implements Serializable, Comparable<Type_TypedParam> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_TypedParam");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYP = new hydra.core.Name("typ");

  public final hydra.ext.scala.meta.Name name;

  public final hydra.ext.scala.meta.Type typ;

  public Type_TypedParam (hydra.ext.scala.meta.Name name, hydra.ext.scala.meta.Type typ) {
    this.name = name;
    this.typ = typ;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_TypedParam)) {
      return false;
    }
    Type_TypedParam o = (Type_TypedParam) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.typ,
      o.typ);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(typ);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_TypedParam other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) typ).compareTo(other.typ);
  }

  public Type_TypedParam withName(hydra.ext.scala.meta.Name name) {
    return new Type_TypedParam(name, typ);
  }

  public Type_TypedParam withTyp(hydra.ext.scala.meta.Type typ) {
    return new Type_TypedParam(name, typ);
  }
}
