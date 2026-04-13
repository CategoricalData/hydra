// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Type_Select implements Serializable, Comparable<Type_Select> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Type_Select");

  public static final hydra.core.Name QUAL = new hydra.core.Name("qual");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.scala.syntax.Data_Ref qual;

  public final hydra.scala.syntax.Type_Name name;

  public Type_Select (hydra.scala.syntax.Data_Ref qual, hydra.scala.syntax.Type_Name name) {
    this.qual = qual;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Select)) {
      return false;
    }
    Type_Select o = (Type_Select) other;
    return java.util.Objects.equals(
      this.qual,
      o.qual) && java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qual) + 3 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Type_Select other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      qual,
      other.qual);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }

  public Type_Select withQual(hydra.scala.syntax.Data_Ref qual) {
    return new Type_Select(qual, name);
  }

  public Type_Select withName(hydra.scala.syntax.Type_Name name) {
    return new Type_Select(qual, name);
  }
}
