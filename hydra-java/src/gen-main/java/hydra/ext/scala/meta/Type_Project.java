// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Type_Project implements Serializable, Comparable<Type_Project> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Type_Project");

  public static final hydra.core.Name QUAL = new hydra.core.Name("qual");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

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
    Type_Project o = (Type_Project) other;
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
  public int compareTo(Type_Project other) {
    int cmp = 0;
    cmp = ((Comparable) qual).compareTo(other.qual);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public Type_Project withQual(hydra.ext.scala.meta.Type qual) {
    return new Type_Project(qual, name);
  }

  public Type_Project withName(hydra.ext.scala.meta.Type_Name name) {
    return new Type_Project(qual, name);
  }
}
