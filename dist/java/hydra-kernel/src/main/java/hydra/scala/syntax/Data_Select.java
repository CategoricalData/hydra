// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Data_Select implements Serializable, Comparable<Data_Select> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Data_Select");

  public static final hydra.core.Name QUAL = new hydra.core.Name("qual");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public final hydra.scala.syntax.Data qual;

  public final hydra.scala.syntax.Data_Name name;

  public Data_Select (hydra.scala.syntax.Data qual, hydra.scala.syntax.Data_Name name) {
    this.qual = qual;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Select)) {
      return false;
    }
    Data_Select o = (Data_Select) other;
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
  public int compareTo(Data_Select other) {
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

  public Data_Select withQual(hydra.scala.syntax.Data qual) {
    return new Data_Select(qual, name);
  }

  public Data_Select withName(hydra.scala.syntax.Data_Name name) {
    return new Data_Select(qual, name);
  }
}
