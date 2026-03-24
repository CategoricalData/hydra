// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Xml implements Serializable, Comparable<Data_Xml> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Xml");

  public static final hydra.core.Name PARTS = new hydra.core.Name("parts");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Lit> parts;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Data> args;

  public Data_Xml (hydra.util.ConsList<hydra.ext.scala.syntax.Lit> parts, hydra.util.ConsList<hydra.ext.scala.syntax.Data> args) {
    this.parts = parts;
    this.args = args;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Xml)) {
      return false;
    }
    Data_Xml o = (Data_Xml) other;
    return java.util.Objects.equals(
      this.parts,
      o.parts) && java.util.Objects.equals(
      this.args,
      o.args);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parts) + 3 * java.util.Objects.hashCode(args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Xml other) {
    int cmp = 0;
    cmp = ((Comparable) parts).compareTo(other.parts);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) args).compareTo(other.args);
  }

  public Data_Xml withParts(hydra.util.ConsList<hydra.ext.scala.syntax.Lit> parts) {
    return new Data_Xml(parts, args);
  }

  public Data_Xml withArgs(hydra.util.ConsList<hydra.ext.scala.syntax.Data> args) {
    return new Data_Xml(parts, args);
  }
}
