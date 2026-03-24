// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_Interpolate implements Serializable, Comparable<Data_Interpolate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_Interpolate");

  public static final hydra.core.Name PREFIX = new hydra.core.Name("prefix");

  public static final hydra.core.Name PARTS = new hydra.core.Name("parts");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final hydra.ext.scala.syntax.Data_Name prefix;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Lit> parts;

  public final hydra.util.ConsList<hydra.ext.scala.syntax.Data> args;

  public Data_Interpolate (hydra.ext.scala.syntax.Data_Name prefix, hydra.util.ConsList<hydra.ext.scala.syntax.Lit> parts, hydra.util.ConsList<hydra.ext.scala.syntax.Data> args) {
    this.prefix = prefix;
    this.parts = parts;
    this.args = args;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Interpolate)) {
      return false;
    }
    Data_Interpolate o = (Data_Interpolate) other;
    return java.util.Objects.equals(
      this.prefix,
      o.prefix) && java.util.Objects.equals(
      this.parts,
      o.parts) && java.util.Objects.equals(
      this.args,
      o.args);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(prefix) + 3 * java.util.Objects.hashCode(parts) + 5 * java.util.Objects.hashCode(args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_Interpolate other) {
    int cmp = 0;
    cmp = ((Comparable) prefix).compareTo(other.prefix);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) parts).compareTo(other.parts);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) args).compareTo(other.args);
  }

  public Data_Interpolate withPrefix(hydra.ext.scala.syntax.Data_Name prefix) {
    return new Data_Interpolate(prefix, parts, args);
  }

  public Data_Interpolate withParts(hydra.util.ConsList<hydra.ext.scala.syntax.Lit> parts) {
    return new Data_Interpolate(prefix, parts, args);
  }

  public Data_Interpolate withArgs(hydra.util.ConsList<hydra.ext.scala.syntax.Data> args) {
    return new Data_Interpolate(prefix, parts, args);
  }
}
