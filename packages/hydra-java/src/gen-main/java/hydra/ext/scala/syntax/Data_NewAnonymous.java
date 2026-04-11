// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Data_NewAnonymous implements Serializable, Comparable<Data_NewAnonymous> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Data_NewAnonymous");

  public static final hydra.core.Name TEMPL = new hydra.core.Name("templ");

  public final hydra.ext.scala.syntax.Template templ;

  public Data_NewAnonymous (hydra.ext.scala.syntax.Template templ) {
    this.templ = templ;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_NewAnonymous)) {
      return false;
    }
    Data_NewAnonymous o = (Data_NewAnonymous) other;
    return java.util.Objects.equals(
      this.templ,
      o.templ);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(templ);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Data_NewAnonymous other) {
    return hydra.util.Comparing.compare(
      templ,
      other.templ);
  }
}
