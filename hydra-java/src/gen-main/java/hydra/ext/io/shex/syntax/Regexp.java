// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class Regexp implements Serializable, Comparable<Regexp> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Regexp");

  public static final hydra.core.Name LIST_OF_ALTS = new hydra.core.Name("listOfAlts");

  public static final hydra.core.Name LIST_OF_REGEX = new hydra.core.Name("listOfRegex");

  public final hydra.util.ConsList<hydra.ext.io.shex.syntax.Regexp_ListOfAlts_Elmt> listOfAlts;

  public final hydra.util.ConsList<String> listOfRegex;

  public Regexp (hydra.util.ConsList<hydra.ext.io.shex.syntax.Regexp_ListOfAlts_Elmt> listOfAlts, hydra.util.ConsList<String> listOfRegex) {
    this.listOfAlts = listOfAlts;
    this.listOfRegex = listOfRegex;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Regexp)) {
      return false;
    }
    Regexp o = (Regexp) other;
    return java.util.Objects.equals(
      this.listOfAlts,
      o.listOfAlts) && java.util.Objects.equals(
      this.listOfRegex,
      o.listOfRegex);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(listOfAlts) + 3 * java.util.Objects.hashCode(listOfRegex);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Regexp other) {
    int cmp = 0;
    cmp = ((Comparable) listOfAlts).compareTo(other.listOfAlts);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) listOfRegex).compareTo(other.listOfRegex);
  }

  public Regexp withListOfAlts(hydra.util.ConsList<hydra.ext.io.shex.syntax.Regexp_ListOfAlts_Elmt> listOfAlts) {
    return new Regexp(listOfAlts, listOfRegex);
  }

  public Regexp withListOfRegex(hydra.util.ConsList<String> listOfRegex) {
    return new Regexp(listOfAlts, listOfRegex);
  }
}
