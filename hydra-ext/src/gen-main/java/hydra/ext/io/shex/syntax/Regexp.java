// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class Regexp implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.Regexp");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_ALTS = new hydra.core.Name("listOfAlts");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_REGEX = new hydra.core.Name("listOfRegex");
  
  public final java.util.List<hydra.ext.io.shex.syntax.Regexp_ListOfAlts_Elmt> listOfAlts;
  
  public final java.util.List<String> listOfRegex;
  
  public Regexp (java.util.List<hydra.ext.io.shex.syntax.Regexp_ListOfAlts_Elmt> listOfAlts, java.util.List<String> listOfRegex) {
    java.util.Objects.requireNonNull((listOfAlts));
    java.util.Objects.requireNonNull((listOfRegex));
    this.listOfAlts = listOfAlts;
    this.listOfRegex = listOfRegex;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Regexp)) {
      return false;
    }
    Regexp o = (Regexp) (other);
    return listOfAlts.equals(o.listOfAlts) && listOfRegex.equals(o.listOfRegex);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfAlts.hashCode() + 3 * listOfRegex.hashCode();
  }
  
  public Regexp withListOfAlts(java.util.List<hydra.ext.io.shex.syntax.Regexp_ListOfAlts_Elmt> listOfAlts) {
    java.util.Objects.requireNonNull((listOfAlts));
    return new Regexp(listOfAlts, listOfRegex);
  }
  
  public Regexp withListOfRegex(java.util.List<String> listOfRegex) {
    java.util.Objects.requireNonNull((listOfRegex));
    return new Regexp(listOfAlts, listOfRegex);
  }
}