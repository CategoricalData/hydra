// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ShexDoc_Sequence_Option implements Serializable, Comparable<ShexDoc_Sequence_Option> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option");
  
  public static final hydra.core.Name ALTS = new hydra.core.Name("alts");
  
  public static final hydra.core.Name LIST_OF_STATEMENT = new hydra.core.Name("listOfStatement");
  
  public final hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts alts;
  
  public final java.util.List<hydra.ext.io.shex.syntax.Statement> listOfStatement;
  
  public ShexDoc_Sequence_Option (hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts alts, java.util.List<hydra.ext.io.shex.syntax.Statement> listOfStatement) {
    this.alts = alts;
    this.listOfStatement = listOfStatement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShexDoc_Sequence_Option)) {
      return false;
    }
    ShexDoc_Sequence_Option o = (ShexDoc_Sequence_Option) other;
    return java.util.Objects.equals(
      this.alts,
      o.alts) && java.util.Objects.equals(
      this.listOfStatement,
      o.listOfStatement);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(alts) + 3 * java.util.Objects.hashCode(listOfStatement);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ShexDoc_Sequence_Option other) {
    int cmp = 0;
    cmp = ((Comparable) alts).compareTo(other.alts);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      listOfStatement.hashCode(),
      other.listOfStatement.hashCode());
  }
  
  public ShexDoc_Sequence_Option withAlts(hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts alts) {
    return new ShexDoc_Sequence_Option(alts, listOfStatement);
  }
  
  public ShexDoc_Sequence_Option withListOfStatement(java.util.List<hydra.ext.io.shex.syntax.Statement> listOfStatement) {
    return new ShexDoc_Sequence_Option(alts, listOfStatement);
  }
}
