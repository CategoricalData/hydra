// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ShexDoc_Sequence_Option implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/io/shex/syntax.ShexDoc.Sequence.Option");
  
  public static final hydra.core.Name FIELD_NAME_ALTS = new hydra.core.Name("alts");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_STATEMENT = new hydra.core.Name("listOfStatement");
  
  public final hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts alts;
  
  public final java.util.List<hydra.ext.io.shex.syntax.Statement> listOfStatement;
  
  public ShexDoc_Sequence_Option (hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts alts, java.util.List<hydra.ext.io.shex.syntax.Statement> listOfStatement) {
    java.util.Objects.requireNonNull((alts));
    java.util.Objects.requireNonNull((listOfStatement));
    this.alts = alts;
    this.listOfStatement = listOfStatement;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShexDoc_Sequence_Option)) {
      return false;
    }
    ShexDoc_Sequence_Option o = (ShexDoc_Sequence_Option) (other);
    return alts.equals(o.alts) && listOfStatement.equals(o.listOfStatement);
  }
  
  @Override
  public int hashCode() {
    return 2 * alts.hashCode() + 3 * listOfStatement.hashCode();
  }
  
  public ShexDoc_Sequence_Option withAlts(hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts alts) {
    java.util.Objects.requireNonNull((alts));
    return new ShexDoc_Sequence_Option(alts, listOfStatement);
  }
  
  public ShexDoc_Sequence_Option withListOfStatement(java.util.List<hydra.ext.io.shex.syntax.Statement> listOfStatement) {
    java.util.Objects.requireNonNull((listOfStatement));
    return new ShexDoc_Sequence_Option(alts, listOfStatement);
  }
}