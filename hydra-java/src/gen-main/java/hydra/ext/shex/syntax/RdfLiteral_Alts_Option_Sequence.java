package hydra.ext.shex.syntax;

public class RdfLiteral_Alts_Option_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.RdfLiteral.Alts.Option.Sequence");
  
  public final hydra.ext.shex.syntax.Datatype datatype;
  
  public RdfLiteral_Alts_Option_Sequence (hydra.ext.shex.syntax.Datatype datatype) {
    this.datatype = datatype;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RdfLiteral_Alts_Option_Sequence)) {
      return false;
    }
    RdfLiteral_Alts_Option_Sequence o = (RdfLiteral_Alts_Option_Sequence) (other);
    return datatype.equals(o.datatype);
  }
  
  @Override
  public int hashCode() {
    return 2 * datatype.hashCode();
  }
}