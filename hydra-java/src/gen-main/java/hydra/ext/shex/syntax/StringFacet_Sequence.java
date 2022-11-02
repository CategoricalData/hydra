package hydra.ext.shex.syntax;

public class StringFacet_Sequence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.StringFacet.Sequence");
  
  public final hydra.ext.shex.syntax.StringLength stringLength;
  
  public final hydra.ext.shex.syntax.Integer_ integer;
  
  public StringFacet_Sequence (hydra.ext.shex.syntax.StringLength stringLength, hydra.ext.shex.syntax.Integer_ integer) {
    this.stringLength = stringLength;
    this.integer = integer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringFacet_Sequence)) {
      return false;
    }
    StringFacet_Sequence o = (StringFacet_Sequence) (other);
    return stringLength.equals(o.stringLength) && integer.equals(o.integer);
  }
  
  @Override
  public int hashCode() {
    return 2 * stringLength.hashCode() + 3 * integer.hashCode();
  }
  
  public StringFacet_Sequence withStringLength(hydra.ext.shex.syntax.StringLength stringLength) {
    return new StringFacet_Sequence(stringLength, integer);
  }
  
  public StringFacet_Sequence withInteger(hydra.ext.shex.syntax.Integer_ integer) {
    return new StringFacet_Sequence(stringLength, integer);
  }
}