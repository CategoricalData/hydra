// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class StringFacet_Sequence implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.StringFacet_Sequence");
  
  public static final hydra.core.Name FIELD_NAME_STRING_LENGTH = new hydra.core.Name("stringLength");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER = new hydra.core.Name("integer");
  
  public final hydra.ext.io.shex.syntax.StringLength stringLength;
  
  public final hydra.ext.io.shex.syntax.Integer_ integer;
  
  public StringFacet_Sequence (hydra.ext.io.shex.syntax.StringLength stringLength, hydra.ext.io.shex.syntax.Integer_ integer) {
    java.util.Objects.requireNonNull((stringLength));
    java.util.Objects.requireNonNull((integer));
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
  
  public StringFacet_Sequence withStringLength(hydra.ext.io.shex.syntax.StringLength stringLength) {
    java.util.Objects.requireNonNull((stringLength));
    return new StringFacet_Sequence(stringLength, integer);
  }
  
  public StringFacet_Sequence withInteger(hydra.ext.io.shex.syntax.Integer_ integer) {
    java.util.Objects.requireNonNull((integer));
    return new StringFacet_Sequence(stringLength, integer);
  }
}