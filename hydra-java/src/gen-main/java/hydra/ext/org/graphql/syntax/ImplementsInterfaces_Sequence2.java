// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class ImplementsInterfaces_Sequence2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.ImplementsInterfaces_Sequence2");
  
  public static final hydra.core.Name FIELD_NAME_AMP = new hydra.core.Name("amp");
  
  public static final hydra.core.Name FIELD_NAME_NAMED_TYPE = new hydra.core.Name("namedType");
  
  public final hydra.util.Opt<java.lang.Void> amp;
  
  public final hydra.ext.org.graphql.syntax.NamedType namedType;
  
  public ImplementsInterfaces_Sequence2 (hydra.util.Opt<java.lang.Void> amp, hydra.ext.org.graphql.syntax.NamedType namedType) {
    java.util.Objects.requireNonNull((amp));
    java.util.Objects.requireNonNull((namedType));
    this.amp = amp;
    this.namedType = namedType;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ImplementsInterfaces_Sequence2)) {
      return false;
    }
    ImplementsInterfaces_Sequence2 o = (ImplementsInterfaces_Sequence2) (other);
    return amp.equals(o.amp) && namedType.equals(o.namedType);
  }
  
  @Override
  public int hashCode() {
    return 2 * amp.hashCode() + 3 * namedType.hashCode();
  }
  
  public ImplementsInterfaces_Sequence2 withAmp(hydra.util.Opt<java.lang.Void> amp) {
    java.util.Objects.requireNonNull((amp));
    return new ImplementsInterfaces_Sequence2(amp, namedType);
  }
  
  public ImplementsInterfaces_Sequence2 withNamedType(hydra.ext.org.graphql.syntax.NamedType namedType) {
    java.util.Objects.requireNonNull((namedType));
    return new ImplementsInterfaces_Sequence2(amp, namedType);
  }
}