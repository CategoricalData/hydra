// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public class ShexDoc implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShexDoc");
  
  public static final hydra.core.Name FIELD_NAME_LIST_OF_DIRECTIVE = new hydra.core.Name("listOfDirective");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
  public static final hydra.core.Name FIELD_NAME_PREFIX_DECL = new hydra.core.Name("prefixDecl");
  
  public final java.util.List<hydra.ext.shex.syntax.Directive> listOfDirective;
  
  public final hydra.util.Opt<hydra.ext.shex.syntax.ShexDoc_Sequence_Option> sequence;
  
  public final hydra.ext.shex.syntax.PrefixDecl prefixDecl;
  
  public ShexDoc (java.util.List<hydra.ext.shex.syntax.Directive> listOfDirective, hydra.util.Opt<hydra.ext.shex.syntax.ShexDoc_Sequence_Option> sequence, hydra.ext.shex.syntax.PrefixDecl prefixDecl) {
    java.util.Objects.requireNonNull((listOfDirective));
    java.util.Objects.requireNonNull((sequence));
    java.util.Objects.requireNonNull((prefixDecl));
    this.listOfDirective = listOfDirective;
    this.sequence = sequence;
    this.prefixDecl = prefixDecl;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShexDoc)) {
      return false;
    }
    ShexDoc o = (ShexDoc) (other);
    return listOfDirective.equals(o.listOfDirective) && sequence.equals(o.sequence) && prefixDecl.equals(o.prefixDecl);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfDirective.hashCode() + 3 * sequence.hashCode() + 5 * prefixDecl.hashCode();
  }
  
  public ShexDoc withListOfDirective(java.util.List<hydra.ext.shex.syntax.Directive> listOfDirective) {
    java.util.Objects.requireNonNull((listOfDirective));
    return new ShexDoc(listOfDirective, sequence, prefixDecl);
  }
  
  public ShexDoc withSequence(hydra.util.Opt<hydra.ext.shex.syntax.ShexDoc_Sequence_Option> sequence) {
    java.util.Objects.requireNonNull((sequence));
    return new ShexDoc(listOfDirective, sequence, prefixDecl);
  }
  
  public ShexDoc withPrefixDecl(hydra.ext.shex.syntax.PrefixDecl prefixDecl) {
    java.util.Objects.requireNonNull((prefixDecl));
    return new ShexDoc(listOfDirective, sequence, prefixDecl);
  }
}
