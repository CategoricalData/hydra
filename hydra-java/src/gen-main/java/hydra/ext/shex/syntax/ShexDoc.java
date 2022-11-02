package hydra.ext.shex.syntax;

public class ShexDoc {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ShexDoc");
  
  public final java.util.List<hydra.ext.shex.syntax.Directive> listOfDirective;
  
  public final java.util.Optional<hydra.ext.shex.syntax.ShexDoc_Sequence_Option> sequence;
  
  public final hydra.ext.shex.syntax.PrefixDecl prefixDecl;
  
  public ShexDoc (java.util.List<hydra.ext.shex.syntax.Directive> listOfDirective, java.util.Optional<hydra.ext.shex.syntax.ShexDoc_Sequence_Option> sequence, hydra.ext.shex.syntax.PrefixDecl prefixDecl) {
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
    return new ShexDoc(listOfDirective, sequence, prefixDecl);
  }
  
  public ShexDoc withSequence(java.util.Optional<hydra.ext.shex.syntax.ShexDoc_Sequence_Option> sequence) {
    return new ShexDoc(listOfDirective, sequence, prefixDecl);
  }
  
  public ShexDoc withPrefixDecl(hydra.ext.shex.syntax.PrefixDecl prefixDecl) {
    return new ShexDoc(listOfDirective, sequence, prefixDecl);
  }
}