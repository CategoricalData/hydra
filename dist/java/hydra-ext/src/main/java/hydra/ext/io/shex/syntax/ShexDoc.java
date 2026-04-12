// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class ShexDoc implements Serializable, Comparable<ShexDoc> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.ShexDoc");

  public static final hydra.core.Name LIST_OF_DIRECTIVE = new hydra.core.Name("listOfDirective");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("Sequence");

  public static final hydra.core.Name PREFIX_DECL = new hydra.core.Name("PrefixDecl");

  public final java.util.List<hydra.ext.io.shex.syntax.Directive> listOfDirective;

  public final hydra.util.Maybe<hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option> Sequence;

  public final hydra.ext.io.shex.syntax.PrefixDecl PrefixDecl;

  public ShexDoc (java.util.List<hydra.ext.io.shex.syntax.Directive> listOfDirective, hydra.util.Maybe<hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option> Sequence, hydra.ext.io.shex.syntax.PrefixDecl PrefixDecl) {
    this.listOfDirective = listOfDirective;
    this.Sequence = Sequence;
    this.PrefixDecl = PrefixDecl;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ShexDoc)) {
      return false;
    }
    ShexDoc o = (ShexDoc) other;
    return java.util.Objects.equals(
      this.listOfDirective,
      o.listOfDirective) && java.util.Objects.equals(
      this.Sequence,
      o.Sequence) && java.util.Objects.equals(
      this.PrefixDecl,
      o.PrefixDecl);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(listOfDirective) + 3 * java.util.Objects.hashCode(Sequence) + 5 * java.util.Objects.hashCode(PrefixDecl);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ShexDoc other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      listOfDirective,
      other.listOfDirective);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      Sequence,
      other.Sequence);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      PrefixDecl,
      other.PrefixDecl);
  }

  public ShexDoc withListOfDirective(java.util.List<hydra.ext.io.shex.syntax.Directive> listOfDirective) {
    return new ShexDoc(listOfDirective, Sequence, PrefixDecl);
  }

  public ShexDoc withSequence(hydra.util.Maybe<hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option> Sequence) {
    return new ShexDoc(listOfDirective, Sequence, PrefixDecl);
  }

  public ShexDoc withPrefixDecl(hydra.ext.io.shex.syntax.PrefixDecl PrefixDecl) {
    return new ShexDoc(listOfDirective, Sequence, PrefixDecl);
  }
}
