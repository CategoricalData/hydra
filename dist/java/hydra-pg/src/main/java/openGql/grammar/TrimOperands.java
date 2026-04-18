// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TrimOperands implements Serializable, Comparable<TrimOperands> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TrimOperands");

  public static final hydra.core.Name SPECIFICATION = new hydra.core.Name("specification");

  public static final hydra.core.Name CHARACTER_OR_BYTE_STRING = new hydra.core.Name("characterOrByteString");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public final hydra.util.Maybe<openGql.grammar.TrimSpecification> specification;

  public final hydra.util.Maybe<openGql.grammar.ValueExpression> characterOrByteString;

  public final openGql.grammar.ValueExpression source;

  public TrimOperands (hydra.util.Maybe<openGql.grammar.TrimSpecification> specification, hydra.util.Maybe<openGql.grammar.ValueExpression> characterOrByteString, openGql.grammar.ValueExpression source) {
    this.specification = specification;
    this.characterOrByteString = characterOrByteString;
    this.source = source;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TrimOperands)) {
      return false;
    }
    TrimOperands o = (TrimOperands) other;
    return java.util.Objects.equals(
      this.specification,
      o.specification) && java.util.Objects.equals(
      this.characterOrByteString,
      o.characterOrByteString) && java.util.Objects.equals(
      this.source,
      o.source);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(specification) + 3 * java.util.Objects.hashCode(characterOrByteString) + 5 * java.util.Objects.hashCode(source);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TrimOperands other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      specification,
      other.specification);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      characterOrByteString,
      other.characterOrByteString);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      source,
      other.source);
  }

  public TrimOperands withSpecification(hydra.util.Maybe<openGql.grammar.TrimSpecification> specification) {
    return new TrimOperands(specification, characterOrByteString, source);
  }

  public TrimOperands withCharacterOrByteString(hydra.util.Maybe<openGql.grammar.ValueExpression> characterOrByteString) {
    return new TrimOperands(specification, characterOrByteString, source);
  }

  public TrimOperands withSource(openGql.grammar.ValueExpression source) {
    return new TrimOperands(specification, characterOrByteString, source);
  }
}
