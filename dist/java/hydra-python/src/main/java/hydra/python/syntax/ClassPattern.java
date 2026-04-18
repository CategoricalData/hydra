// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class ClassPattern implements Serializable, Comparable<ClassPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.ClassPattern");

  public static final hydra.core.Name NAME_OR_ATTRIBUTE = new hydra.core.Name("nameOrAttribute");

  public static final hydra.core.Name POSITIONAL_PATTERNS = new hydra.core.Name("positionalPatterns");

  public static final hydra.core.Name KEYWORD_PATTERNS = new hydra.core.Name("keywordPatterns");

  public final hydra.python.syntax.NameOrAttribute nameOrAttribute;

  public final hydra.util.Maybe<hydra.python.syntax.PositionalPatterns> positionalPatterns;

  public final hydra.util.Maybe<hydra.python.syntax.KeywordPatterns> keywordPatterns;

  public ClassPattern (hydra.python.syntax.NameOrAttribute nameOrAttribute, hydra.util.Maybe<hydra.python.syntax.PositionalPatterns> positionalPatterns, hydra.util.Maybe<hydra.python.syntax.KeywordPatterns> keywordPatterns) {
    this.nameOrAttribute = nameOrAttribute;
    this.positionalPatterns = positionalPatterns;
    this.keywordPatterns = keywordPatterns;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassPattern)) {
      return false;
    }
    ClassPattern o = (ClassPattern) other;
    return java.util.Objects.equals(
      this.nameOrAttribute,
      o.nameOrAttribute) && java.util.Objects.equals(
      this.positionalPatterns,
      o.positionalPatterns) && java.util.Objects.equals(
      this.keywordPatterns,
      o.keywordPatterns);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(nameOrAttribute) + 3 * java.util.Objects.hashCode(positionalPatterns) + 5 * java.util.Objects.hashCode(keywordPatterns);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClassPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      nameOrAttribute,
      other.nameOrAttribute);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      positionalPatterns,
      other.positionalPatterns);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      keywordPatterns,
      other.keywordPatterns);
  }

  public ClassPattern withNameOrAttribute(hydra.python.syntax.NameOrAttribute nameOrAttribute) {
    return new ClassPattern(nameOrAttribute, positionalPatterns, keywordPatterns);
  }

  public ClassPattern withPositionalPatterns(hydra.util.Maybe<hydra.python.syntax.PositionalPatterns> positionalPatterns) {
    return new ClassPattern(nameOrAttribute, positionalPatterns, keywordPatterns);
  }

  public ClassPattern withKeywordPatterns(hydra.util.Maybe<hydra.python.syntax.KeywordPatterns> keywordPatterns) {
    return new ClassPattern(nameOrAttribute, positionalPatterns, keywordPatterns);
  }
}
