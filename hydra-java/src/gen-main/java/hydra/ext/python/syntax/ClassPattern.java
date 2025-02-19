// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ClassPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ClassPattern");
  
  public static final hydra.core.Name FIELD_NAME_NAME_OR_ATTRIBUTE = new hydra.core.Name("nameOrAttribute");
  
  public static final hydra.core.Name FIELD_NAME_POSITIONAL_PATTERNS = new hydra.core.Name("positionalPatterns");
  
  public static final hydra.core.Name FIELD_NAME_KEYWORD_PATTERNS = new hydra.core.Name("keywordPatterns");
  
  public final hydra.ext.python.syntax.NameOrAttribute nameOrAttribute;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.PositionalPatterns> positionalPatterns;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.KeywordPatterns> keywordPatterns;
  
  public ClassPattern (hydra.ext.python.syntax.NameOrAttribute nameOrAttribute, hydra.util.Opt<hydra.ext.python.syntax.PositionalPatterns> positionalPatterns, hydra.util.Opt<hydra.ext.python.syntax.KeywordPatterns> keywordPatterns) {
    java.util.Objects.requireNonNull((nameOrAttribute));
    java.util.Objects.requireNonNull((positionalPatterns));
    java.util.Objects.requireNonNull((keywordPatterns));
    this.nameOrAttribute = nameOrAttribute;
    this.positionalPatterns = positionalPatterns;
    this.keywordPatterns = keywordPatterns;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassPattern)) {
      return false;
    }
    ClassPattern o = (ClassPattern) (other);
    return nameOrAttribute.equals(o.nameOrAttribute) && positionalPatterns.equals(o.positionalPatterns) && keywordPatterns.equals(o.keywordPatterns);
  }
  
  @Override
  public int hashCode() {
    return 2 * nameOrAttribute.hashCode() + 3 * positionalPatterns.hashCode() + 5 * keywordPatterns.hashCode();
  }
  
  public ClassPattern withNameOrAttribute(hydra.ext.python.syntax.NameOrAttribute nameOrAttribute) {
    java.util.Objects.requireNonNull((nameOrAttribute));
    return new ClassPattern(nameOrAttribute, positionalPatterns, keywordPatterns);
  }
  
  public ClassPattern withPositionalPatterns(hydra.util.Opt<hydra.ext.python.syntax.PositionalPatterns> positionalPatterns) {
    java.util.Objects.requireNonNull((positionalPatterns));
    return new ClassPattern(nameOrAttribute, positionalPatterns, keywordPatterns);
  }
  
  public ClassPattern withKeywordPatterns(hydra.util.Opt<hydra.ext.python.syntax.KeywordPatterns> keywordPatterns) {
    java.util.Objects.requireNonNull((keywordPatterns));
    return new ClassPattern(nameOrAttribute, positionalPatterns, keywordPatterns);
  }
}