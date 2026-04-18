// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A Notation declaration
 */
public class NotationDeclaration implements Serializable, Comparable<NotationDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.NotationDeclaration");

  public static final hydra.core.Name NOTATION = new hydra.core.Name("notation");

  public static final hydra.core.Name DEFINITION = new hydra.core.Name("definition");

  public static final hydra.core.Name LEVEL = new hydra.core.Name("level");

  public static final hydra.core.Name ASSOCIATIVITY = new hydra.core.Name("associativity");

  public final hydra.coq.syntax.String_ notation;

  public final hydra.coq.syntax.Term definition;

  public final hydra.util.Maybe<hydra.coq.syntax.Natural> level;

  public final hydra.util.Maybe<String> associativity;

  public NotationDeclaration (hydra.coq.syntax.String_ notation, hydra.coq.syntax.Term definition, hydra.util.Maybe<hydra.coq.syntax.Natural> level, hydra.util.Maybe<String> associativity) {
    this.notation = notation;
    this.definition = definition;
    this.level = level;
    this.associativity = associativity;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NotationDeclaration)) {
      return false;
    }
    NotationDeclaration o = (NotationDeclaration) other;
    return java.util.Objects.equals(
      this.notation,
      o.notation) && java.util.Objects.equals(
      this.definition,
      o.definition) && java.util.Objects.equals(
      this.level,
      o.level) && java.util.Objects.equals(
      this.associativity,
      o.associativity);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(notation) + 3 * java.util.Objects.hashCode(definition) + 5 * java.util.Objects.hashCode(level) + 7 * java.util.Objects.hashCode(associativity);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NotationDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      notation,
      other.notation);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      definition,
      other.definition);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      level,
      other.level);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      associativity,
      other.associativity);
  }

  public NotationDeclaration withNotation(hydra.coq.syntax.String_ notation) {
    return new NotationDeclaration(notation, definition, level, associativity);
  }

  public NotationDeclaration withDefinition(hydra.coq.syntax.Term definition) {
    return new NotationDeclaration(notation, definition, level, associativity);
  }

  public NotationDeclaration withLevel(hydra.util.Maybe<hydra.coq.syntax.Natural> level) {
    return new NotationDeclaration(notation, definition, level, associativity);
  }

  public NotationDeclaration withAssociativity(hydra.util.Maybe<String> associativity) {
    return new NotationDeclaration(notation, definition, level, associativity);
  }
}
