// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * A Module ... End block
 */
public class ModuleDefinition implements Serializable, Comparable<ModuleDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.ModuleDefinition");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name SENTENCES = new hydra.core.Name("sentences");

  public final hydra.coq.syntax.Ident name;

  public final java.util.List<hydra.coq.syntax.Sentence> sentences;

  public ModuleDefinition (hydra.coq.syntax.Ident name, java.util.List<hydra.coq.syntax.Sentence> sentences) {
    this.name = name;
    this.sentences = sentences;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ModuleDefinition)) {
      return false;
    }
    ModuleDefinition o = (ModuleDefinition) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.sentences,
      o.sentences);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(sentences);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ModuleDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      sentences,
      other.sentences);
  }

  public ModuleDefinition withName(hydra.coq.syntax.Ident name) {
    return new ModuleDefinition(name, sentences);
  }

  public ModuleDefinition withSentences(java.util.List<hydra.coq.syntax.Sentence> sentences) {
    return new ModuleDefinition(name, sentences);
  }
}
