// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class TypeCase implements Serializable, Comparable<TypeCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.TypeCase");

  public static final hydra.core.Name PAT = new hydra.core.Name("pat");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.scala.syntax.Type pat;

  public final hydra.scala.syntax.Type body;

  public TypeCase (hydra.scala.syntax.Type pat, hydra.scala.syntax.Type body) {
    this.pat = pat;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeCase)) {
      return false;
    }
    TypeCase o = (TypeCase) other;
    return java.util.Objects.equals(
      this.pat,
      o.pat) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pat) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeCase other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pat,
      other.pat);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public TypeCase withPat(hydra.scala.syntax.Type pat) {
    return new TypeCase(pat, body);
  }

  public TypeCase withBody(hydra.scala.syntax.Type body) {
    return new TypeCase(pat, body);
  }
}
