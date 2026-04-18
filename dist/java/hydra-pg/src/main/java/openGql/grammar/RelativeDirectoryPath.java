// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class RelativeDirectoryPath implements Serializable, Comparable<RelativeDirectoryPath> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.RelativeDirectoryPath");

  public static final hydra.core.Name PARENT_DIRECTORIES = new hydra.core.Name("parentDirectories");

  public static final hydra.core.Name SIMPLE_PATH = new hydra.core.Name("simplePath");

  public final Integer parentDirectories;

  public final hydra.util.Maybe<java.util.List<String>> simplePath;

  public RelativeDirectoryPath (Integer parentDirectories, hydra.util.Maybe<java.util.List<String>> simplePath) {
    this.parentDirectories = parentDirectories;
    this.simplePath = simplePath;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelativeDirectoryPath)) {
      return false;
    }
    RelativeDirectoryPath o = (RelativeDirectoryPath) other;
    return java.util.Objects.equals(
      this.parentDirectories,
      o.parentDirectories) && java.util.Objects.equals(
      this.simplePath,
      o.simplePath);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parentDirectories) + 3 * java.util.Objects.hashCode(simplePath);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RelativeDirectoryPath other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      parentDirectories,
      other.parentDirectories);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      simplePath,
      other.simplePath);
  }

  public RelativeDirectoryPath withParentDirectories(Integer parentDirectories) {
    return new RelativeDirectoryPath(parentDirectories, simplePath);
  }

  public RelativeDirectoryPath withSimplePath(hydra.util.Maybe<java.util.List<String>> simplePath) {
    return new RelativeDirectoryPath(parentDirectories, simplePath);
  }
}
