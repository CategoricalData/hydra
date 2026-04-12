// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class TryWithResourcesStatement implements Serializable, Comparable<TryWithResourcesStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.java.syntax.TryWithResourcesStatement");

  public static final hydra.core.Name RESOURCE_SPECIFICATION = new hydra.core.Name("resourceSpecification");

  public static final hydra.core.Name BLOCK = new hydra.core.Name("block");

  public static final hydra.core.Name CATCHES = new hydra.core.Name("catches");

  public static final hydra.core.Name FINALLY = new hydra.core.Name("finally");

  public final hydra.ext.java.syntax.ResourceSpecification resourceSpecification;

  public final hydra.ext.java.syntax.Block block;

  public final hydra.util.Maybe<hydra.ext.java.syntax.Catches> catches;

  public final hydra.util.Maybe<hydra.ext.java.syntax.Finally> finally_;

  public TryWithResourcesStatement (hydra.ext.java.syntax.ResourceSpecification resourceSpecification, hydra.ext.java.syntax.Block block, hydra.util.Maybe<hydra.ext.java.syntax.Catches> catches, hydra.util.Maybe<hydra.ext.java.syntax.Finally> finally_) {
    this.resourceSpecification = resourceSpecification;
    this.block = block;
    this.catches = catches;
    this.finally_ = finally_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TryWithResourcesStatement)) {
      return false;
    }
    TryWithResourcesStatement o = (TryWithResourcesStatement) other;
    return java.util.Objects.equals(
      this.resourceSpecification,
      o.resourceSpecification) && java.util.Objects.equals(
      this.block,
      o.block) && java.util.Objects.equals(
      this.catches,
      o.catches) && java.util.Objects.equals(
      this.finally_,
      o.finally_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(resourceSpecification) + 3 * java.util.Objects.hashCode(block) + 5 * java.util.Objects.hashCode(catches) + 7 * java.util.Objects.hashCode(finally_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TryWithResourcesStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      resourceSpecification,
      other.resourceSpecification);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      block,
      other.block);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      catches,
      other.catches);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      finally_,
      other.finally_);
  }

  public TryWithResourcesStatement withResourceSpecification(hydra.ext.java.syntax.ResourceSpecification resourceSpecification) {
    return new TryWithResourcesStatement(resourceSpecification, block, catches, finally_);
  }

  public TryWithResourcesStatement withBlock(hydra.ext.java.syntax.Block block) {
    return new TryWithResourcesStatement(resourceSpecification, block, catches, finally_);
  }

  public TryWithResourcesStatement withCatches(hydra.util.Maybe<hydra.ext.java.syntax.Catches> catches) {
    return new TryWithResourcesStatement(resourceSpecification, block, catches, finally_);
  }

  public TryWithResourcesStatement withFinally(hydra.util.Maybe<hydra.ext.java.syntax.Finally> finally_) {
    return new TryWithResourcesStatement(resourceSpecification, block, catches, finally_);
  }
}
