// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class AwaitPrimary implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.AwaitPrimary");
  
  public static final hydra.core.Name FIELD_NAME_AWAIT = new hydra.core.Name("await");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public final Boolean await;
  
  public final hydra.ext.python.syntax.Primary primary;
  
  public AwaitPrimary (Boolean await, hydra.ext.python.syntax.Primary primary) {
    java.util.Objects.requireNonNull((await));
    java.util.Objects.requireNonNull((primary));
    this.await = await;
    this.primary = primary;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AwaitPrimary)) {
      return false;
    }
    AwaitPrimary o = (AwaitPrimary) (other);
    return await.equals(o.await) && primary.equals(o.primary);
  }
  
  @Override
  public int hashCode() {
    return 2 * await.hashCode() + 3 * primary.hashCode();
  }
  
  public AwaitPrimary withAwait(Boolean await) {
    java.util.Objects.requireNonNull((await));
    return new AwaitPrimary(await, primary);
  }
  
  public AwaitPrimary withPrimary(hydra.ext.python.syntax.Primary primary) {
    java.util.Objects.requireNonNull((primary));
    return new AwaitPrimary(await, primary);
  }
}