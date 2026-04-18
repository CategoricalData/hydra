// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class DestinationPredicate implements Serializable, Comparable<DestinationPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DestinationPredicate");

  public static final hydra.core.Name NODE_REFERENCE = new hydra.core.Name("nodeReference");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public static final hydra.core.Name DESTINATION_OF = new hydra.core.Name("destinationOf");

  public final String nodeReference;

  public final Boolean not;

  public final String destinationOf;

  public DestinationPredicate (String nodeReference, Boolean not, String destinationOf) {
    this.nodeReference = nodeReference;
    this.not = not;
    this.destinationOf = destinationOf;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DestinationPredicate)) {
      return false;
    }
    DestinationPredicate o = (DestinationPredicate) other;
    return java.util.Objects.equals(
      this.nodeReference,
      o.nodeReference) && java.util.Objects.equals(
      this.not,
      o.not) && java.util.Objects.equals(
      this.destinationOf,
      o.destinationOf);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(nodeReference) + 3 * java.util.Objects.hashCode(not) + 5 * java.util.Objects.hashCode(destinationOf);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DestinationPredicate other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      nodeReference,
      other.nodeReference);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      not,
      other.not);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      destinationOf,
      other.destinationOf);
  }

  public DestinationPredicate withNodeReference(String nodeReference) {
    return new DestinationPredicate(nodeReference, not, destinationOf);
  }

  public DestinationPredicate withNot(Boolean not) {
    return new DestinationPredicate(nodeReference, not, destinationOf);
  }

  public DestinationPredicate withDestinationOf(String destinationOf) {
    return new DestinationPredicate(nodeReference, not, destinationOf);
  }
}
