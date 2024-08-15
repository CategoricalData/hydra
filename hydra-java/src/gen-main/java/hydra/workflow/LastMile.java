// Note: this is an automatically generated file. Do not edit.

package hydra.workflow;

/**
 * The last mile of a transformation, which encodes and serializes terms to a file
 */
public class LastMile<S, A> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/workflow.LastMile");
  
  public static final hydra.core.Name FIELD_NAME_ENCODER = new hydra.core.Name("encoder");
  
  public static final hydra.core.Name FIELD_NAME_SERIALIZER = new hydra.core.Name("serializer");
  
  public static final hydra.core.Name FIELD_NAME_FILE_EXTENSION = new hydra.core.Name("fileExtension");
  
  /**
   * An encoder for terms to a list of output objects
   */
  public final java.util.function.Function<hydra.core.Type, hydra.compute.Flow<S, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<S, java.util.List<A>>>>>> encoder;
  
  /**
   * A function which serializes a list of output objects to a string representation
   */
  public final java.util.function.Function<java.util.List<A>, hydra.compute.Flow<S, String>> serializer;
  
  /**
   * A file extension for the generated file(s)
   */
  public final String fileExtension;
  
  public LastMile (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<S, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<S, java.util.List<A>>>>>> encoder, java.util.function.Function<java.util.List<A>, hydra.compute.Flow<S, String>> serializer, String fileExtension) {
    java.util.Objects.requireNonNull((encoder));
    java.util.Objects.requireNonNull((serializer));
    java.util.Objects.requireNonNull((fileExtension));
    this.encoder = encoder;
    this.serializer = serializer;
    this.fileExtension = fileExtension;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LastMile)) {
      return false;
    }
    LastMile o = (LastMile) (other);
    return encoder.equals(o.encoder) && serializer.equals(o.serializer) && fileExtension.equals(o.fileExtension);
  }
  
  @Override
  public int hashCode() {
    return 2 * encoder.hashCode() + 3 * serializer.hashCode() + 5 * fileExtension.hashCode();
  }
  
  public LastMile withEncoder(java.util.function.Function<hydra.core.Type, hydra.compute.Flow<S, java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.graph.Graph, hydra.compute.Flow<S, java.util.List<A>>>>>> encoder) {
    java.util.Objects.requireNonNull((encoder));
    return new LastMile(encoder, serializer, fileExtension);
  }
  
  public LastMile withSerializer(java.util.function.Function<java.util.List<A>, hydra.compute.Flow<S, String>> serializer) {
    java.util.Objects.requireNonNull((serializer));
    return new LastMile(encoder, serializer, fileExtension);
  }
  
  public LastMile withFileExtension(String fileExtension) {
    java.util.Objects.requireNonNull((fileExtension));
    return new LastMile(encoder, serializer, fileExtension);
  }
}