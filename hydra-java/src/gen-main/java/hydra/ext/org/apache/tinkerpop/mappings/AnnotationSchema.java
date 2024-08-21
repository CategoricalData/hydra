// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.mappings;

import java.io.Serializable;

/**
 * Configurable annotation keys for property graph mapping specifications
 */
public class AnnotationSchema implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/mappings.AnnotationSchema");
  
  public static final hydra.core.Name FIELD_NAME_VERTEX_LABEL = new hydra.core.Name("vertexLabel");
  
  public static final hydra.core.Name FIELD_NAME_EDGE_LABEL = new hydra.core.Name("edgeLabel");
  
  public static final hydra.core.Name FIELD_NAME_VERTEX_ID = new hydra.core.Name("vertexId");
  
  public static final hydra.core.Name FIELD_NAME_EDGE_ID = new hydra.core.Name("edgeId");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY_KEY = new hydra.core.Name("propertyKey");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY_VALUE = new hydra.core.Name("propertyValue");
  
  public static final hydra.core.Name FIELD_NAME_OUT_VERTEX = new hydra.core.Name("outVertex");
  
  public static final hydra.core.Name FIELD_NAME_OUT_VERTEX_LABEL = new hydra.core.Name("outVertexLabel");
  
  public static final hydra.core.Name FIELD_NAME_IN_VERTEX = new hydra.core.Name("inVertex");
  
  public static final hydra.core.Name FIELD_NAME_IN_VERTEX_LABEL = new hydra.core.Name("inVertexLabel");
  
  public static final hydra.core.Name FIELD_NAME_OUT_EDGE = new hydra.core.Name("outEdge");
  
  public static final hydra.core.Name FIELD_NAME_OUT_EDGE_LABEL = new hydra.core.Name("outEdgeLabel");
  
  public static final hydra.core.Name FIELD_NAME_IN_EDGE = new hydra.core.Name("inEdge");
  
  public static final hydra.core.Name FIELD_NAME_IN_EDGE_LABEL = new hydra.core.Name("inEdgeLabel");
  
  public static final hydra.core.Name FIELD_NAME_IGNORE = new hydra.core.Name("ignore");
  
  public final String vertexLabel;
  
  public final String edgeLabel;
  
  public final String vertexId;
  
  public final String edgeId;
  
  public final String propertyKey;
  
  public final String propertyValue;
  
  public final String outVertex;
  
  public final String outVertexLabel;
  
  public final String inVertex;
  
  public final String inVertexLabel;
  
  public final String outEdge;
  
  public final String outEdgeLabel;
  
  public final String inEdge;
  
  public final String inEdgeLabel;
  
  public final String ignore;
  
  public AnnotationSchema (String vertexLabel, String edgeLabel, String vertexId, String edgeId, String propertyKey, String propertyValue, String outVertex, String outVertexLabel, String inVertex, String inVertexLabel, String outEdge, String outEdgeLabel, String inEdge, String inEdgeLabel, String ignore) {
    java.util.Objects.requireNonNull((vertexLabel));
    java.util.Objects.requireNonNull((edgeLabel));
    java.util.Objects.requireNonNull((vertexId));
    java.util.Objects.requireNonNull((edgeId));
    java.util.Objects.requireNonNull((propertyKey));
    java.util.Objects.requireNonNull((propertyValue));
    java.util.Objects.requireNonNull((outVertex));
    java.util.Objects.requireNonNull((outVertexLabel));
    java.util.Objects.requireNonNull((inVertex));
    java.util.Objects.requireNonNull((inVertexLabel));
    java.util.Objects.requireNonNull((outEdge));
    java.util.Objects.requireNonNull((outEdgeLabel));
    java.util.Objects.requireNonNull((inEdge));
    java.util.Objects.requireNonNull((inEdgeLabel));
    java.util.Objects.requireNonNull((ignore));
    this.vertexLabel = vertexLabel;
    this.edgeLabel = edgeLabel;
    this.vertexId = vertexId;
    this.edgeId = edgeId;
    this.propertyKey = propertyKey;
    this.propertyValue = propertyValue;
    this.outVertex = outVertex;
    this.outVertexLabel = outVertexLabel;
    this.inVertex = inVertex;
    this.inVertexLabel = inVertexLabel;
    this.outEdge = outEdge;
    this.outEdgeLabel = outEdgeLabel;
    this.inEdge = inEdge;
    this.inEdgeLabel = inEdgeLabel;
    this.ignore = ignore;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotationSchema)) {
      return false;
    }
    AnnotationSchema o = (AnnotationSchema) (other);
    return vertexLabel.equals(o.vertexLabel) && edgeLabel.equals(o.edgeLabel) && vertexId.equals(o.vertexId) && edgeId.equals(o.edgeId) && propertyKey.equals(o.propertyKey) && propertyValue.equals(o.propertyValue) && outVertex.equals(o.outVertex) && outVertexLabel.equals(o.outVertexLabel) && inVertex.equals(o.inVertex) && inVertexLabel.equals(o.inVertexLabel) && outEdge.equals(o.outEdge) && outEdgeLabel.equals(o.outEdgeLabel) && inEdge.equals(o.inEdge) && inEdgeLabel.equals(o.inEdgeLabel) && ignore.equals(o.ignore);
  }
  
  @Override
  public int hashCode() {
    return 2 * vertexLabel.hashCode() + 3 * edgeLabel.hashCode() + 5 * vertexId.hashCode() + 7 * edgeId.hashCode() + 11 * propertyKey.hashCode() + 13 * propertyValue.hashCode() + 17 * outVertex.hashCode() + 19 * outVertexLabel.hashCode() + 23 * inVertex.hashCode() + 29 * inVertexLabel.hashCode() + 31 * outEdge.hashCode() + 37 * outEdgeLabel.hashCode() + 41 * inEdge.hashCode() + 43 * inEdgeLabel.hashCode() + 47 * ignore.hashCode();
  }
  
  public AnnotationSchema withVertexLabel(String vertexLabel) {
    java.util.Objects.requireNonNull((vertexLabel));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withEdgeLabel(String edgeLabel) {
    java.util.Objects.requireNonNull((edgeLabel));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withVertexId(String vertexId) {
    java.util.Objects.requireNonNull((vertexId));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withEdgeId(String edgeId) {
    java.util.Objects.requireNonNull((edgeId));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withPropertyKey(String propertyKey) {
    java.util.Objects.requireNonNull((propertyKey));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withPropertyValue(String propertyValue) {
    java.util.Objects.requireNonNull((propertyValue));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withOutVertex(String outVertex) {
    java.util.Objects.requireNonNull((outVertex));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withOutVertexLabel(String outVertexLabel) {
    java.util.Objects.requireNonNull((outVertexLabel));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withInVertex(String inVertex) {
    java.util.Objects.requireNonNull((inVertex));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withInVertexLabel(String inVertexLabel) {
    java.util.Objects.requireNonNull((inVertexLabel));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withOutEdge(String outEdge) {
    java.util.Objects.requireNonNull((outEdge));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withOutEdgeLabel(String outEdgeLabel) {
    java.util.Objects.requireNonNull((outEdgeLabel));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withInEdge(String inEdge) {
    java.util.Objects.requireNonNull((inEdge));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withInEdgeLabel(String inEdgeLabel) {
    java.util.Objects.requireNonNull((inEdgeLabel));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withIgnore(String ignore) {
    java.util.Objects.requireNonNull((ignore));
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
}