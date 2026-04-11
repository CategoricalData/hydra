// Note: this is an automatically generated file. Do not edit.

package hydra.pg.mapping;

import java.io.Serializable;

/**
 * Configurable annotation keys for property graph mapping specifications
 */
public class AnnotationSchema implements Serializable, Comparable<AnnotationSchema> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.mapping.AnnotationSchema");

  public static final hydra.core.Name VERTEX_LABEL = new hydra.core.Name("vertexLabel");

  public static final hydra.core.Name EDGE_LABEL = new hydra.core.Name("edgeLabel");

  public static final hydra.core.Name VERTEX_ID = new hydra.core.Name("vertexId");

  public static final hydra.core.Name EDGE_ID = new hydra.core.Name("edgeId");

  public static final hydra.core.Name PROPERTY_KEY = new hydra.core.Name("propertyKey");

  public static final hydra.core.Name PROPERTY_VALUE = new hydra.core.Name("propertyValue");

  public static final hydra.core.Name OUT_VERTEX = new hydra.core.Name("outVertex");

  public static final hydra.core.Name OUT_VERTEX_LABEL = new hydra.core.Name("outVertexLabel");

  public static final hydra.core.Name IN_VERTEX = new hydra.core.Name("inVertex");

  public static final hydra.core.Name IN_VERTEX_LABEL = new hydra.core.Name("inVertexLabel");

  public static final hydra.core.Name OUT_EDGE = new hydra.core.Name("outEdge");

  public static final hydra.core.Name OUT_EDGE_LABEL = new hydra.core.Name("outEdgeLabel");

  public static final hydra.core.Name IN_EDGE = new hydra.core.Name("inEdge");

  public static final hydra.core.Name IN_EDGE_LABEL = new hydra.core.Name("inEdgeLabel");

  public static final hydra.core.Name IGNORE = new hydra.core.Name("ignore");

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
    AnnotationSchema o = (AnnotationSchema) other;
    return java.util.Objects.equals(
      this.vertexLabel,
      o.vertexLabel) && java.util.Objects.equals(
      this.edgeLabel,
      o.edgeLabel) && java.util.Objects.equals(
      this.vertexId,
      o.vertexId) && java.util.Objects.equals(
      this.edgeId,
      o.edgeId) && java.util.Objects.equals(
      this.propertyKey,
      o.propertyKey) && java.util.Objects.equals(
      this.propertyValue,
      o.propertyValue) && java.util.Objects.equals(
      this.outVertex,
      o.outVertex) && java.util.Objects.equals(
      this.outVertexLabel,
      o.outVertexLabel) && java.util.Objects.equals(
      this.inVertex,
      o.inVertex) && java.util.Objects.equals(
      this.inVertexLabel,
      o.inVertexLabel) && java.util.Objects.equals(
      this.outEdge,
      o.outEdge) && java.util.Objects.equals(
      this.outEdgeLabel,
      o.outEdgeLabel) && java.util.Objects.equals(
      this.inEdge,
      o.inEdge) && java.util.Objects.equals(
      this.inEdgeLabel,
      o.inEdgeLabel) && java.util.Objects.equals(
      this.ignore,
      o.ignore);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(vertexLabel) + 3 * java.util.Objects.hashCode(edgeLabel) + 5 * java.util.Objects.hashCode(vertexId) + 7 * java.util.Objects.hashCode(edgeId) + 11 * java.util.Objects.hashCode(propertyKey) + 13 * java.util.Objects.hashCode(propertyValue) + 17 * java.util.Objects.hashCode(outVertex) + 19 * java.util.Objects.hashCode(outVertexLabel) + 23 * java.util.Objects.hashCode(inVertex) + 29 * java.util.Objects.hashCode(inVertexLabel) + 31 * java.util.Objects.hashCode(outEdge) + 37 * java.util.Objects.hashCode(outEdgeLabel) + 41 * java.util.Objects.hashCode(inEdge) + 43 * java.util.Objects.hashCode(inEdgeLabel) + 47 * java.util.Objects.hashCode(ignore);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AnnotationSchema other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      vertexLabel,
      other.vertexLabel);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      edgeLabel,
      other.edgeLabel);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      vertexId,
      other.vertexId);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      edgeId,
      other.edgeId);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      propertyKey,
      other.propertyKey);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      propertyValue,
      other.propertyValue);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      outVertex,
      other.outVertex);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      outVertexLabel,
      other.outVertexLabel);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      inVertex,
      other.inVertex);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      inVertexLabel,
      other.inVertexLabel);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      outEdge,
      other.outEdge);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      outEdgeLabel,
      other.outEdgeLabel);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      inEdge,
      other.inEdge);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      inEdgeLabel,
      other.inEdgeLabel);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      ignore,
      other.ignore);
  }

  public AnnotationSchema withVertexLabel(String vertexLabel) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withEdgeLabel(String edgeLabel) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withVertexId(String vertexId) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withEdgeId(String edgeId) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withPropertyKey(String propertyKey) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withPropertyValue(String propertyValue) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withOutVertex(String outVertex) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withOutVertexLabel(String outVertexLabel) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withInVertex(String inVertex) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withInVertexLabel(String inVertexLabel) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withOutEdge(String outEdge) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withOutEdgeLabel(String outEdgeLabel) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withInEdge(String inEdge) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withInEdgeLabel(String inEdgeLabel) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }

  public AnnotationSchema withIgnore(String ignore) {
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
}
