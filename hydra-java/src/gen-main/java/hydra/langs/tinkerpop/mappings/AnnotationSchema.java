// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.mappings;

import java.io.Serializable;

/**
 * Configurable annotation keys for property graph mapping specifications
 */
public class AnnotationSchema implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/mappings.AnnotationSchema");
  
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
    if (vertexLabel == null) {
      throw new IllegalArgumentException("null value for 'vertexLabel' argument");
    }
    if (edgeLabel == null) {
      throw new IllegalArgumentException("null value for 'edgeLabel' argument");
    }
    if (vertexId == null) {
      throw new IllegalArgumentException("null value for 'vertexId' argument");
    }
    if (edgeId == null) {
      throw new IllegalArgumentException("null value for 'edgeId' argument");
    }
    if (propertyKey == null) {
      throw new IllegalArgumentException("null value for 'propertyKey' argument");
    }
    if (propertyValue == null) {
      throw new IllegalArgumentException("null value for 'propertyValue' argument");
    }
    if (outVertex == null) {
      throw new IllegalArgumentException("null value for 'outVertex' argument");
    }
    if (outVertexLabel == null) {
      throw new IllegalArgumentException("null value for 'outVertexLabel' argument");
    }
    if (inVertex == null) {
      throw new IllegalArgumentException("null value for 'inVertex' argument");
    }
    if (inVertexLabel == null) {
      throw new IllegalArgumentException("null value for 'inVertexLabel' argument");
    }
    if (outEdge == null) {
      throw new IllegalArgumentException("null value for 'outEdge' argument");
    }
    if (outEdgeLabel == null) {
      throw new IllegalArgumentException("null value for 'outEdgeLabel' argument");
    }
    if (inEdge == null) {
      throw new IllegalArgumentException("null value for 'inEdge' argument");
    }
    if (inEdgeLabel == null) {
      throw new IllegalArgumentException("null value for 'inEdgeLabel' argument");
    }
    if (ignore == null) {
      throw new IllegalArgumentException("null value for 'ignore' argument");
    }
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
    if (vertexLabel == null) {
      throw new IllegalArgumentException("null value for 'vertexLabel' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withEdgeLabel(String edgeLabel) {
    if (edgeLabel == null) {
      throw new IllegalArgumentException("null value for 'edgeLabel' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withVertexId(String vertexId) {
    if (vertexId == null) {
      throw new IllegalArgumentException("null value for 'vertexId' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withEdgeId(String edgeId) {
    if (edgeId == null) {
      throw new IllegalArgumentException("null value for 'edgeId' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withPropertyKey(String propertyKey) {
    if (propertyKey == null) {
      throw new IllegalArgumentException("null value for 'propertyKey' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withPropertyValue(String propertyValue) {
    if (propertyValue == null) {
      throw new IllegalArgumentException("null value for 'propertyValue' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withOutVertex(String outVertex) {
    if (outVertex == null) {
      throw new IllegalArgumentException("null value for 'outVertex' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withOutVertexLabel(String outVertexLabel) {
    if (outVertexLabel == null) {
      throw new IllegalArgumentException("null value for 'outVertexLabel' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withInVertex(String inVertex) {
    if (inVertex == null) {
      throw new IllegalArgumentException("null value for 'inVertex' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withInVertexLabel(String inVertexLabel) {
    if (inVertexLabel == null) {
      throw new IllegalArgumentException("null value for 'inVertexLabel' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withOutEdge(String outEdge) {
    if (outEdge == null) {
      throw new IllegalArgumentException("null value for 'outEdge' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withOutEdgeLabel(String outEdgeLabel) {
    if (outEdgeLabel == null) {
      throw new IllegalArgumentException("null value for 'outEdgeLabel' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withInEdge(String inEdge) {
    if (inEdge == null) {
      throw new IllegalArgumentException("null value for 'inEdge' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withInEdgeLabel(String inEdgeLabel) {
    if (inEdgeLabel == null) {
      throw new IllegalArgumentException("null value for 'inEdgeLabel' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
  
  public AnnotationSchema withIgnore(String ignore) {
    if (ignore == null) {
      throw new IllegalArgumentException("null value for 'ignore' argument");
    }
    return new AnnotationSchema(vertexLabel, edgeLabel, vertexId, edgeId, propertyKey, propertyValue, outVertex, outVertexLabel, inVertex, inVertexLabel, outEdge, outEdgeLabel, inEdge, inEdgeLabel, ignore);
  }
}