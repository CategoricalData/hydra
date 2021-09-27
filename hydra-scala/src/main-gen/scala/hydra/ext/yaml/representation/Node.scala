/**
 * @comments Every YAML node has an optional scalar tag or non-specific tag (omitted from this model)
 */
package hydra.ext.yaml.representation

/**
 * @comments Every YAML node has an optional scalar tag or non-specific tag (omitted from this model)
 */
enum Node:
    /**
     * @comments Failsafe schema: tag:yaml.org,2002:map
     * @type map:
     *         keys: hydra/ext/yaml/representation.Node
     *         values: hydra/ext/yaml/representation.Node
     */
    case mapping(value: Map[Node, Node]) extends Node
    /**
     * @type hydra/ext/yaml/representation.Scalar
     */
    case scalar(value: Scalar) extends Node
    /**
     * @comments Failsafe schema: tag:yaml.org,2002:seq
     * @type list: hydra/ext/yaml/representation.Node
     */
    case sequence(value: Seq[Node]) extends Node

val _Node: String = "hydra/ext/yaml/representation.Node"
val _Node_mapping: String = "mapping"
val _Node_scalar: String = "scalar"
val _Node_sequence: String = "sequence"
