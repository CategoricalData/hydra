/**
 * @comments Every YAML node has an optional scalar tag or non-specific tag (omitted from this model)
 */
package hydra.ext.yaml.model

/**
 * @comments Every YAML node has an optional scalar tag or non-specific tag (omitted from this model)
 */
enum Node:
    /**
     * @comments Failsafe schema: tag:yaml.org,2002:map
     * @type map:
     *         keys: hydra/ext/yaml/model.Node
     *         values: hydra/ext/yaml/model.Node
     */
    case mapping(value: Map[hydra.ext.yaml.model.Node, hydra.ext.yaml.model.Node]) extends Node
    /**
     * @type hydra/ext/yaml/model.Scalar
     */
    case scalar(value: hydra.ext.yaml.model.Scalar) extends Node
    /**
     * @comments Failsafe schema: tag:yaml.org,2002:seq
     * @type list: hydra/ext/yaml/model.Node
     */
    case sequence(value: Seq[hydra.ext.yaml.model.Node]) extends Node

val _Node: String = "hydra/ext/yaml/model.Node"
val _Node_mapping: String = "mapping"
val _Node_scalar: String = "scalar"
val _Node_sequence: String = "sequence"
