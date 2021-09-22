/**
 * A graph element, having a name, data term (value), and schema term (type)
 */
package hydra.graph

import hydra.core.Name
import hydra.core.Term

/**
 * A graph element, having a name, data term (value), and schema term (type)
 */
case class Element(
    /**
     * @type hydra/core.Name
     */
    name: Name,
    
    /**
     * @type hydra/core.Term
     */
    data: Term,
    
    /**
     * @type hydra/core.Term
     */
    schema: Term
)
