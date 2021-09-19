
package org.example.hydra

import org.example.hydra.Core.Name
import org.example.hydra.Core.Term

object Graph{
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
    
    /**
     * A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the
     * schema graph
     */
    case class Graph(
        /**
         * @type list: hydra/graph.Element
         */
        elements: Seq[Element],
        
        /**
         * @type function:
         *         from:
         *         - hydra/core.Term
         *         to: boolean
         */
        dataTerms: Term => Boolean,
        
        /**
         * @type hydra/graph.Graph
         */
        schemaGraph: Graph
    )
}
