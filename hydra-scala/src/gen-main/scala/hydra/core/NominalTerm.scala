/**
 * A term annotated with a fixed, named type; an instance of a newtype
 */
package hydra.core

/**
 * A term annotated with a fixed, named type; an instance of a newtype
 */
case class NominalTerm[a] (
    /**
     * @type hydra/core.Name
     */
    typeName: hydra.core.Name,
    
    /**
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    term: hydra.core.Term[a]
)

val _NominalTerm: String = "hydra/core.NominalTerm"
val _NominalTerm_term: String = "term"
val _NominalTerm_typeName: String = "typeName"
