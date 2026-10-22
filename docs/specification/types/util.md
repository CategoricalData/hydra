<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.util

<!-- Note: this is an automatically generated file. Do not edit. -->

## hydra.core.util.CaseConvention

*(undocumented)*

`∀.union{camel:unit, pascal:unit, lowerSnake:unit, upperSnake:unit, upperDashed:unit}@{description="A naming convention for symbols, such as camelCase or snake_case"}`

## hydra.core.util.Comparison

*(undocumented)*

`∀.union{lessThan:unit, equalTo:unit, greaterThan:unit}@{description="An equality judgement: less than, equal to, or greater than"}`

## hydra.core.util.ModuleNames

*(undocumented)*

`∀.(forall n. record{focus:(hydra.core.packaging.ModuleName, n)@{description="The module name in focus, together with its associated value"}, mapping:map<hydra.core.packaging.ModuleName, n>@{description="A mapping of module names to values"}})@{description="A mapping from module names to values of type n, with a focus on one module name"}`

## hydra.core.util.Precision

*(undocumented)*

`∀.union{arbitrary:unit@{description="Arbitrary precision"}, bits:int32@{description="Precision to a specified number of bits"}}@{description="Numeric precision: arbitrary precision, or precision to a specified number of bits"}`

## hydra.core.util.QualifiedName

*(undocumented)*

`∀.record{moduleName:optional<hydra.core.packaging.ModuleName>@{description="The optional module name"}, local:string@{description="The local name"}}@{description="A qualified name consisting of an optional module name together with a mandatory local name"}`
