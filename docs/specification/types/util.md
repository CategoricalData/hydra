<!-- Note: this is an automatically generated file. Do not edit. -->

# hydra.core.util

## CaseConvention

A naming convention for symbols, such as camelCase or snake_case

`union{
  camel: unit,
  pascal: unit,
  lowerSnake: unit,
  upperSnake: unit,
  upperDashed: unit}`

**Variants**

- `camel` (`unit`)
- `pascal` (`unit`)
- `lowerSnake` (`unit`)
- `upperSnake` (`unit`)
- `upperDashed` (`unit`)

## Comparison

An equality judgement: less than, equal to, or greater than

`union{
  lessThan: unit,
  equalTo: unit,
  greaterThan: unit}`

**Variants**

- `lessThan` (`unit`)
- `equalTo` (`unit`)
- `greaterThan` (`unit`)

## ModuleNames

A mapping from module names to values of type n, with a focus on one module name

`forall n. record{
  focus: (hydra.core.packaging.ModuleName, n),
  mapping: map<hydra.core.packaging.ModuleName, n>}`

## Precision

Numeric precision: arbitrary precision, or precision to a specified number of bits

`union{
  arbitrary: unit,
  bits: int32}`

**Variants**

- `arbitrary` (`unit`): Arbitrary precision
- `bits` (`int32`): Precision to a specified number of bits

## QualifiedName

A qualified name consisting of an optional module name together with a mandatory local name

`record{
  moduleName: optional<hydra.core.packaging.ModuleName>,
  local: string}`

**Fields**

- `moduleName` (`optional<hydra.core.packaging.ModuleName>`): The optional module name
- `local` (`string`): The local name
