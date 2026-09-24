package util

// Unit is Hydra's unit type: a single value equal to itself. The empty struct
// carries no data and all instances are identical.
type Unit struct{}

// TheUnit is the sole value of the unit type.
var TheUnit = Unit{}
