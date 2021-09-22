package hydra.core

enum TypeVariant:
    case atomic() extends TypeVariant
    case element() extends TypeVariant
    case function() extends TypeVariant
    case list() extends TypeVariant
    case nominal() extends TypeVariant
    case record() extends TypeVariant
    case union() extends TypeVariant
