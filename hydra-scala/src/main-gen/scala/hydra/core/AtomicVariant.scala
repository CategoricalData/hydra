package hydra.core

enum AtomicVariant:
    case binary() extends AtomicVariant
    case boolean() extends AtomicVariant
    case float() extends AtomicVariant
    case integer() extends AtomicVariant
    case string() extends AtomicVariant
