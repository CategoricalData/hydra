package hydra.core

enum TermVariant:
    case application() extends TermVariant
    case atomic() extends TermVariant
    case cases() extends TermVariant
    case compareTo() extends TermVariant
    case data() extends TermVariant
    case element() extends TermVariant
    case function() extends TermVariant
    case lambda() extends TermVariant
    case list() extends TermVariant
    case projection() extends TermVariant
    case record() extends TermVariant
    case union() extends TermVariant
    case variable() extends TermVariant
