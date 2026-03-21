package hydra.variants

import hydra.core.*

enum EliminationVariant :
   case record extends EliminationVariant
   case union extends EliminationVariant
   case wrap extends EliminationVariant

enum FunctionVariant :
   case elimination extends FunctionVariant
   case lambda extends FunctionVariant
   case primitive extends FunctionVariant

enum LiteralVariant :
   case binary extends LiteralVariant
   case boolean extends LiteralVariant
   case float extends LiteralVariant
   case integer extends LiteralVariant
   case string extends LiteralVariant

enum TermVariant :
   case annotated extends TermVariant
   case application extends TermVariant
   case either extends TermVariant
   case function extends TermVariant
   case let extends TermVariant
   case list extends TermVariant
   case literal extends TermVariant
   case map extends TermVariant
   case maybe extends TermVariant
   case pair extends TermVariant
   case record extends TermVariant
   case set extends TermVariant
   case typeApplication extends TermVariant
   case typeLambda extends TermVariant
   case union extends TermVariant
   case unit extends TermVariant
   case variable extends TermVariant
   case wrap extends TermVariant

enum TypeVariant :
   case annotated extends TypeVariant
   case application extends TypeVariant
   case either extends TypeVariant
   case forall extends TypeVariant
   case function extends TypeVariant
   case list extends TypeVariant
   case literal extends TypeVariant
   case map extends TypeVariant
   case maybe extends TypeVariant
   case pair extends TypeVariant
   case record extends TypeVariant
   case set extends TypeVariant
   case union extends TypeVariant
   case unit extends TypeVariant
   case variable extends TypeVariant
   case wrap extends TypeVariant
