package hydra.eval.lib.math

import hydra.core.*

def even[T0, T1, T2](cx: T0)(g: T1)(x: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.equality.equal"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.maybes.fromMaybe"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0))))),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.maybeMod"),
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     x)), hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(2))))))))),
     hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(0))))))

def odd[T0, T1, T2](cx: T0)(g: T1)(x: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.not"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.even"),
     x)))))
