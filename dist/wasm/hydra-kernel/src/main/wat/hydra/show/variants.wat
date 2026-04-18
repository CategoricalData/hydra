(module
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\09\00\00\00\61\6e\6e\6f\74\61\74\65\64\0b\00\00\00\61\70\70\6c\69\63\61\74\69\6f\6e\05\00\00\00\63\61\73\65\73\06\00\00\00\65\69\74\68\65\72\06\00\00\00\66\6f\72\61\6c\6c\08\00\00\00\66\75\6e\63\74\69\6f\6e\06\00\00\00\69\6e\6a\65\63\74\06\00\00\00\6c\61\6d\62\64\61\03\00\00\00\6c\65\74\04\00\00\00\6c\69\73\74\07\00\00\00\6c\69\74\65\72\61\6c\03\00\00\00\6d\61\70\05\00\00\00\6d\61\79\62\65\04\00\00\00\70\61\69\72\07\00\00\00\70\72\6f\6a\65\63\74\06\00\00\00\72\65\63\6f\72\64\03\00\00\00\73\65\74\0f\00\00\00\74\79\70\65\41\70\70\6c\69\63\61\74\69\6f\6e\0a\00\00\00\74\79\70\65\4c\61\6d\62\64\61\05\00\00\00\75\6e\69\6f\6e\04\00\00\00\75\6e\69\74\06\00\00\00\75\6e\77\72\61\70\08\00\00\00\76\61\72\69\61\62\6c\65\04\00\00\00\76\6f\69\64\04\00\00\00\77\72\61\70")
  (global $__bump_ptr (mut i32) i32.const 1280 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.show.variants.term_variant" (func $hydra.show.variants.term_variant) )
  (export "hydra.show.variants.type_variant" (func $hydra.show.variants.type_variant) )
  (func $hydra.show.variants.term_variant (param $arg_0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_term_variant (result i32)
  (block $wrap
  (block $variable
  (block $unwrap
  (block $unit
  (block $inject
  (block $type_application
  (block $type_lambda
  (block $set
  (block $record
  (block $project
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $let
  (block $lambda
  (block $either
  (block $cases
  (block $application
  (block $annotated
  local.get $arg_0
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $cases $either $lambda $let $list $literal $map $maybe $pair $project $record $set $type_lambda $type_application $inject $unit $unwrap $variable $wrap $wrap
)
  local.get $v
  drop
  i32.const 1024
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1037
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1052
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1061
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1103
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1113
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1120
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1128
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1139
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1146
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1155
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1163
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1174
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1184
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1210
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1191
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1093
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1233
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1241
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1251
  br $end_term_variant
)
  local.get $v
  drop
  i32.const 1271
  br $end_term_variant
)
)
  (func $hydra.show.variants.type_variant (param $arg_0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_type_variant (result i32)
  (block $wrap
  (block $void
  (block $variable
  (block $unit
  (block $union
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $function
  (block $forall
  (block $either
  (block $application
  (block $annotated
  local.get $arg_0
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $annotated $application $either $forall $function $list $literal $map $maybe $pair $record $set $union $unit $variable $void $wrap $wrap
)
  local.get $v
  drop
  i32.const 1024
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1037
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1061
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1071
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1081
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1120
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1128
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1139
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1146
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1155
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1174
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1184
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1224
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1233
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1251
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1263
  br $end_type_variant
)
  local.get $v
  drop
  i32.const 1271
  br $end_type_variant
)
)
)
