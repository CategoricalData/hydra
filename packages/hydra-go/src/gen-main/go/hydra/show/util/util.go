// Note: this is an automatically generated file. Do not edit.

package showutil

import "hydra.dev/hydra/util"

func CaseConvention (c util.CaseConvention) string {
  return func (x any) any {
    switch v := x.(type) {
      case util.CaseConventionLowerSnake:
      return func (_ struct{}) any {
        return "lower_snake_case"
      }(v)
      case util.CaseConventionUpperSnake:
      return func (_ struct{}) any {
        return "UPPER_SNAKE_CASE"
      }(v)
      case util.CaseConventionCamel:
      return func (_ struct{}) any {
        return "camelCase"
      }(v)
      case util.CaseConventionPascal:
      return func (_ struct{}) any {
        return "PascalCase"
      }(v)
    }
    return nil
  }(c).(string)
}
