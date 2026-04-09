// Note: this is an automatically generated file. Do not edit.

package parsing

type ParseError struct {
  Message string
  Remainder string
}

type ParseResult [A any] interface {
  isParseResult()
}

type ParseResultSuccess [A any] struct {
  Value ParseSuccess[A]
}

func (ParseResultSuccess[A]) isParseResult() {

}

type ParseResultFailure [A any] struct {
  Value ParseError
}

func (ParseResultFailure[A]) isParseResult() {

}

type ParseSuccess [A any] struct {
  Value A
  Remainder string
}

type Parser [A any] func(string) ParseResult[A]
