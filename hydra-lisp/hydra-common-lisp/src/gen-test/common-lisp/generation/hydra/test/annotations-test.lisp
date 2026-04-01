;; Note: this is an automatically generated file. Do not edit.
;; annotations

;; arbitrary annotations

(defun test-annotations-negarbitrary-annotations-negset-single-annotation--num1 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="foo"}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="foo"}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}}}})))

(defun test-annotations-negarbitrary-annotations-negset-single-annotation--num2 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"myKey"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=-17:int32}}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"myKey"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=-17:int32}}}}}})))

(defun test-annotations-negarbitrary-annotations-negset-single-annotation--num3 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=0:int32}}}, annotation={wrap(hydra.core.Name){"x"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="hello"}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=0:int32}}}, annotation={wrap(hydra.core.Name){"x"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="hello"}}}}})))

(defun test-annotations-negarbitrary-annotations-negget-existing-annotation--num1 ()

  (assert (equal just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="value"}}) just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="value"}}))))

(defun test-annotations-negarbitrary-annotations-negget-existing-annotation--num2 ()

  (assert (equal just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){string=""}}) just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){string=""}}))))

(defun test-annotations-negarbitrary-annotations-negget-existing-annotation--num3 ()

  (assert (equal just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=123:int32}}}) just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=123:int32}}}))))

(defun test-annotations-negarbitrary-annotations-negget-missing-annotation--num1 ()

  (assert (equal nothing nothing)))

(defun test-annotations-negarbitrary-annotations-negget-missing-annotation--num2 ()

  (assert (equal nothing nothing)))

(defun test-annotations-negarbitrary-annotations-negget-missing-annotation--num3 ()

  (assert (equal nothing nothing)))

(defun test-annotations-negarbitrary-annotations-negset-multiple-annotations--num1 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=true}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="first"}}, wrap(hydra.core.Name){"k2"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=200:int32}}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=true}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="first"}}, wrap(hydra.core.Name){"k2"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=200:int32}}}}}})))

(defun test-annotations-negarbitrary-annotations-negset-multiple-annotations--num2 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}}, annotation={wrap(hydra.core.Name){"a"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=-5:int32}}}, wrap(hydra.core.Name){"b"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=0:int32}}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}}, annotation={wrap(hydra.core.Name){"a"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=-5:int32}}}, wrap(hydra.core.Name){"b"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=0:int32}}}}}})))

(defun test-annotations-negarbitrary-annotations-negouter-annotation-overrides-inner--num1 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="outer"}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"k1"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="outer"}}}}})))

(defun test-annotations-negarbitrary-annotations-negouter-annotation-overrides-inner--num2 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}, annotation={wrap(hydra.core.Name){"x"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="new"}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}, annotation={wrap(hydra.core.Name){"x"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="new"}}}}})))

(defun test-annotations-negarbitrary-annotations-negouter-annotation-overrides-inner--num3 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=false}}, annotation={wrap(hydra.core.Name){"key"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=999:int32}}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=false}}, annotation={wrap(hydra.core.Name){"key"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=999:int32}}}}}})))

(defun test-annotations-negarbitrary-annotations-negunset-single-annotation--num1 ()

  (assert (equal inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}} inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}})))

(defun test-annotations-negarbitrary-annotations-negunset-single-annotation--num2 ()

  (assert (equal inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}} inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}})))

(defun test-annotations-negarbitrary-annotations-negunset-one-of-multiple-annotations--num1 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}}, annotation={wrap(hydra.core.Name){"k2"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=200:int32}}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}}, annotation={wrap(hydra.core.Name){"k2"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=200:int32}}}}}})))

(defun test-annotations-negarbitrary-annotations-negunset-one-of-multiple-annotations--num2 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="x"}}, annotation={wrap(hydra.core.Name){"a"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="x"}}, annotation={wrap(hydra.core.Name){"a"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}}}}})))

;; descriptions

(defun test-annotations-negdescriptions-negset-description--num1 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="foo"}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="my description"}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="foo"}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="my description"}}}}})))

(defun test-annotations-negdescriptions-negset-description--num2 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string=""}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=42:int32}}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string=""}}}}})))

(defun test-annotations-negdescriptions-negset-description--num3 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=true}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="A longer description with spaces"}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){boolean=true}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="A longer description with spaces"}}}}})))

(defun test-annotations-negdescriptions-negget-existing-description--num1 ()

  (assert (equal right(just("hello")) right(just("hello")))))

(defun test-annotations-negdescriptions-negget-existing-description--num2 ()

  (assert (equal right(just("")) right(just("")))))

(defun test-annotations-negdescriptions-negget-existing-description--num3 ()

  (assert (equal right(just("desc")) right(just("desc")))))

(defun test-annotations-negdescriptions-negget-missing-description--num1 ()

  (assert (equal right(nothing) right(nothing))))

(defun test-annotations-negdescriptions-negget-missing-description--num2 ()

  (assert (equal right(nothing) right(nothing))))

(defun test-annotations-negdescriptions-negget-missing-description--num3 ()

  (assert (equal right(nothing) right(nothing))))

(defun test-annotations-negdescriptions-negouter-description-overrides-inner--num1 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="outer"}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="bar"}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="outer"}}}}})))

(defun test-annotations-negdescriptions-negouter-description-overrides-inner--num2 ()

  (assert (equal inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=99:int32}}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="new"}}}}} inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body=inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=99:int32}}}, annotation={wrap(hydra.core.Name){"description"}=inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="new"}}}}})))

(defun test-annotations-negdescriptions-negunset-description--num1 ()

  (assert (equal inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}} inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int64=137:int64}}})))

(defun test-annotations-negdescriptions-negunset-description--num2 ()

  (assert (equal inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}} inject(hydra.core.Term){literal=inject(hydra.core.Literal){string="test"}})))

;; layered annotations

(defun test-annotations-neglayered-annotations-negget-annotation-from-unannotated-term ()

  (assert (equal nothing nothing)))

(defun test-annotations-neglayered-annotations-negget-annotation-from-singly-annotated-term ()

  (assert (equal just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}}) just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}}))))

(defun test-annotations-neglayered-annotations-negget-inner-annotation-from-doubly-annotated-term ()

  (assert (equal just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}}) just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=1:int32}}}))))

(defun test-annotations-neglayered-annotations-negget-outer-annotation-from-doubly-annotated-term ()

  (assert (equal just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=2:int32}}}) just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=2:int32}}}))))

(defun test-annotations-neglayered-annotations-negget-non-negoverridden-annotation-from-triply-annotated-term ()

  (assert (equal just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=2:int32}}}) just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=2:int32}}}))))

(defun test-annotations-neglayered-annotations-negouter-annotation-overrides-inner-in-layered-term ()

  (assert (equal just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=99:int32}}}) just(inject(hydra.core.Term){literal=inject(hydra.core.Literal){integer=inject(hydra.core.IntegerValue){int32=99:int32}}}))))
