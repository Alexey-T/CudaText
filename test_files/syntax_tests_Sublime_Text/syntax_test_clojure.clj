;; SYNTAX TEST "Packages/Clojure/Clojure.sublime-syntax"

  (+ 3 3)
;;<- - meta.sexpr.clojure
;;^^^^^^^ meta.sexpr.clojure
;;       ^ - meta.sexpr.clojure
;; ^ keyword.operator.clojure
;;   ^ constant.numeric.float.clojure
;;     ^ constant.numeric.float.clojure

  (/ 10 3)
;;<- - meta.sexpr.clojure
;;^^^^^^^^ meta.sexpr.clojure
;;        ^ - meta.sexpr.clojure
;; ^ keyword.operator.clojure
;;   ^^ constant.numeric.float.clojure
;;      ^ constant.numeric.float.clojure

  (/ 10 3.0)
;;<- - meta.sexpr.clojure
;;^^^^^^^^^^ meta.sexpr.clojure
;;          ^ - meta.sexpr.clojure
;; ^ keyword.operator.clojure
;;   ^^ constant.numeric.float.clojure
;;      ^^^ constant.numeric.float.clojure

  (+ 1 2 3 4 5 6)
;;<- - meta.sexpr.clojure - meta.function.clojure
;;^^^^^^^^^^^^^^^ meta.sexpr.clojure
;;               ^ - meta.sexpr.clojure
;; ^ keyword.operator.clojure
;;   ^ constant.numeric.float.clojure

  (defn square [x] (* x x))
;;<- - meta.sexpr.clojure - meta.function.clojure
;;^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.clojure
;; ^^^^ storage.type.function.type.clojure 
;;      ^^^^^ entity.name.function.clojure source.symbol.clojure
;;             ^^^ meta.function.body.clojure meta.function.parameters.vector.clojure
;;             ^ punctuation.definition.vector.begin.clojure
;;              ^ meta.function.parameters.vector.clojure variable.parameter.clojure source.symbol.clojure
;;               ^ punctuation.definition.vector.end.clojure
;;                 ^^^^^^^ meta.function.body.clojure meta.function.body.code.clojure meta.sexpr.clojure
;;                  ^ keyword.operator.clojure
;;                   ^ - keyword.operator.clojure - source.symbol.clojure
;;                    ^ source.symbol.clojure
;;                     ^ - source.symbol.clojure
;;                      ^ source.symbol.clojure
;;                        ^ - meta.sexpr.clojure

  (square 10) ;; test
;;^^^^^^^^^^^ meta.sexpr.clojure
;;           ^^^^^^^^ - meta.sexpr.clojure
;;            ^^ punctuation.definition.comment.clojure
;;            ^^^^^^^ comment.line.semicolon.double.clojure
;;^^^^^^^^^^^ - keyword.operator.clojure
;; ^^^^^^ source.symbol.clojure
;;       ^^^^^^^^^^^^ - source.symbol.clojure
;;        ^^ constant.numeric.float.clojure

  (def testfunc (fn [] "Hello world"))
;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.sexpr.clojure meta.function.def_form.clojure
;; ^^^ meta.sexpr.clojure meta.function.def_form.clojure storage.type.variable.clojure
;;     ^^^^^^^^ entity.name.function.clojure source.symbol.clojure
;;              ^^^^^^^^^^^^^^^^^^^^^ meta.function.lambda.clojure
;;               ^^ storage.type.function.type.clojure
;;                  ^^^^^^^^^^^^^^^^ meta.function.body.clojure
;;                  ^^ meta.function.parameters.vector.clojure punctuation.definition.vector
;;                     ^^^^^^^^^^^^^ meta.function.body.code.clojure string.quoted.double.clojure
;;                                  ^ - meta.function.body.code.clojure string.quoted.double.clojure
;;                     ^ punctuation.definition.string.begin.clojure
;;                                 ^ punctuation.definition.string.end.clojure

  (def hello1 (fn [] #"Hello world"))
;;                   ^^^^^^^^^^^^^^ string.regexp.clojure
;;                   ^^ punctuation.definition.string.begin.clojure
;;                                ^ punctuation.definition.string.end.clojure
;;                                 ^ - string.regexp.clojure
;;                     ^^^^^^^^^^^ meta.literal.regexp

  (def hello2 (fn [] #"Hello world\\"))
;;                   ^^^^^^^^^^^^^^^^ string.regexp.clojure
;;                   ^^ punctuation.definition.string.begin.clojure
;;                                  ^ punctuation.definition.string.end.clojure
;;                                   ^ - string.regexp.clojure
;;                     ^^^^^^^^^^^ meta.literal.regexp
;;                                ^^ constant.character.escape.regexp

  (def hello (fn [] #"{2}Hello*?* world\"{2} foobar*"))
;;                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string.regexp.clojure
;;                  ^^ punctuation.definition.string.begin.clojure
;;                                     ^^ constant.character.escape.regexp - punctuation.definition.string.end.clojure
;;                                                  ^ punctuation.definition.string.end.clojure
;;                                                   ^ - string.regexp.clojure
;;                       ^^^^^ meta.literal.regexp
;;                                          ^^^^^^^ meta.literal.regexp
;;                    ^^^ invalid.illegal.unexpected-quantifier.regexp - meta.literal.regexp
;;                            ^^ keyword.operator.quantifier.regexp
;;                              ^ invalid.illegal.unexpected-quantifier.regexp
;;                                       ^^ keyword.operator.quantifier.regexp
;;                                                 ^ keyword.operator.quantifier.regexp

  (def hello (fn [] #"(?=unclosed_paren"))
;;                  ^^^^^^^^^^^^^^^^^^^^ string.regexp.clojure
;;                  ^^ punctuation.definition.string.begin.clojure
;;                                     ^ punctuation.definition.string.end.clojure
;;                                      ^ - string.regexp.clojure
