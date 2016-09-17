-- SYNTAX TEST "Packages/Haskell/Haskell.sublime-syntax"

23*36  -- single line comment
--     ^^ punctuation.definition.comment.haskell
--     ^^^^^^^^^^^^^^^^^^^^^^^ comment.line.double-dash.haskell
23*36
-- <- - comment.line.double-dash.haskell

   {- block comment -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^^^^^^^^^ comment.block.haskell
--                  ^^ punctuation.definition.comment.end.haskell
--                    ^ - comment.block.haskell

   {- {-# #-} -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^^^ comment.block.haskell - meta.preprocessor.haskell
--            ^^ punctuation.definition.comment.end.haskell
--              ^ - comment.block.haskell

   {- {- #-} -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^^ comment.block.haskell
--           ^^ punctuation.definition.comment.end.haskell
--             ^ - comment.block.haskell

   {- {- -} -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^ comment.block.haskell
--          ^^ punctuation.definition.comment.end.haskell
--            ^ - comment.block.haskell

   {- {-# -} -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^^ comment.block.haskell - meta.preprocessor.haskell
--           ^^ punctuation.definition.comment.end.haskell
--             ^ - comment.block.haskell

   {- {-# {- test -} -} -} 23*36
-- ^^ punctuation.definition.comment.begin.haskell
-- ^^^^^^^^^^^^^^^^^^^^^^^ comment.block.haskell - meta.preprocessor.haskell
--                      ^^ punctuation.definition.comment.end.haskell
--                        ^ - comment.block.haskell

   class (Functor t, Foldable t) => Traversable t where
-- ^^^^^ keyword.other.haskell
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.declaration.class.haskell
   {-# MINIMAL traverse | sequenceA LANGUAGE #-}
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.preprocessor.haskell
--                                              ^ - meta.preprocessor.haskell
--                                   ^^^^^^^ keyword.other.preprocessor.haskell

-- | Map each element of a structure to an action,
-- evaluate these actions from left to right, and
-- collect the results. For a version that ignores
-- the results see 'Data.Foldable.traverse_'.
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ comment.line.double-dash.haskell

   traverse :: Applicative f =>
-- ^^^^^^^^ entity.name.function.haskell
--          ^^ keyword.other.double-colon.haskell
--             ^^^^^^^^^^^ storage.type.haskell
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.function.type-declaration.haskell
--                           ^^ keyword.other.big-arrow.haskell
    (a -> f b)
-- ^^^^^^^^^^^^ meta.function.type-declaration.haskell
--     ^^ keyword.other.arrow.haskell
    -> t a
-- ^^^^^^^^ meta.function.type-declaration.haskell
--  ^^ keyword.other.arrow.haskell
    -> f (t b)
-- ^^^^^^^^^^^^ meta.function.type-declaration.haskell
--  ^^ keyword.other.arrow.haskell
   traverse f = sequenceA . fmap f
-- ^^^^^^^^^^^^^ meta.function.type-declaration.haskell
--            ^ keyword.operator.haskell
--                        ^ keyword.operator.haskell

-- | Evaluate each action in the structure from
-- left to right, and collect the results.
-- For a version that ignores the results see
-- 'Data.Foldable.sequenceA_'.
   sequenceA ∷ Applicative f ⇒ t (f a) → f (t a)
-- ^^^^^^^^^ entity.name.function.haskell
--           ^ keyword.other.double-colon.haskell
--             ^^^^^^^^^^^ storage.type.haskell
--                           ^ keyword.other.big-arrow.haskell
--                                     ^ keyword.other.arrow.haskell
   sequenceA = traverse id
--           ^ keyword.operator.haskell
