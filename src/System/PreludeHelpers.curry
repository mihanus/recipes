------------------------------------------------------------------------------
-- A few helper operations which should be part of the Prelude.
------------------------------------------------------------------------------

module System.PreludeHelpers
 where

------------------------------------------------------------------------------
-- Read/Show instances for larger tuples.

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) =>
         Show (a, b, c, d, e, f, g) where
  showsPrec _ (a, b, c, d, e, f, g) =
    showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g]

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) =>
         Show (a, b, c, d, e, f, g, h) where
  showsPrec _ (a, b, c, d, e, f, g, h) =
    showTuple [ shows a, shows b, shows c, shows d, shows e, shows f
              , shows g, shows h]


instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g) =>
    Read (a, b, c, d, e, f, g) where
  readsPrec _ = readParen False
                  (\o -> [ ((a, b, c, d, e, f, g), z) | ("(", p) <- lex o
                                                   , (a, q) <- reads p
                                                   , (",", r) <- lex q
                                                   , (b, s) <- reads r
                                                   , (",", t) <- lex s
                                                   , (c, u) <- reads t
                                                   , (",", v) <- lex u
                                                   , (d, w) <- reads v
                                                   , (",", x) <- lex w
                                                   , (e, y) <- reads x
                                                   , (",", z1) <- lex y
                                                   , (f, z2) <- reads z1
                                                   , (",", z3) <- lex z2
                                                   , (g, z4) <- reads z3
                                                   , (")", z) <- lex z4 ])

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h) =>
    Read (a, b, c, d, e, f, g, h) where
  readsPrec _ = readParen False
                  (\o -> [ ((a, b, c, d, e, f, g, h), z) | ("(", p) <- lex o
                                                   , (a, q) <- reads p
                                                   , (",", r) <- lex q
                                                   , (b, s) <- reads r
                                                   , (",", t) <- lex s
                                                   , (c, u) <- reads t
                                                   , (",", v) <- lex u
                                                   , (d, w) <- reads v
                                                   , (",", x) <- lex w
                                                   , (e, y) <- reads x
                                                   , (",", z1) <- lex y
                                                   , (f, z2) <- reads z1
                                                   , (",", z3) <- lex z2
                                                   , (g, z4) <- reads z3
                                                   , (",", z5) <- lex z4
                                                   , (h, z6) <- reads z5
                                                   , (")", z) <- lex z6 ])

------------------------------------------------------------------------------
