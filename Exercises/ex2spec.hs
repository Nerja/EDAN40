import qualified GHC.Classes as F
import qualified Prelude as P

-- 2.4
instance (P.Ord a, P.Ord b) => P.Ord (a,b) where
  (ex2spec.compare) (a,b) (c,d)
    | a P.< b       = P.LT
    | a P.> b       = P.GT
    | b P.< c       = P.LT
    | b P.> c       = P.GT
    | P.otherwise   = P.EQ
