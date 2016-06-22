module External where

import Recursion as R

-- External Table (Alan's Solution)
-----------------------------------
type UniqueIdentifier = Int -- or (SrcSpan, ...) as in Alan's work

type Exp' col id  = R.Exp' UniqueIdentifier col id
type Exp  col id  = R.Exp  UniqueIdentifier col id
type Tbl  row     = [(UniqueIdentifier, row)]
-- Table

-- it can be
--    `[(UniqueIdentifier, Maybe ann)]`
-- or even
--    `[ann]`
-- (where the UniqueIdentifier is based on the annotation index in the list)
