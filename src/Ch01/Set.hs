module Ch01.Set
  ( powerSet,
  )
where

import Control.Monad (filterM)

powerSet :: [a] -> [[a]]
powerSet = filterM (const [True, False])
