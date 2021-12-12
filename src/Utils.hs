module Utils
  ( column,
  )
where

column :: Int -> [[a]] -> [a]
column i = map (!! i)