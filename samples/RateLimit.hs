module RateLimit where

import qualified Github.RateLimit as Github

main = do
  x <- Github.rateLimit
  print x
  
