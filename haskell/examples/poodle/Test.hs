{-# LANGUAGE TypeOperators #-}

import Data.Label

data A = A { _b :: Int, _c :: Char } deriving Show

b :: A :-> Int
b = lens _b (\x a -> a { _b = x })
