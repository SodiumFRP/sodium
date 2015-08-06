{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, IncoherentInstances #-}
module Type where

import Data.Maybe
import Data.String

data Type = IntT | StringT
          | ListT Type
          | StreamT Type | StreamSinkT Type
          | ListenerT
    deriving (Eq, Ord, Show)

data Literal = IntL Int
             | StringL String
             | ListL Type [Literal]
    deriving Show

instance IsString Literal where
    fromString = StringL

class KnownType a where
    typeOf :: a -> Type
    literalOf :: a -> Maybe Literal

instance KnownType Int where
    typeOf _ = IntT
    literalOf a = Just $ IntL a

instance KnownType [Char] where
    typeOf _ = StringT
    literalOf a = Just $ StringL a

instance KnownType a => KnownType [a] where
    typeOf _ = ListT (typeOf (undefined :: a))
    literalOf as = Just $ ListL (typeOf (undefined :: a)) $ catMaybes $ map literalOf as

lit :: KnownType a => a -> Literal
lit a = case literalOf a of
    Just a -> a
    Nothing -> error "bad literal"
