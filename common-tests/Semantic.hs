{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies, TypeOperators,
        UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

module Semantic where

import Type
import Data.Proxy
import GHC.TypeLits
import qualified Reactive.Sodium.Denotational as D
import Reactive.Sodium.Denotational (S, C)

data List a = Nil | Cons a (List a)

type family Index (e :: List *) (i :: Nat) :: * where
    Index (Cons a f) 0 = a
    Index (Cons a f) i = Index f (i-1)

type family StreamType s :: * where
    StreamType (Stream e a) = a

data Semantic (e :: List *) where
    LetStream :: KnownType a => Stream e a -> Semantic (Cons (Stream e a) e) -> Semantic e
    AssertEquals :: (KnownNat ref, KnownType (StreamType (Index e ref))) =>
                    Proxy (ref :: Nat) -> S (StreamType (Index e ref)) -> Semantic e -> Semantic e
    End :: Semantic e

data Ref (e :: List *) a where
    Ref :: Proxy (ref :: Nat) -> Ref e (Index e ref)
    Lit :: a -> Ref e a

data Stream (e :: List *) a where
    StreamRef :: KnownNat ref => Proxy (ref :: Nat) -> Stream e (StreamType (Index e ref))
    MkStream :: KnownType a => S a -> Stream e a
    Split    :: KnownType a => Stream e [a] -> Stream e a
    Defer    :: KnownType a => Stream e a -> Stream e a
    OrElse   :: KnownType a => Stream e a -> Stream e a -> Stream e a

instance KnownType a => KnownType (Stream e a) where
    typeOf _ = StreamT (typeOf (undefined :: a))
    literalOf _ = Nothing

prev1 :: Proxy 0
prev1 = Proxy

prev2 :: Proxy 1 
prev2 = Proxy

prev3 :: Proxy 2 
prev3 = Proxy

data SemanticTest = SemanticTest String String (Semantic Nil)

