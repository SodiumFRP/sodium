{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeOperators, TypeFamilies,
        FlexibleContexts, FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}

module FRP.Sodium.C.Sodium where

import Control.Applicative
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Int
import Data.IntMap
import qualified Data.IntMap as IM
import Data.Maybe
import Data.Word
import Language.C
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Parser

data Value = Value {
        vaDecls :: [CDecl],
        vaStats :: [CStat],
        vaExpr  :: CExpr
    }

class RType a where
    r :: Value -> a
    unr :: a -> Value

class RType (R a) => RNum a where
    data R a :: *
    constant :: a -> R a
    plus :: R a -> R a -> R a
    ra `plus` rb = r $ Value {
            vaDecls = vaDecls a ++ vaDecls b,
            vaStats = vaStats a ++ vaStats b,
            vaExpr  = CBinary CAddOp (vaExpr a) (vaExpr b) undefNode
        }
      where a = unr ra
            b = unr rb

castedNumeric :: Integral i => String -> i -> CExpr
castedNumeric typ i = CCast
                        (CDecl [CTypeSpec (CTypeDef (Ident typ 0 undefNode) undefNode)] [] undefNode)
                        (CConst (CIntConst (CInteger (fromIntegral i) DecRepr noFlags) undefNode))
                        undefNode

instance RNum Int64 where
    newtype R Int64 = RInt64 Value
    constant i = RInt64 $ Value {
            vaDecls = [],
            vaStats = [],
            vaExpr  = castedNumeric "int64_t" i
        }
instance RType (R Int64) where
    r = RInt64
    unr (RInt64 v) = v

instance RNum Int32 where
    newtype R Int32 = RInt32 Value
    constant i = RInt32 $ Value {
            vaDecls = [],
            vaStats = [],
            vaExpr  = castedNumeric "int32_t" i
        }
instance RType (R Int32) where
    r = RInt32
    unr (RInt32 v) = v

instance RNum Int16 where
    newtype R Int16 = RInt16 Value
    constant i = RInt16 $ Value {
            vaDecls = [],
            vaStats = [],
            vaExpr  = castedNumeric "int16_t" i
        }
instance RType (R Int16) where
    unr (RInt16 v) = v

instance RNum Int8 where
    newtype R Int8 = RInt8 Value
    constant i = RInt8 $ Value {
            vaDecls = [],
            vaStats = [],
            vaExpr  = castedNumeric "int8_t" i
        }
instance RType (R Int8) where
    unr (RInt8 v) = v

instance RNum Word64 where
    newtype R Word64 = RWord64 Value
    constant i = RWord64 $ Value {
            vaDecls = [],
            vaStats = [],
            vaExpr  = castedNumeric "uint64_t" i
        }
instance RType (R Word64) where
    unr (RWord64 v) = v

instance RNum Word32 where
    newtype R Word32 = RWord32 Value
    constant i = RWord32 $ Value {
            vaDecls = [],
            vaStats = [],
            vaExpr  = castedNumeric "uint32_t" i
        }
instance RType (R Word32) where
    unr (RWord32 v) = v

instance RNum Word16 where
    newtype R Word16 = RWord16 Value
    constant i = RWord16 $ Value {
            vaDecls = [],
            vaStats = [],
            vaExpr  = castedNumeric "uint16_t" i
        }
instance RType (R Word16) where
    unr (RWord16 v) = v

instance RNum Word8 where
    newtype R Word8 = RWord8 Value
    constant i = RWord8 $ Value {
            vaDecls = [],
            vaStats = [],
            vaExpr  = castedNumeric "uint8_t" i
        }
instance RType (R Word8) where
    unr (RWord8 v) = v

data Behavior a = Behavior Int
data Event a = Event Int

data EventImpl where
    To :: Int -> EventImpl
    MapE :: (Value -> Value) -> Int -> EventImpl
    Code :: String -> CStat -> EventImpl

data ReactiveState = ReactiveState {
        rsInputs    :: [Int],
        rsNextIdent :: String,
        rsNextEvent :: Int,
        rsEvents    :: IntMap [EventImpl]
    }

newReactiveState :: ReactiveState
newReactiveState = ReactiveState {
        rsInputs    = [],
        rsNextIdent = "a",
        rsNextEvent = 2,
        rsEvents    = IM.empty
    }

newtype Reactive a = Reactive { unReactive :: State ReactiveState a }
    deriving (Functor, Applicative, Monad)

connect :: Int -> EventImpl -> Reactive ()
connect i impl = Reactive $ modify $ \rs -> rs {
        rsEvents = IM.alter (Just . (impl:) . fromMaybe []) i (rsEvents rs)
    }

incIdent :: String -> String
incIdent = reverse . ii . reverse
  where
    ii [] = ['a']
    ii ('z':xs) = 'a' : ii xs
    ii (x:xs)   = succ x : xs

never :: Event a
never = Event 0

allocEvent :: Reactive Int
allocEvent = Reactive $ do
    e <- gets rsNextEvent
    modify $ \rs -> rs { rsNextEvent = rsNextEvent rs + 1 }
    return e

merge :: RType a => Event a -> Event a -> Reactive (Event a)
merge (Event ea) (Event eb) = do
    ec <- allocEvent
    connect ea (To ec)
    connect eb (To ec)
    return $ Event ec

mapE :: forall a b . (RType a, RType b) => (a -> b) -> Event a -> Reactive (Event b)
mapE f (Event ea) = do
    eb <- allocEvent
    connect ea (MapE (\v -> unr (f (r v))) eb)
    return $ Event eb

listen :: Event a
       -> String     -- ^ Variable identifier
       -> ByteString -- ^ Statement or statement block to handle it
       -> Reactive ()
listen (Event ea) ident statement = do
    case execParser statementP statement (position 0 "" 1 1) [] [] of
        Left err -> fail $ "parse failed in listen: " ++ show err
        Right (stmt, _) -> do
            connect ea (Code ident stmt)

react :: (Event a -> Reactive ()) -> ReactiveState
react r = execState (unReactive (r (Event 1))) newReactiveState

main :: IO ()
main = do
    let st = react $ \ea -> do
            eb <- mapE (`plus` constant (1 :: Int32)) ea
            listen eb "x" "{ printf(\"x=%d\\n\", x); }"
            return ()
    return ()

