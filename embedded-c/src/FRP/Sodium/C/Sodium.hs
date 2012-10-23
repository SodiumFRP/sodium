{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeOperators, TypeFamilies,
        FlexibleContexts, FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}

module FRP.Sodium.C.Sodium where

import Control.Applicative
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Int
import Data.IntMap (IntMap)
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
    ctype :: a -> CTypeSpec
    r :: Value -> a
    unr :: a -> Value

konstant :: forall a . (RNum a, Integral a) => a -> R a
konstant i = r $ Value {
        vaDecls = [],
        vaStats = [],
        vaExpr  = castedNumeric (ctype (undefined :: R a)) i
    }

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

castedNumeric :: Integral i => CTypeSpec -> i -> CExpr
castedNumeric typ i = CCast
                        (CDecl [CTypeSpec typ] [] undefNode)
                        (CConst (CIntConst (CInteger (fromIntegral i) DecRepr noFlags) undefNode))
                        undefNode

instance RNum Int64 where
    newtype R Int64 = RInt64 Value
    constant = konstant
instance RType (R Int64) where
    ctype _ = CTypeDef (Ident "int64_t" 0 undefNode) undefNode
    r = RInt64
    unr (RInt64 v) = v

instance RNum Int32 where
    newtype R Int32 = RInt32 Value
    constant = konstant
instance RType (R Int32) where
    ctype _ = CTypeDef (Ident "int32_t" 0 undefNode) undefNode
    r = RInt32
    unr (RInt32 v) = v

instance RNum Int16 where
    newtype R Int16 = RInt16 Value
    constant = konstant
instance RType (R Int16) where
    ctype _ = CTypeDef (Ident "int16_t" 0 undefNode) undefNode
    r = RInt16
    unr (RInt16 v) = v

instance RNum Int8 where
    newtype R Int8 = RInt8 Value
    constant = konstant
instance RType (R Int8) where
    ctype _ = CTypeDef (Ident "int8_t" 0 undefNode) undefNode
    r = RInt8
    unr (RInt8 v) = v

instance RNum Word64 where
    newtype R Word64 = RWord64 Value
    constant = konstant
instance RType (R Word64) where
    ctype _ = CTypeDef (Ident "uint64_t" 0 undefNode) undefNode
    r = RWord64
    unr (RWord64 v) = v

instance RNum Word32 where
    newtype R Word32 = RWord32 Value
    constant = konstant
instance RType (R Word32) where
    ctype _ = CTypeDef (Ident "uint32_t" 0 undefNode) undefNode
    r = RWord32
    unr (RWord32 v) = v

instance RNum Word16 where
    newtype R Word16 = RWord16 Value
    constant = konstant
instance RType (R Word16) where
    ctype _ = CTypeDef (Ident "uint16_t" 0 undefNode) undefNode
    r = RWord16
    unr (RWord16 v) = v

instance RNum Word8 where
    newtype R Word8 = RWord8 Value
    constant = konstant
instance RType (R Word8) where
    ctype _ = CTypeDef (Ident "uint8_t" 0 undefNode) undefNode
    r = RWord8
    unr (RWord8 v) = v

data Behavior a = Behavior Int
data Event a = Event Int

data EventImpl where
    To :: Int -> EventImpl
    MapE :: (Value -> Value) -> Int -> EventImpl
    Code :: String -> CStat -> EventImpl

data ReactiveState = ReactiveState {
        rsInputType :: CTypeSpec,
        rsInputs    :: [Int],
        rsNextIdent :: String,
        rsNextEvent :: Int,
        rsEvents    :: IntMap [EventImpl]
    }

newReactiveState :: CTypeSpec -> ReactiveState
newReactiveState typ = ReactiveState {
        rsInputType = typ,
        rsInputs    = [],
        rsNextIdent = "a",
        rsNextEvent = 2,
        rsEvents    = IM.empty
    }

toC :: ReactiveState -> [CExternalDeclaration NodeInfo]
toC rs = [
        CFDefExt $ CFunDef [
                CTypeSpec (CVoidType undefNode)
            ]
            (
                CDeclr (Just $ Ident "banana" 0 undefNode) [
                        CFunDeclr (Right ([
                            CDecl [
                                CTypeSpec $ rsInputType rs
                            ] [(Just (
                                CDeclr (Just $ Ident "apple" 0 undefNode) [] Nothing [] undefNode
                            ), Nothing, Nothing)] undefNode
                        ], False)) [] undefNode
                    ] Nothing [] undefNode
            )
            []
            (
                CCompound [] stmts undefNode
--                    CBlockStmt (CBreak undefNode)
            )
            undefNode
    ]
  where
    stmts = flip concatMap (IM.toList (rsEvents rs)) $ \(ix, impls) ->
        flip concatMap impls $ \impl ->
            case impl of
                _ -> []

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
    case execParser statementP statement (position 0 "blah" 1 1) [Ident "hello" 0 undefNode] (map Name [0..]) of
        Left err -> fail $ "parse failed in listen: " ++ show err
        Right (stmt, _) -> do
            connect ea (Code ident stmt)

react :: forall a . RType a => (Event a -> Reactive ()) -> ReactiveState
react r = execState (unReactive (r (Event 1))) (newReactiveState (ctype (undefined :: a)))

main :: IO ()
main = do
    let st = react $ \ea -> do
            eb <- mapE (`plus` constant (1 :: Int32)) ea
            listen eb "x" "{ printf(\"x=%d\\n\", x); }"
            return ()
    putStrLn $ show $ pretty $ CTranslUnit (toC st) undefNode

