{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, TypeOperators, TypeFamilies,
        FlexibleContexts, FlexibleInstances, ScopedTypeVariables, OverloadedStrings,
        UndecidableInstances #-}

module FRP.Sodium.C.Sodium where

import Control.Applicative
import Control.Monad.State.Strict
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word
import Language.C
import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Parser

data Value = Value {
        vaDecls :: [CDecl],
        vaStmts :: [CStat],
        vaExpr  :: CExpr
    }

class RType a where
    ctype :: a -> CTypeSpec
    r :: Value -> a
    unr :: a -> Value

class RType (R a) => RConstant a where
    formatConstant :: a -> CExpr

instance (RType (R a), Integral a) => RConstant a where
    formatConstant a = CCast
            (CDecl [CTypeSpec typ] [] undefNode)
            (CConst (CIntConst (CInteger (fromIntegral a) DecRepr noFlags) undefNode))
            undefNode
        where typ = ctype (undefined :: R a)

constant :: RConstant a => a -> Value
constant i = Value {
        vaDecls = [],
        vaStmts = [],
        vaExpr  = formatConstant i
    }

variable :: Ident -> Value
variable ident = Value [] [] (CVar ident undefNode)

formatValue :: (CExpr -> CBlockItem) -> Value -> [CBlockItem]
formatValue processOutput va =
    map CBlockDecl (vaDecls va) ++
    map CBlockStmt (vaStmts va) ++
    [
        processOutput (vaExpr va)
    ]

class RConstant a => RNum a where
    data R a :: *
    plus :: R a -> R a -> R a
    ra `plus` rb = r $ Value {
            vaDecls = vaDecls a ++ vaDecls b,
            vaStmts = vaStmts a ++ vaStmts b,
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
instance RType (R Int64) where
    ctype _ = CTypeDef (Ident "int64_t" 0 undefNode) undefNode
    r = RInt64
    unr (RInt64 v) = v

instance RNum Int32 where
    newtype R Int32 = RInt32 Value
instance RType (R Int32) where
    ctype _ = CTypeDef (Ident "int32_t" 0 undefNode) undefNode
    r = RInt32
    unr (RInt32 v) = v

instance RNum Int16 where
    newtype R Int16 = RInt16 Value
instance RType (R Int16) where
    ctype _ = CTypeDef (Ident "int16_t" 0 undefNode) undefNode
    r = RInt16
    unr (RInt16 v) = v

instance RNum Int8 where
    newtype R Int8 = RInt8 Value
instance RType (R Int8) where
    ctype _ = CTypeDef (Ident "int8_t" 0 undefNode) undefNode
    r = RInt8
    unr (RInt8 v) = v

instance RNum Word64 where
    newtype R Word64 = RWord64 Value
instance RType (R Word64) where
    ctype _ = CTypeDef (Ident "uint64_t" 0 undefNode) undefNode
    r = RWord64
    unr (RWord64 v) = v

instance RNum Word32 where
    newtype R Word32 = RWord32 Value
instance RType (R Word32) where
    ctype _ = CTypeDef (Ident "uint32_t" 0 undefNode) undefNode
    r = RWord32
    unr (RWord32 v) = v

instance RNum Word16 where
    newtype R Word16 = RWord16 Value
instance RType (R Word16) where
    ctype _ = CTypeDef (Ident "uint16_t" 0 undefNode) undefNode
    r = RWord16
    unr (RWord16 v) = v

instance RNum Word8 where
    newtype R Word8 = RWord8 Value
instance RType (R Word8) where
    ctype _ = CTypeDef (Ident "uint8_t" 0 undefNode) undefNode
    r = RWord8
    unr (RWord8 v) = v

data Event a = Event Int

data EventImpl where
    To :: CTypeSpec -> Int -> EventImpl
    MapE :: CTypeSpec -> (Value -> Value) -> Int -> EventImpl
    Code :: CTypeSpec -> String -> CStat -> EventImpl
    SnapshotWith :: CTypeSpec -> (Value -> Value -> Value) -> CExpr -> Int -> EventImpl
    Store :: CTypeSpec -> Ident -> EventImpl

typeOf :: EventImpl -> CTypeSpec
typeOf (To t _) = t
typeOf (MapE t _ _) = t
typeOf (Code t _ _) = t
typeOf (SnapshotWith t _ _ _) = t
typeOf (Store t _) = t

data Header = AngleHeader ByteString | QuoteHeader ByteString deriving (Eq, Ord)

formatHeader :: Header -> ByteString
formatHeader (AngleHeader h) = "#include <" `mappend` h `mappend` ">\n"
formatHeader (QuoteHeader h) = "#include \"" `mappend` h `mappend` "\"\n"

data ReactiveState = ReactiveState {
        rsHeaders   :: Set Header,
        rsInputType :: CTypeSpec,
        rsInputs    :: [Int],
        rsNextIdent :: String,
        rsNextEvent :: Int,
        rsEvents    :: IntMap [EventImpl],
        rsGlobalVars :: [(CTypeSpec, Ident, CExpr)]
    }

newReactiveState :: CTypeSpec -> ReactiveState
newReactiveState typ = ReactiveState {
        rsHeaders   = S.fromList [AngleHeader "stdint.h"],
        rsInputType = typ,
        rsInputs    = [],
        rsNextIdent = "__a",
        rsNextEvent = 2,
        rsEvents    = IM.empty,
        rsGlobalVars = []
    }

toC :: ReactiveState -> [CExtDecl]
toC rs = prototypes ++ globals ++ funcs
  where
    implsType :: [EventImpl] -> CTypeSpec
    implsType = fromMaybe (CVoidType undefNode) .
                fmap typeOf .
                listToMaybe
    inputTypeOf :: Int -> CTypeSpec
    inputTypeOf ix = implsType $ fromMaybe [] $ ix `IM.lookup` rsEvents rs
    mkVarDecl :: CTypeSpec -> Ident -> CDecl
    mkVarDecl ty ident = 
        let tyDecl = CTypeSpec ty
            declr = CDeclr (Just ident) [] Nothing [] undefNode
        in  CDecl [tyDecl] [(Just declr, Nothing, Nothing)] undefNode
    mkLabel ix = Ident ("react"++show ix) 0 undefNode
    
    allocIdent :: State String Ident
    allocIdent = do
        ident <- get
        modify succIdent
        return $ Ident ident 0 undefNode

    prototypes :: [CExtDecl]
    prototypes = flip map (IM.toList (rsEvents rs)) $ \(ix, impls0) ->
        let impls = reverse impls0
            itype = implsType impls
            ivar = Ident "a" 0 undefNode
            declr = CDeclr (Just $ mkLabel ix) [
                    CFunDeclr (Right ([
                        mkVarDecl itype ivar
                    ], False)) [] undefNode
                ] Nothing [] undefNode
        in 
            CDeclExt $ CDecl [
                    CTypeSpec (CVoidType undefNode)
                ] [(Just declr, Nothing, Nothing)] undefNode

    globals :: [CExtDecl]
    globals = flip map (rsGlobalVars rs) $ \(ty, ident, expr) ->
        CDeclExt $
            let declr = CDeclr (Just ident) [] Nothing [] undefNode
                ass = CInitExpr expr undefNode 
            in  CDecl [CTypeSpec ty] [(Just declr, Just ass, Nothing)] undefNode 

    funcs :: [CExtDecl]
    funcs = flip evalState (rsNextIdent rs) $ do
        forM (IM.toList (rsEvents rs)) $ \(ix, impls0) -> do
            let impls = reverse impls0
            ivar <- allocIdent
            let itype = implsType impls
                call to expr = CBlockStmt $ CExpr (Just $ CCall (CVar (mkLabel to) undefNode) [expr] undefNode) undefNode
                blocks = flip map impls $ \impl ->
                    let items =
                            case impl of
                                To ty to -> [call to (CVar ivar undefNode)]
                                MapE ty f to -> formatValue (call to) (f (variable ivar))
                                Code ty var stmt -> [
                                        CBlockDecl (
                                            let declr = CDeclr (Just (Ident var 0 undefNode)) [] Nothing [] undefNode
                                                ass = CInitExpr (CVar ivar undefNode) undefNode 
                                            in  CDecl [CTypeSpec ty] [(Just declr, Just ass, Nothing)] undefNode 
                                        ),
                                        CBlockStmt (fmap (const undefNode) stmt)
                                    ]
                                SnapshotWith ty f bvar to -> formatValue (call to) (f (variable ivar) (Value [] [] bvar))
                                Store ty ident -> [
                                        let expr = CAssign CAssignOp (CVar ident undefNode) (CVar ivar undefNode) undefNode
                                        in  CBlockStmt $ CExpr (Just expr) undefNode
                                    ]
                    in  case items of
                            [stat@(CBlockStmt _)] -> stat
                            _                     -> CBlockStmt $ CCompound [] items undefNode
            return $
                CFDefExt $ CFunDef [
                        CTypeSpec (CVoidType undefNode)
                    ]
                    (
                        CDeclr (Just $ mkLabel ix) [
                                CFunDeclr (Right ([
                                    mkVarDecl itype ivar
                                ], False)) [] undefNode
                            ] Nothing [] undefNode
                    )
                    []
                    (
                        case blocks of
                            [CBlockStmt one@(CCompound _ _ _)] -> one
                            _     -> CCompound [] blocks undefNode
                    )
                    undefNode

newtype Reactive a = Reactive { unReactive :: State ReactiveState a }
    deriving (Functor, Applicative, Monad)

connect :: Int -> EventImpl -> Reactive ()
connect i impl = Reactive $ modify $ \rs -> rs {
        rsEvents = IM.alter (Just . (impl:) . fromMaybe []) i (rsEvents rs)
    }

succIdent :: String -> String
succIdent = reverse . ii . reverse
  where
    ii [] = ['a']
    ii underscores@('_':_) = underscores
    ii ('z':xs) = 'a' : ii xs
    ii (x:xs)   = succ x : xs

never :: Event a
never = Event 0

allocEvent :: Reactive Int
allocEvent = Reactive $ do
    e <- gets rsNextEvent
    modify $ \rs -> rs { rsNextEvent = rsNextEvent rs + 1 }
    return e

allocIdent :: Reactive Ident
allocIdent = Reactive $ do
    i <- gets rsNextIdent
    modify $ \rs -> rs { rsNextIdent = succIdent (rsNextIdent rs) }
    return $ Ident i 0 undefNode

merge :: forall a . RType a => Event a -> Event a -> Reactive (Event a)
merge (Event ea) (Event eb) = do
    ec <- allocEvent
    let ty = ctype (undefined :: a)
    connect ea (To ty ec)
    connect eb (To ty ec)
    return $ Event ec

mapE :: forall a b . (RType a, RType b) => (a -> b) -> Event a -> Reactive (Event b)
mapE f (Event ea) = do
    eb <- allocEvent
    let ty = ctype (undefined :: a)
    connect ea (MapE ty (\v -> unr (f (r v))) eb)
    return $ Event eb

data Behavior a = Behavior {
       behEvent :: Event a,
       behVar   :: CExpr
    }

hold :: forall a . RConstant a => a -> Event (R a) -> Reactive (Behavior (R a))
hold init ea@(Event ea_i) = do
    i <- allocIdent
    let ty = ctype (undefined :: R a)
        var = CVar i undefNode
    Reactive $ modify $ \rs -> rs {
            rsGlobalVars = (ty, i, formatConstant init):rsGlobalVars rs
        }
    connect ea_i $ Store ty i
    return $ Behavior ea var

snapshotWith :: forall a b c . (RType a, RType b, RType c) => (a -> b -> c) -> Event a -> Behavior b -> Reactive (Event c)
snapshotWith f (Event ea) bb = do
    ec <- allocEvent
    let ty = ctype (undefined :: c)
    connect ea $ SnapshotWith ty
        (\va vb -> unr (f (r va) (r vb)))
        (behVar bb)
        ec
    return $ Event ec

addHeader :: Header -> Reactive ()
addHeader h = Reactive $ modify $ \rs -> rs { rsHeaders = S.insert h (rsHeaders rs) }

listen :: forall a . RType a => Event a
       -> [Header]   -- ^ Header
       -> String     -- ^ Variable identifier
       -> ByteString -- ^ Statement or statement block to handle it
       -> Reactive ()
listen (Event ea) headers ident statement = do
    mapM_ addHeader headers
    case execParser statementP statement (position 0 "blah" 1 1) [] (map Name [0..]) of
        Left err -> fail $ "parse failed in listen: " ++ show err
        Right (stmt, _) -> do
            let ty = ctype (undefined :: a)
            connect ea (Code ty ident stmt)

react :: forall a . RType a => (Event a -> Reactive ()) -> ByteString
react r =
    let rs = execState (unReactive (r (Event 1))) (newReactiveState (ctype (undefined :: a)))
    in  mconcat (map formatHeader (S.toList $ rsHeaders rs)) `mappend`
        C.pack (show $ pretty $ CTranslUnit (toC rs) undefNode)

main :: IO ()
main =
    {-
    C.putStrLn $ react $ \ea -> do
        eb <- mapE (`plus` constant (1 :: Int32)) ea
        ec <- mapE (`plus` constant (100 :: Int32)) ea
        ed <- merge eb ec
        listen ec [AngleHeader "stdio.h"] "x" "printf(\"ec=%d\\n\", x);"
        listen ed [AngleHeader "stdio.h"] "x" "printf(\"ed=%d\\n\", x);"
    -}
    C.putStrLn $ react $ \ea -> do
        ba <- hold (100 :: Int32) ea
        eb <- snapshotWith (\e b -> e `plus` b) ea ba
        listen eb [AngleHeader "stdio.h"] "x" "printf(\"%d\\n\", (int)x);"

