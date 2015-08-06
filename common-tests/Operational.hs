{-# LANGUAGE ScopedTypeVariables, DataKinds, GADTs, TupleSections #-}
module Operational where

import qualified Reactive.Sodium.Denotational as D
import Reactive.Sodium.Denotational (S, C, T)
import Control.Arrow (second)
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import qualified Semantic as S
import GHC.TypeLits
import Type


newtype Var = Var String

underscore :: Var -> Var
underscore (Var x) = Var (x ++ "_")

suffix :: Var -> String -> Var
suffix (Var x) suf = Var (x ++ "_" ++ suf)

instance IsString Var where
    fromString = Var

data Val = V Var |
           Lit Literal

data Handle = AppendList Var Val

data Expr = ExprVar Var
          | Split Expr
          | Defer Expr
          | OrElse Expr Expr

data Reactive =
    NewStream Type Var |
    Assign Type Var Expr |
    Send Var Literal |
    Listen Var Var (Type, Var) [Handle]

data Statement =
    Transactional [Reactive] [(Type, Var, Var)] |
    NewList Type Var |
    AssertEqual Literal Var |
    Unlisten Var

data Test = Test String String [Statement]

varName :: Int -> Var
varName depth = Var $ chr (ord 'a' + depth) : []

typeOfS :: S a -> a
typeOfS = undefined

fromSemantic :: S.SemanticTest -> Test
fromSemantic (S.SemanticTest group name s) =
    Test group name statements 
  where
    statements =
        logic 0 s ++
        verification 0 (inputs 0 s) (outputs 0 s)
    logic :: forall e . Int -> S.Semantic e -> [Statement]
    logic depth (S.LetStream val k) =
        let v = varName depth
            v_ = underscore v
        in  case val of
                S.MkStream s ->
                    let StreamT ty = typeOf val
                    in  Transactional [NewStream ty v_] [(StreamSinkT ty, v_, v)]
                expr ->
                    let ty = typeOf val
                    in  Transactional [Assign ty v_ (fromExpr depth expr)] [(ty, v_, v)]
          : logic (depth+1) k
    logic depth (S.AssertEquals _ _ k) = logic depth k
    logic depth S.End = []
    inputs :: forall e . Int -> S.Semantic e -> [(Int, Type, [(T, Literal)])]
    inputs depth (S.LetStream val@(S.MkStream s) k) =
        let v = varName depth
            v_ = underscore v
            StreamT ty = typeOf val
        in  (depth, ty, (map (second (fromJust . literalOf)) s)) : inputs (depth+1) k
    inputs depth (S.LetStream _ k) = inputs (depth+1) k
    inputs depth (S.AssertEquals _ _ k) = inputs depth k
    inputs depth S.End = []
    outputs :: forall e . Int -> S.Semantic e -> [(Int, Type, [(T, Literal)])]
    outputs depth (S.LetStream _ k) = outputs (depth+1) k
    outputs depth (S.AssertEquals ref s k) =
        (depth - 1 - fromIntegral (natVal ref),
         typeOf (typeOfS s),
            map (second (fromJust . literalOf)) s) :
        outputs depth k
    outputs depth S.End = []
    fromExpr :: forall e a . Int -> S.Stream e a -> Expr
    fromExpr depth (S.StreamRef ref) = ExprVar $ varName (depth - 1 - fromIntegral (natVal ref))
    fromExpr depth (S.Split expr) = Split (fromExpr depth expr)
    fromExpr depth (S.Defer expr) = Defer (fromExpr depth expr)
    fromExpr depth (S.OrElse e1 e2) = OrElse (fromExpr depth e1) (fromExpr depth e2)

dropBefore :: Int -> [(T, Literal)] -> [(T, Literal)]
dropBefore t = dropWhile (\(tt, l) -> tt < [t])

dropAllBefore :: Int -> [(Int, Type, [(T, Literal)])] -> [(Int, Type, [(T, Literal)])]
dropAllBefore t = mapMaybe $ \(var, ty, s) ->
    case dropBefore t s of
        [] -> Nothing
        s' -> Just (var, ty, s')

literalsBefore :: Int -> [(T, Literal)] -> [Literal]
literalsBefore t = map snd . takeWhile (\(tt, l) -> tt < [t])

valuesBefore :: Int -> [(Int, Type, [(T, Literal)])] -> [(Int, Type, Literal)]
valuesBefore t values =
    concatMap (\(var, ty, vals) -> map (var,ty,) $ literalsBefore t vals) values

verification :: Int -> [(Int, Type, [(T, Literal)])] -> [(Int, Type, [(T, Literal)])] -> [Statement]
verification _ [] [] = []
verification t inputs outputs =
    let t' = t+1
        theseInputs = valuesBefore t' inputs
        theseOutputs = map (\(var, ty, lits) -> (var, ty, literalsBefore t' lits)) outputs
        outputVars = nub (map (\(var, ty, _) -> (ty, var)) theseOutputs)
    in  concatMap (\(ty, var) ->
            let v = varName var
                v_o = suffix v (show t)
                v_o_l = suffix v_o "l"
                v_o_l_ = underscore v_o_l
            in
            [NewList ty v_o,
             Transactional [Listen v_o_l_ v (ty, Var "val") [AppendList v_o (V (Var "val"))]] [(ListenerT, v_o_l_, v_o_l)]
            ]
        ) outputVars
        ++ [
           Transactional (
               map (\(var, ty, lit) ->
                   Send (varName var) lit
               ) theseInputs
           ) []
        ]
        ++ concatMap (\(var, ty, lits) ->
            let v = varName var
                v_o = suffix v (show t)
                v_o_l = suffix v_o "l"
            in  [Unlisten v_o_l, AssertEqual (ListL ty lits) v_o]
        ) theseOutputs
        ++ verification t' (dropAllBefore t' inputs) (dropAllBefore t' outputs)

