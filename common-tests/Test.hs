module Test where

import Data.String

newtype Var = Var String

instance IsString Var where
    fromString = Var

data Val = V Var |
           String String |
           List [Val]

instance IsString Val where
    fromString = String

data Handle = AppendList Var Val

data Reactive =
    NewStream Type Var |
    Send Var Val |
    Listen Var Var (Type, Var) [Handle]

data Type = StreamT Type | StreamSinkT Type | StringT | ListenerT

data Statement =
    Transactional [Reactive] [(Type, Var, Var)] |
    NewList Type Var |
    AssertEqual Val Var |
    Unlisten Var

data Test = Test String String [Statement]

