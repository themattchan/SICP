{-# LANGUAGE OverloadedStrings #-}
-- A domain specific language for register machines
module SICP.Chapter5.RegisterMachine where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

--------------------------------------------------------------------------------
-- * Monad for "assembly" programs
-- based partly on
-- https://github.com/ucsd-cse131/
-- https://github.com/sdiehl/tinyjit/blob/master/src/Assembler.hs

{- Section 5.1.5: Instructions
A controller instruction in our register-machine language has one of the
following forms, where each <inputi> is either (reg <register-name>) or (const
<constant-value>).

These instructions were introduced in section 5.1.1:

(assign <register-name> (reg <register-name>))

(assign <register-name> (const <constant-value>))

(assign <register-name> (op <operation-name>) <input1> ... <inputn>)

(perform (op <operation-name>) <input1> ... <inputn>)

(test (op <operation-name>) <input1> ... <inputn>)

(branch (label <label-name>))

(goto (label <label-name>))

The use of registers to hold labels was introduced in section 5.1.3:

(assign <register-name> (label <label-name>))

(goto (reg <register-name>))

Instructions to use the stack were introduced in section 5.1.4:

(save <register-name>)

(restore <register-name>)

The only kind of <constant-value> we have seen so far is a number, but later we
will use strings, symbols, and lists. For example, (const "abc") is the string
"abc", (const abc) is the symbol abc, (const (a b c)) is the list (a b c), and
(const ()) is the empty list.
-}

data Reg = A | B | C | D
  deriving (Show, Bounded, Enum, Eq)

newtype Label = Lbl String
  deriving (Show, Eq)

newtype Operation = Operation String
  deriving (Show, Eq)

data Val = Reg Reg | Label Label
  deriving (Show, Eq)

data Val = Op Operation [| Const Constant

data Constant = Num Int
  deriving (Show, Eq)

data Instr
  = Assign Reg Val
  | Goto Val
  | Branch Label
  | Save Reg
  | Restore Reg
