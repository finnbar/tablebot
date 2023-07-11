-- |
-- Module      : Tablebot.Plugins.Roll.Dice
-- Description : Lex, parse, and evaluate dice and other expressions using this plugin.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the neccessary parsers and stucture to get the AST for an
-- expression that contains dice, as well as evaluate that expression.
--
-- The behind the scenes for the dice is split into six files, two of which
-- are for generating dice statistics.
-- - DiceData - the data structures for the AST for dice
-- - DiceFunctions - functionality for dealing with functions and processing
--    them
-- - DiceParsing - parsers for getting all the DiceData items
-- - DiceEval - methods for evaluating elements from DiceData
-- - DiceStats - filling the type classes and function needed to generate
--    statistics on dice
-- - DiceStatsBase - functions to process dice value distributions
--
-- Below is the regex representing the parsing for the expressions, and
-- explanations for each component. It's not 100% accurate to the actual data
-- representation, but it's close enough that you can start reading `DiceData`,
-- which is the canonical representation of the AST, and then DiceParsing.
--
-- If there is a gap between terms, any number of spaces (including none) is
-- valid, barring in lstv, dice, die, dopr, ords, funcBasics, misc; spaces are
-- added manually in those.
--
-- TODO: it's usually safer to put these kinds of grammars in Backus-Naur form
--  rather than regex due to the sheer number of regex standards and possible
--  overloading of ?.
--
-- prog - stat* (lstv | expr)
-- stat - (lstv | expr) ";"
-- misc - ifst | vars
-- ifst - "if" spc1 expr spc1 "then" spc1 (lstv | expr) spc1 "else" spc1 (lstv | expr)
-- vars - "var" spc1 "!"? ("l_" name spcs "=" spcs lstv | name spcs "=" spcs expr)
-- lstv - nbse "#" base | funcBasics | lstb | name | misc
-- lstb - "{" expr ("," expr)* "}" | "(" lstv ")"
-- expr - term ([+-] term)* | misc
-- term - nega ([*/] nega)*
-- nega - "-" expo | expo
-- expo - func ("^" func)*
-- func - funcBasics | base
-- base - dice | nbse | name
-- nbse - "(" expr ")" | [0-9]+
-- dice - base die dopr?
-- die  - "d" "!"? (base | lstb)
-- dopr - dopo+
-- dopo - "!"? (("rr" | "ro") ords | ("k"|"d") (("l" | "h") nbse | "w" ords))
-- ords - ("/=" | "<=" | ">=" | "<" | "=" | ">") nbase
-- spcs - " "*
-- spc1 - " "+
-- argv - lstv | expr
-- funcBasics - {some string identifier} "(" spcs (argv (spcs "," spcs argv)*)? spcs ")"
-- name - [a-z_]*
--
-- prog (Program)                      - representing a complete program - a series of statements and a value to output at the end.
-- stat (Statement)                    - representing a single statement - an expression or list value
-- misc (MiscData)                     - either an if or a var
-- ifst (If)                           - representing one of two values depending on the outcome of an expression
-- vars (Var)                          - setting a variable to a certain value
-- lstv (ListValues)                   - representing all possible list values (basic list values, functions that return lists, and values which are lists of length N that consist of `Base`s, as well as a MiscData value)
-- lstb (ListValuesBase)               - representing some basic list values (those that can be used in dice expressions, such as manually created lists and bracketed `ListValues`)
-- expr (Expr)                         - representing addition, subtraction, or a single `Term` value, or a MiscData value
-- term (Term)                         - representing multiplication, division, or a single `Negation` value
-- nega (Negation)                     - representing a negation, or a single `Expo` value
-- expo (Expo)                         - representing exponentiation or a single `Func` value
-- func (Func)                         - representing a function that returns an integer, or a single `Base` value
-- base (Base)                         - representing a base value, which is either a dice value or a `NumBase` value
-- nbse (NumBase)                      - representing an integer or an expression in parentheses
-- dice (Dice)                         - representing an amount of dice of a given size (a `Base`) and type (dependent on what the `Die` is), with certain modifiers optionally given (in the `DieOpRecur`)
-- die  (Die)                          - representing a die with either a `NumBase` or a `ListValuesBase` to choose a value from. The die could also be a lazy die, where the option is evaluated repeatedly
-- dopr (DieOpRecur)                   - representing one or more `DieOpOption`s
-- dopo (DieOpOption)                  - representing one of rerolling on a condition, keeping/dropping certain values (the highest/lowest `NumBase` values or where values meet a condition), or performing another operation lazily (meaning that the values should be evaluated every time they need)
-- ords (AdvancedOrdering and NumBase) - representing a more complex ordering operation than a basic `Ordering`, when compared to a `NumBase`
-- argv (ArgValue)                     - representing an argument to a function
-- funcBasics                          - a generic regex representation for a general function parser
module Tablebot.Plugins.Roll.Dice (evalProgram, evalInteger, evalList, ListValues (..), defaultRoll, ParseShow (parseShow), integerFunctionsList, listFunctionsList, maximumListLength, maximumRNG, Converter (promote)) where

import Tablebot.Plugins.Roll.Dice.DiceData
  ( Converter (promote),
    Die (Die),
    Expr,
    ListValues (..),
    NumBase (Value),
  )
import Tablebot.Plugins.Roll.Dice.DiceEval (ParseShow (parseShow), evalInteger, evalList, evalProgram, maximumListLength, maximumRNG)
import Tablebot.Plugins.Roll.Dice.DiceFunctions (integerFunctionsList, listFunctionsList)
import Tablebot.Plugins.Roll.Dice.DiceParsing ()

-- | The default expression to evaluate if no expression is given.
defaultRoll :: Expr
defaultRoll = promote (Die (Value 20))
