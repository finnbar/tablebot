-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceStats
-- Description : Get statistics on particular expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin generates statistics based on the values of dice in given
-- expressions.
module Tablebot.Plugins.Roll.Dice.DiceStats (rangeExpr, rangeListValues, getStats) where

import Control.Monad
import Control.Monad.Exception
import Data.Bifunctor (Bifunctor (first))
import Data.Distribution hiding (Distribution, Experiment, fromList)
import qualified Data.Distribution as D
import Data.List
import qualified Data.Map as M
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceEval
import Tablebot.Plugins.Roll.Dice.DiceFunctions
import Tablebot.Plugins.Roll.Dice.DiceStatsBase (Distribution)
import Tablebot.Utility.Exception (catchBot)

-- | Alias for an experiment of integers.
--
-- Where a distribution is a concrete mapping between values and probabilities,
-- an Experiment is more a monadic representation of a Distribution, effectively
-- deferring calculation to the end.
--
-- I'm not sure if it's more efficient but it certainly makes composing things
-- a lot easier
type Experiment = D.Experiment Integer

-- | Convenient alias for a experiments of lists of integers.
type ExperimentList = D.Experiment [Integer]

-- | Get the most common values, the mean, and the standard deviation of a given
-- distribution.
getStats :: Distribution -> ([Integer], Double, Double)
getStats d = (modalOrder, expectation d, standardDeviation d)
  where
    vals = toList d
    modalOrder = fst <$> sortBy (\(_, r) (_, r') -> compare r' r) vals

rangeExpr :: (MonadException m) => Expr -> m Distribution
rangeExpr e = do
  ex <- range e
  return $ run ex

rangeListValues :: (MonadException m) => ListValues -> m [Distribution]
rangeListValues lv = do
  lve <- rangeList lv
  let lvd = run lve
      lvd' = toList lvd
  return $ D.fromList <$> zip' lvd'
  where
    head' [] = []
    head' (x : _) = [x]
    getHeads xs = (\(xs', p) -> (,p) <$> head' xs') =<< xs
    getTails xs = first tail <$> xs
    zip' xs = getHeads xs : zip' (getTails xs)

-- | Type class to get the overall range of a value.
--
-- A `Data.Distribution.Distribution` is a map of values to probabilities, and
-- has a variety of  functions that operate on them.
--
-- An `Data.Distribution.Experiment` is a monadic form of this.
class ParseShow a => Range a where
  -- | Try and get the `Experiment` of the given value, throwing a
  -- `MonadException` on failure.
  range :: (MonadException m, ParseShow a) => a -> m Experiment
  range a = propagateException (parseShow a) (range' a)

  range' :: (MonadException m, ParseShow a) => a -> m Experiment

instance (Range a) => Range (MiscData a) where
  range' (MiscVar l) = range l
  range' (MiscIf i) = rangeIfExpr range i

instance (RangeList a) => RangeList (MiscData a) where
  rangeList' (MiscVar l) = rangeList l
  rangeList' (MiscIf i) = rangeIfExpr rangeList i

rangeIfExpr :: (MonadException m, Ord b) => (a -> m (D.Experiment b)) -> If a -> m (D.Experiment b)
rangeIfExpr func (If b t f) = do
  b' <- range b
  let mp = toMap $ run b'
      canBeFalse = M.member 0 mp
      canBeTrue = not $ M.null $ M.filterWithKey (\k _ -> k /= 0) mp
      emptyExp = from $ D.fromList @_ @Integer []
  t' <- if canBeTrue then func t else return emptyExp
  f' <- if canBeFalse then func f else return emptyExp
  return $
    do
      b'' <- b'
      if b'' /= 0 then t' else f'

instance (Range a) => Range (Var a) where
  range' (Var _ a) = range a
  range' (VarLazy _ a) = range a

instance (RangeList a) => RangeList (Var a) where
  rangeList' (Var _ a) = rangeList a
  rangeList' (VarLazy _ a) = rangeList a

instance (ParseShow typ, Range sub) => Range (BinOp sub typ) where
  range' (BinOp a tas) = foldl' foldel (range a) tas
    where
      foldel at (typ, b) = do
        a' <- at
        b' <- range b
        return $ getOperation typ <$> a' <*> b'

instance Range Expr where
  range' (Expr e) = range e
  range' (ExprMisc t) = range t

instance Range Term where
  range' (Term (BinOp a tas)) = foldl' foldel (range a) tas
    where
      foldel at (Div, b) = do
        a' <- at
        b' <- range b
        -- If 0 is always the denominator, the distribution will be empty.
        return $ getOperation Div <$> a' <*> from (assuming (/= 0) (run b'))
      foldel at (typ, b) = do
        a' <- at
        b' <- range b
        return $ getOperation typ <$> a' <*> b'

instance Range Negation where
  range' (Neg t) = fmap negate <$> range t
  range' (NoNeg t) = range t

instance Range Expo where
  range' (NoExpo t) = range t
  range' (Expo t e) = do
    d <- range t
    d' <- range e
    -- if the exponent is always negative, the distribution will be empty
    return $ (^) <$> d <*> from (assuming (>= 0) (run d'))

instance Range Func where
  range' (NoFunc t) = range t
  range' (Func fi avs) = rangeFunction fi avs

instance Range NumBase where
  range' (Value i) = return $ return i
  range' (NBParen (Paren e)) = range e

instance Range Base where
  range' (NBase nb) = range nb
  range' (DiceBase d) = range d
  range' b@(NumVar _) = evaluationException "cannot find range of variable" [parseShow b]

instance Range Die where
  range' (LazyDie d) = range d
  range' (Die nb) = do
    nbr <- range nb
    return $
      do
        nbV <- nbr
        from $ uniform [1 .. nbV]
  range' (CustomDie lv) = do
    dievs <- rangeList lv
    return $ dievs >>= from . uniform

instance Range Dice where
  range' (Dice b d mdor) = do
    b' <- range b
    d' <- range d
    let e = do
          diecount <- b'
          getDiceExperiment diecount (run d')
    res <- rangeDiceExperiment d' mdor e
    return $ sum <$> res

-- | Get the distribution of values from a given number of (identically
-- distributed) values and the distribution of that value.
getDiceExperiment :: Integer -> Distribution -> ExperimentList
getDiceExperiment i = replicateM (fromInteger i) . from

-- | Go through each operator on dice and modify the `Experiment` representing
-- all possible collections of rolls, returning the `Experiment` produced on
-- finding `Nothing`.
rangeDiceExperiment :: (MonadException m) => Experiment -> Maybe DieOpRecur -> ExperimentList -> m ExperimentList
rangeDiceExperiment _ Nothing is = return is
rangeDiceExperiment die (Just (DieOpRecur doo mdor)) is = rangeDieOpExperiment die doo is >>= rangeDiceExperiment die mdor

-- | Perform one dice operation on the given `Experiment`, possibly returning
-- a modified experiment representing the distribution of dice rolls.
rangeDieOpExperiment :: MonadException m => Experiment -> DieOpOption -> ExperimentList -> m ExperimentList
rangeDieOpExperiment die (DieOpOptionLazy o) is = rangeDieOpExperiment die o is
rangeDieOpExperiment _ (DieOpOptionKD kd lhw) is = rangeDieOpExperimentKD kd lhw is
rangeDieOpExperiment die (Reroll rro cond lim) is = do
  limd <- range lim
  return $ do
    limit <- limd
    let newDie = mkNewDie limit
    rolls <- is
    let (count, cutdownRolls) = countTriggers limit rolls
    if count == 0
      then return cutdownRolls
      else (cutdownRolls ++) <$> getDiceExperiment count (run newDie)
  where
    mkNewDie limitValue
      | rro = die
      | otherwise = from $ assuming (\i -> not $ applyCompare cond i limitValue) (run die)
    countTriggers limitValue = foldr (\i (c, xs') -> if applyCompare cond i limitValue then (c + 1, xs') else (c, i : xs')) (0, [])

-- | Perform a keep/drop operation on the `Experiment` of dice rolls.
rangeDieOpExperimentKD :: (MonadException m) => KeepDrop -> LowHighWhere -> ExperimentList -> m ExperimentList
rangeDieOpExperimentKD kd (Where cond nb) is = do
  nbDis <- range nb
  return $ do
    wherelimit <- nbDis
    filter (\i -> keepDrop $ applyCompare cond i wherelimit) <$> is
  where
    keepDrop
      | kd == Keep = id
      | otherwise = not
rangeDieOpExperimentKD kd lhw is = do
  let nb = getValueLowHigh lhw
  case nb of
    Nothing -> whereException
    Just nb' -> do
      nbd <- range nb'
      return $ do
        kdlh <- nbd
        getKeep kdlh . sortBy' <$> is
  where
    -- the below exception should never trigger - it is a hold over. it is
    -- present so that this thing type checks nicely.
    whereException = evaluationException "keep/drop where is unsupported" []
    order l l' = if isLow lhw then compare l l' else compare l' l
    sortBy' = sortBy order
    getKeep = if kd == Keep then genericTake else genericDrop

-- | Type class to get the overall range of a list of values.
--
-- Only used within `DiceStats` as I have no interest in producing statistics on
-- lists
class ParseShow a => RangeList a where
  -- | Try and get the `DistributionList` of the given value, throwing a
  -- `MonadException` on failure.
  rangeList :: (MonadException m, ParseShow a) => a -> m ExperimentList
  rangeList a = propagateException (parseShow a) (rangeList' a)

  rangeList' :: (MonadException m, ParseShow a) => a -> m ExperimentList

instance RangeList ListValuesBase where
  rangeList' (LVBList es) = do
    exprs <- mapM range es
    return $ sequence exprs
  rangeList' (LVBParen (Paren lv)) = rangeList lv

instance RangeList ListValues where
  rangeList' (LVBase lvb) = rangeList lvb
  rangeList' (MultipleValues nb b) = do
    nbd <- range nb
    bd <- range b
    return $
      do
        valNum <- nbd
        getDiceExperiment valNum (run bd)
  rangeList' (LVFunc fi avs) = rangeFunction fi avs
  rangeList' (ListValuesMisc m) = rangeList m
  rangeList' b@(LVVar _) = evaluationException "cannot find range of variable" [parseShow b]

rangeArgValue :: MonadException m => ArgValue -> m (D.Experiment ListInteger)
rangeArgValue (AVExpr e) = (LIInteger <$>) <$> range e
rangeArgValue (AVListValues lv) = (LIList <$>) <$> rangeList lv

rangeFunction :: (MonadException m, Ord j) => FuncInfoBase j -> [ArgValue] -> m (D.Experiment j)
rangeFunction fi exprs = do
  exprs' <- mapM rangeArgValue exprs
  let params = first (funcInfoFunc fi) <$> toList (run $ sequence exprs')
  from . D.fromList <$> foldAndIgnoreErrors params
  where
    foldAndIgnoreErrors = foldr (\(mv, p) mb -> mb >>= \b -> catchBot ((: []) . (,p) <$> mv) (const (return [])) >>= \v -> return (v ++ b)) (return [])
