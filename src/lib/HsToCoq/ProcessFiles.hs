{-# LANGUAGE FlexibleContexts, LambdaCase #-}
{-# LANGUAGE RecordWildCards     #-}
module HsToCoq.ProcessFiles (ProcessingMode(..), processFiles) where

import Control.Lens

import Data.Foldable
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Set as S

import System.Directory

import GHC
import Outputable

import HsToCoq.ConvertHaskell.Monad
import HsToCoq.Util.GHC.Deriving
import System.IO (stdout)
import HscTypes
import DynamicLoading
import Data.List (nub)
import Digraph

--------------------------------------------------------------------------------

data ProcessingMode = Recursive | NonRecursive
                    deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- This function has been heavily edited to be easy to use with Clash.
-- We use `load` on the dependencies so that the modules are in memory, then
-- we process the top-level modules with `parseModule` and `typecheckModule`.
-- Mode is by-default recursive and can't be changed.
-- TODO: implement non-recursive mode again.
processFiles :: GlobalMonad r m => ProcessingMode -> [FilePath] -> m (Maybe [TypecheckedModule])
processFiles mode files = do
  initForDeriving
  df <- getSessionDynFlags
  -- This is Clash-specific, as a practical shortcut.
  let ghcTyLitNormPlugin = mkModuleName "GHC.TypeLits.Normalise"
      ghcTyLitExtrPlugin = mkModuleName "GHC.TypeLits.Extra.Solver"
      ghcTyLitKNPlugin   = mkModuleName "GHC.TypeLits.KnownNat.Solver"
  let dflags = df { pluginModNames = nub $
                          ghcTyLitNormPlugin :
                          ghcTyLitExtrPlugin :
                          ghcTyLitKNPlugin :
                          pluginModNames df
                  }
  let dflags1 = dflags
                { optLevel  = 2
                , ghcMode   = CompManager
                , ghcLink   = LinkInMemory
                , hscTarget = HscNothing
                , reductionDepth = 1000
                }
  hscenv <- getSession
  -- The plugins are initialized explicitly.
  dflags2 <- liftIO (initializePlugins hscenv dflags1)
  _ <- setSessionDynFlags dflags2
  traverse_ (addTarget <=< (guessTarget ?? Nothing)) files
  modGraph <- depanal [] False
  let modGraph2 = flattenSCCs (topSortModuleGraph True modGraph Nothing)
  load (LoadDependenciesOf $ ms_mod_name $ head modGraph2)
  -- We're interested in the first file of the list only.
  -- TODO: Remove this limitation
  Just <$> ((:[]) <$> t (head modGraph2))
    where t m = do
                pm <- parseModule m
                tm <- typecheckModule $ trace (showSDocUnsafe (ppr $ pm_mod_summary pm)) pm
                addDerivedInstances tm
