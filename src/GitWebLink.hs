{-# LANGUAGE OverloadedStrings #-}
-- git-web-link provide links to web UIs for git projects
-- Copyright (C) 2017-2019 Edd Steel
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

module GitWebLink(run) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Map(Map)
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import GitWebLink.FileOps(relDirOrFile)
import GitWebLink.GitOps
import GitWebLink.GitWebProvider
import GitWebLink.Types
import Network.URI
import Text.Read(readMaybe)
import qualified Data.Map as M
import qualified Data.Text as T

type MIO a = MaybeT IO a

fromMaybeA :: Maybe a -> MIO a
fromMaybeA = MaybeT . pure

run :: InputParameters -> IO (Maybe URI)
run options = runMaybeT $ do
  let optionOr  = optionWithDefault options
  root      <- lift gitRootDir
  deref     <- optionOr pDeref  $ pure True
  branch    <- optionOr pBranch $ lift gitActiveBranch
  remote    <- optionOr pRemote $ resolveRemote branch
  reference <- preferredReference options <|> pure branch
  provider  <- resolveProvider remote reference
  params    <- case (pCommit options, pBranch options, pFilePath options, pRegion options) of
    (Just c, _, Nothing, _)     -> return $ CommitP c
    (_, _, (Just fp), (Just r)) -> RegionP reference r <$> resolveFile root fp
    (_, _, (Just fp), _)        -> PathP reference <$> resolvePath root fp
    (_, (Just _), _, _)         -> return $ RefP reference
    _                           -> return HomeP
  return $ mkLink params provider

optionWithDefault :: InputParameters -> (InputParameters -> Maybe a) -> MIO a -> MIO a
optionWithDefault ip f def = (MaybeT . pure . f) ip <|> def

namedReference :: InputParameters -> Maybe GitReference
namedReference o = pCommit o <|> pTag o <|> pBranch o

derefReference :: InputParameters -> Maybe GitReference -> MaybeT IO GitReference
derefReference Params{pDeref=Just True} (Just ref) = lift . gitDereference $ ref
derefReference _ (Just ref) = pure ref
derefReference _ _ = MaybeT $ pure Nothing

preferredReference :: InputParameters -> MaybeT IO GitReference
preferredReference options = derefReference options $ namedReference options

resolveRemote :: GitReference -> MIO Text
resolveRemote branch = branchRemote <|> firstRemote <|> pure "origin"
  where
    branchRemote = MaybeT (gitRemoteForBranch branch)
    firstRemote = lift (head <$> gitRemoteNames)

resolveProvider :: Text -> GitReference -> MIO GitWebProvider
resolveProvider remote branch = do
  remotes <- lift gitRemotesByKey
  remote <- fromMaybeA $ M.lookup remote remotes
  fromMaybeA $ recogniseProvider remote

resolvePath :: FilePath -> Text -> MIO DirOrFile
resolvePath root filepath = relDirOrFile root (T.unpack filepath)

resolveFile :: FilePath -> Text -> MIO FilePath
resolveFile root filepath = relDirOrFile root (T.unpack filepath) >>= justFiles
  where justFiles (Dir _)  = fromMaybeA Nothing
        justFiles (File f) = fromMaybeA (Just f)
