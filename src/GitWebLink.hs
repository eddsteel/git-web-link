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
  let optionOr = optionWithDefault options
  root         <- lift gitRootDir
  deref        <- optionOr pDeref (pure True)
  commit       <- pure pCommit
  branch       <- optionOr pBranch (lift gitActiveBranch)
  actualBranch <- resolveActualBranch deref branch
  remote       <- optionOr pRemote (resolveRemote actualBranch)
  provider     <- resolveProvider remote actualBranch
  params       <- case (pCommit options, pBranch options, pFilePath options, pRegion options) of
                    (Just c, _, _, _)           -> return $ CommitP c
                    (_, _, (Just fp), (Just r)) -> RegionP actualBranch r <$> resolveFile root fp
                    (_, _, (Just fp), _)        -> PathP actualBranch <$> resolvePath root fp
                    (_, (Just _), _, _)         -> return $ BranchP actualBranch -- don't forget to deref
                    _                           -> return HomeP
  return $ mkLink params provider

optionWithDefault :: InputParameters -> (InputParameters -> Maybe a) -> MIO a -> MIO a
optionWithDefault ip f def = fromMaybeA (f ip) <|> def

resolveActualBranch :: Bool -> GitBranch -> MIO GitBranch
resolveActualBranch derefBranch branch = maybeDeref branch
  where maybeDeref = if derefBranch
                     then lift . gitBranchReference
                     else pure

resolveRemote :: GitBranch -> MIO Text
resolveRemote branch = branchRemote <|> firstRemote <|> pure "origin"
  where
    branchRemote = MaybeT (gitRemoteForBranch branch)
    firstRemote = lift (head <$> gitRemoteNames)

resolveProvider :: Text -> GitBranch -> MIO GitWebProvider
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
