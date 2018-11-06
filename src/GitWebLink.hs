{-# LANGUAGE OverloadedStrings #-}
-- git-web-link provide links to web UIs for git projects
-- Copyright (C) 2017-2018 Edd Steel
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

module GitWebLink(runArguments) where
import GitWebLink.GitOps
import GitWebLink.GitRemote
import GitWebLink.GitWebProvider
import GitWebLink.Parameters
import GitWebLink.Types
import GitWebLink.FileOps(relDirOrFile)
import Control.Monad.Trans.Maybe
import Data.Text(Text)
import qualified Data.Text as T
import Network.URI
import Data.Map(Map)
import Text.Read(readMaybe)
import qualified Data.Map as M
import Options.Applicative(execParser)

type MIO a = MaybeT IO a

runArguments :: [Text] -> IO (Maybe URI)
runArguments args = runMaybeT $ do
  params <- resolveParams args
  let remote = undefined
  remotes <- MaybeT $ Just <$> gitRemotesByKey
  provider <- resolveProvider remote remotes
  return $ mkLink params provider
  
resolveParams :: [Text] -> MIO GWLParameters
resolveParams args = do
  options <- MaybeT $ Just <$> execParser args
  activeBranch <- MaybeT $ Just <$> activeGitBranch
  let branch = fromMaybe (pBranch option) activeBranch
  root <- MaybeT $ Just <$> gitRootDir
  case options of
    (Params _ (Just f) (Just i) (Just j)) ->
      resolveFile root f >>= \g -> RegionP b g (Range i j)
    (Params Nothing Nothing Nothing Nothing) -> HomeP

resolveProvider :: Text -> Map Text GitRemote -> MIO GitWebProvider
resolveProvider r rs = MaybeT . return $ M.lookup r rs >>= recogniseProvider

resolvePath :: FilePath -> Text -> MIO DirOrFile
resolvePath root path = relDirOrFile root (T.unpack path)

resolveFile :: FilePath -> Text -> MIO FilePath
resolveFile root path = relDirOrFile root (T.unpack path) >>= justFiles
  where justFiles (Dir _) = MaybeT $ pure Nothing
        justFiles (File f) = MaybeT $ pure (Just f) 
  
resolveBranch :: GitBranch -> MIO GitBranch
resolveBranch = MaybeT . gitBranchReference
