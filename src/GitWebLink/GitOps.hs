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
module GitWebLink.GitOps( gitRemotes
                        , gitRemotesByKey
                        , gitRemoteNames
                        , gitBranches
                        , gitBranchNames
                        , gitBranchReference
                        , gitActiveBranch
                        , gitRemoteForBranch
                        , gitRootDir) where
import Control.Exception
import GitWebLink.Types
import GitWebLink.Parsing
import Data.List(nub)
import Data.Map(Map, fromList, lookup)
import Data.Maybe(catMaybes)
import Turtle hiding (FilePath,nub)
import qualified Data.Text as T

gitRemotes :: IO [GitRemote]
gitRemotes = runAndExtract "git" ["remote", "-v"] remoteFromLine

gitRemotesByKey :: IO (Map Text GitRemote)
gitRemotesByKey = do
  remotes <- gitRemotes
  let names = fmap name remotes
  return . fromList $ zip names remotes

gitRemoteNames :: IO [Text]
gitRemoteNames = fmap lineToText <$> runAndExtract "git" ["remote"] Just

gitBranchNames :: IO [Text]
gitBranchNames = fmap nameOfBranch <$> gitBranches

gitBranches :: IO [GitBranch]
gitBranches = runAndExtract "git" ["branch"] branchFromLine

gitRootDir :: IO FilePath
gitRootDir = fmap (T.unpack . T.strip . linesToText) $ runAndExtract "git" ["rev-parse", "--show-toplevel"] Just

gitRemoteForBranch :: GitBranch -> IO (Maybe Text)
gitRemoteForBranch b = run $ inproc "git" ["config", "--get", configKey] empty
  where configKey = T.concat ["branch.", nameOfBranch b, ".pushRemote"]
        run io = handle handler $ Just . T.strip <$> strict io
        handler :: SomeException -> IO (Maybe Text)
        handler _ = pure Nothing

-- Dereference a named branch e.g. "master" to a ref e.g. "8ce13c8bed94afba0615b515f465447073b366c8".
-- Uses `git show-ref` because
-- > Use of this utility is encouraged in favor of directly accessing files under the .git
-- > directory.
-- If the dereference fails, the original input is just returned
gitBranchReference :: GitBranch -> IO GitBranch
gitBranchReference b = head <$> runAndExtract "git" ["show-ref", "-s", "--verify", refHead] toRef
  where refHead = T.concat ["refs/heads/", nameOfBranch b]
        toRef t = if T.null (lineToText t)
                  then Just b
                  else Just $ Reference . lineToText $ t

gitActiveBranch :: IO GitBranch
gitActiveBranch = do
  branches <- gitBranches
  return . head . filter isActiveBranch $ branches

runAndExtract :: Eq a => Text -> [Text] -> (Line -> Maybe a) -> IO [a]
runAndExtract cmd args extract = fold (inproc cmd args empty) foldLine
  where foldLine = Fold step [] (nub . catMaybes)
        step acc l = extract l : acc
