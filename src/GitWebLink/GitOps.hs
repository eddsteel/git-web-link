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
                        , gitBranches
                        , gitBranchReference
                        , activeGitBranch
                        , gitRootDir) where
import GitWebLink.Types
import GitWebLink.GitRemote
import GitWebLink.Parsing
import Data.List(nub)
import Data.Map(Map, fromList)
import Data.Maybe(catMaybes)
import Turtle hiding (FilePath)
import Safe(headMay)
import qualified Data.Text as T

gitRemotes :: IO [GitRemote]
gitRemotes = runAndExtract "git" ["remote", "-v"] remoteFromLine

gitRemotesByKey :: IO (Map Text GitRemote)
gitRemotesByKey = do
  remotes <- gitRemotes
  let names = fmap name remotes
  return . fromList $ zip names remotes

gitBranches :: IO [GitBranch]
gitBranches = runAndExtract "git" ["branch"] branchFromLine

gitRootDir :: IO FilePath
gitRootDir = fmap (T.unpack . T.strip . linesToText) $ runAndExtract "git" ["rev-parse", "--show-toplevel"] Just

-- Dereference a named branch e.g. "master" to a ref e.g. "8ce13c8bed94afba0615b515f465447073b366c8".
-- Uses `git show-ref` because
-- > Use of this utility is encouraged in favor of directly accessing files under the .git
-- > directory.
--
gitBranchReference :: GitBranch -> IO (Maybe GitBranch)
gitBranchReference b = headMay <$> runAndExtract "git" ["show-ref", "-s", "--verify", refHead] toRef
  where refHead = T.concat ["refs/heads/", branchName b]
        toRef t = if T.null (lineToText t)
                  then Nothing
                  else Just . Reference . lineToText $ t

activeGitBranch :: IO GitBranch
activeGitBranch = do
  branches <- gitBranches
  return . head . filter isActiveBranch $ branches

runAndExtract :: Eq a => Text -> [Text] -> (Line -> Maybe a) -> IO [a]
runAndExtract cmd args extract = fold (inproc cmd args empty) foldLine
  where foldLine = Fold step [] (nub . catMaybes)
        step acc l = extract l : acc
