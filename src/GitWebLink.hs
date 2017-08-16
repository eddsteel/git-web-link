{-# LANGUAGE OverloadedStrings #-}
-- git-web-link provide links to web UIs for git projects
-- Copyright (C) 2017 Edd Steel
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
import GitWebLink.GitRepository
import GitWebLink.Types
import GitWebLink.FileOps(dirOrFile, normalize)

import Data.Text(Text)
import qualified Data.Text as T
import Network.URI
import Turtle.Line
import Data.Map(Map)
import qualified Data.Map as M


runArguments :: [Text] -> IO (Maybe URI)
runArguments args = do
  branch <- activeGitBranch
  remotes <- gitRemotesByKey
  root <- gitRoot
  return $ dispatchArgs remotes branch root args

dispatchArgs :: Map Text GitRemote -> GitBranch -> (DirOrFile -> Maybe DirOrFile) -> [Text] -> Maybe URI
dispatchArgs rs _ _ [] = Nothing
dispatchArgs rs _ norm ("-b":branch:rest) = dispatchArgs rs branch norm rest -- turrible
dispatchArgs rs b _ (r:[]) = M.lookup r rs >>= mainBranch b
dispatchArgs rs b norm (r:p:[]) = M.lookup r rs >>= mainFile b (norm $ dirOrFile p)
dispatchArgs rs b norm (r:p:l:[]) = M.lookup r rs >>= mainLine b (norm $ dirOrFile p) (int l)
dispatchArgs rs b norm (r:p:s:e:[]) = M.lookup r rs >>= mainRange b (norm $ dirOrFile p) (int s) (int e)
dispatchArgs _ _ _ _ = Nothing

int :: Text -> Int
int = read . T.unpack

mainBranch :: GitBranch -> GitRemote -> Maybe URI
mainBranch "master" = fmap mkLinkHome . recogniseRepo
mainBranch b = fmap (mkLinkBranch b) . recogniseRepo

mainFile :: GitBranch -> Maybe DirOrFile -> GitRemote -> Maybe URI
mainFile b (Just df) = fmap (mkLinkFile b df) . recogniseRepo
mainFile _ _ = Nothing

mainLine :: GitBranch -> Maybe DirOrFile -> Int -> GitRemote -> Maybe URI
mainLine b (Just df) i = fmap (mkLinkLine b df i) . recogniseRepo
mainLine _ _ = Nothing

mainRange :: GitBranch -> Maybe DirOrFile -> Int -> Int -> GitRemote -> Maybe URI
mainRange b (Just df) s e = fmap (mkLinkRange b df s e) . recogniseRepo
mainRange _ _ = Nothing
