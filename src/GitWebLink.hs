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
import GitWebLink.FileOps(dirOrFile)

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
  return $ dispatchArgs remotes branch args

dispatchArgs :: Map Text GitRemote -> GitBranch -> [Text] -> Maybe URI
dispatchArgs rs _ [] = Nothing
dispatchArgs rs _ ("-b":branch:rest) = dispatchArgs rs branch rest -- turrible
dispatchArgs rs b (r:[]) = M.lookup r rs >>= mainBranch b
dispatchArgs rs b (r:p:[]) = M.lookup r rs >>= mainFile b (dirOrFile p)
dispatchArgs rs b (r:p:l:[]) = M.lookup r rs >>= mainLine b (dirOrFile p) (int l)
dispatchArgs rs b (r:p:s:e:[]) = M.lookup r rs >>= mainRange b (dirOrFile p) (int s) (int e)
dispatchArgs _ _ _ = Nothing

int :: Text -> Int
int = read . T.unpack

mainBranch :: GitBranch -> GitRemote -> Maybe URI
mainBranch "master" = fmap mkLinkHome . recogniseRepo
mainBranch b = fmap (mkLinkBranch b) . recogniseRepo

mainFile :: GitBranch -> DirOrFile -> GitRemote -> Maybe URI
mainFile b df = fmap (mkLinkFile b df) . recogniseRepo

mainLine :: GitBranch -> DirOrFile -> Int -> GitRemote -> Maybe URI
mainLine b df i = fmap (mkLinkLine b df i) . recogniseRepo

mainRange :: GitBranch -> DirOrFile -> Int -> Int -> GitRemote -> Maybe URI
mainRange b df s e = fmap (mkLinkRange b df s e) . recogniseRepo
