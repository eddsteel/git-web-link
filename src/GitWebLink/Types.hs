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

module GitWebLink.Types where

import Data.Text(Text)
import qualified Data.Text as T
import Network.URL

data GitBranch = ActiveBranch Text | Branch Text | Reference GitReference deriving (Show, Eq)
type GitReference = Text
type IsActive = Bool
data DirOrFile = Root | Dir FilePath | File FilePath deriving (Show, Eq)
type GHUser = Text
type Project = Text

data Region = Line Int | Range Int Int deriving (Show, Eq)

type GitName = Text
type SshUser = Text
type SshHost = Text

-- Git Remote as understood by git-remote
data GitRemote = HttpRemote { name :: GitName, raw :: Text, url :: URL }
               | SshRemote  { name :: GitName, raw :: Text, user :: SshUser,
                              sshhost :: SshHost, sshpath :: Text}
                 deriving (Show, Eq)

-- parameters specifying what kind of link to generate
data GWLParameters = HomeP
                   | CommitP {commit :: Text}
                   | BranchP {branch :: GitBranch}
                   | PathP   {branch :: GitBranch, path :: DirOrFile}
                   | RegionP {branch :: GitBranch, region :: Region, filePath :: FilePath}
                   deriving (Show, Eq)

-- parameters provided on the command line
data InputParameters =
  Params { pRemote :: Maybe Text
         , pBranch :: Maybe GitBranch
         , pFilePath :: Maybe Text
         , pStart :: Maybe Int
         , pEnd :: Maybe Int
         , pDeref :: Maybe Bool
         , pCommit :: Maybe Text
         } deriving Show

hostProtocol :: Host -> String
hostProtocol h = case protocol h of
                   (HTTP True) -> "https:"
                   (HTTP False) -> "http:"
                   (FTP True) -> "ftps:"
                   (FTP False) -> "ftp:"
                   (RawProt s) -> s ++ ":"

-- nothing available for this already?
pathJoin :: [Text] -> Text
pathJoin parts = T.concat $ parts >>= \f -> ["/", f]

isActiveBranch :: GitBranch -> IsActive
isActiveBranch (ActiveBranch _) = True
isActiveBranch (Branch _) = False
isActiveBranch (Reference _) = False

nameOfBranch :: GitBranch -> Text
nameOfBranch (ActiveBranch b) = b
nameOfBranch (Branch b) = b
nameOfBranch (Reference r) = r

pRegion :: InputParameters -> Maybe Region
pRegion (Params _ _ _ (Just s) (Just e) _ _) = Just $ Range s e
pRegion (Params _ _ _ (Just l) Nothing _ _) = Just $ Line l
pRegion _ = Nothing
