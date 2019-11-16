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

module GitWebLink.Types where

import Data.Text(Text)
import qualified Data.Text as T
import Network.URL

data GitReference = Branch Text
                  | Reference Text
                  | Tag Text
                  deriving (Show, Eq)

type IsActive = Bool
data DirOrFile = Root | Dir FilePath | File FilePath deriving (Show, Eq)
type User = Text
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
                   | CommitP { ref :: GitReference } -- commit summary, not repo at ref
                   | RefP    { ref :: GitReference }
                   | PathP   { ref :: GitReference, path :: DirOrFile }
                   | RegionP { ref :: GitReference, region :: Region, filePath :: FilePath }
                   deriving (Show, Eq)

-- parameters provided on the command line
data InputParameters =
  Params { pRemote :: Maybe Text
         , pCommit :: Maybe GitReference
         , pTag :: Maybe GitReference
         , pBranch :: Maybe GitReference
         , pFilePath :: Maybe Text
         , pStart :: Maybe Int
         , pEnd :: Maybe Int
         , pDeref :: Bool
         , pOpen :: Bool
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

nameOfRef :: GitReference -> Text
nameOfRef (Branch b)    = b
nameOfRef (Tag t)       = t
nameOfRef (Reference r) = r

pRegion :: InputParameters -> Maybe Region
pRegion Params{pStart=Just s, pEnd=Just e} = Just $ Range s e
pRegion Params{pStart=Just l}              = Just $ Line l
