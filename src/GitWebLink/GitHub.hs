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
module GitWebLink.GitHub where

import Data.Text(Text)
import qualified Data.Text as T
import Network.URL
import Network.URI
import GitWebLink.Types

glURI :: Text -> String -> URI
glURI = githubstyle $ Host (HTTP True) "gitlab.com" Nothing

ghURI :: Text -> String -> URI
ghURI = githubstyle $ Host (HTTP True) "github.com" Nothing

bbURI :: Text -> String -> URI
bbURI = githubstyle $ Host (HTTP True) "bitbucket.org" Nothing

gheURI :: Host -> Text -> String -> URI
gheURI = githubstyle

githubstyle :: Host -> Text -> String -> URI
githubstyle h p f = URI (hostProtocol h) (Just (URIAuth "" (host h) "")) (T.unpack p) "" f

ghPath :: User -> Project -> Text
ghPath u p = pathJoin [u, p]

ghFile :: User -> Project -> GitReference -> DirOrFile -> Text
ghFile u p b (File fp) = pathJoin [u, p, "blob", (nameOfRef b), (T.pack fp)]
ghFile u p b Root = pathJoin [u, p, "tree", (nameOfRef b)]
ghFile u p b (Dir fp)  = pathJoin [u, p, "tree", (nameOfRef b), (T.pack fp)]

bbFile :: User -> Project -> GitReference -> DirOrFile -> Text
bbFile u p b df = pathJoin [u, p, "src", (nameOfRef b), path df]
  where path (Root) = T.pack ""
        path (File f) = T.pack f
        path (Dir d) = T.pack d

bbPath :: User -> Project -> Text
bbPath u p = pathJoin [u, p, "src", "default"]
