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
module GitWebLink.FileOps where

import GitWebLink.Types
import Data.Text(Text)
import Data.List(stripPrefix)
import qualified Data.Text as T
import System.FilePath


dirOrFile :: Text -> DirOrFile
dirOrFile = (\fp -> ( if hasTrailingPathSeparator fp
                        then Dir (init fp)
                        else File fp)) . T.unpack

normalize :: FilePath -> FilePath -> DirOrFile -> Maybe DirOrFile
normalize root wd (Dir d) = Dir <$> norm root wd d
normalize root wd (File f) = File <$> norm root wd f

norm :: FilePath -> FilePath -> FilePath -> Maybe FilePath
norm root wd = stripPrefix root . (wd ++)
