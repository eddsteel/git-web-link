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
module GitWebLink.FileOps where
import GitWebLink.Types
import Control.Monad.Trans.Maybe
import Data.List(stripPrefix)
import System.Directory

-- Provides a relative path from the given root
--
relDirOrFile :: FilePath -> FilePath -> MaybeT IO DirOrFile
relDirOrFile root fp =
  MaybeT $ do
    full   <- concat . (:["/", fp]) <$> getCurrentDirectory
    exists <- doesPathExist full
    cons   <- choose Dir File <$> doesDirectoryExist full
    return $ if exists
             then fmap cons (stripPrefix (root ++ "/") full)
             else Nothing

choose :: a -> a -> Bool -> a
choose a _ True  = a
choose _ b False = b
