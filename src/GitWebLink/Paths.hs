{-# LANGUAGE OverloadedStrings #-}
module GitWebLink.Paths where

import Data.Text

-- For non-nix systems use whatever PATH is.
-- For nix systems nix will replace this with the path to git.

git :: Text
git = "git"
