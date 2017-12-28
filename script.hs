#!/usr/bin/env stack
-- stack --resolver lts-10.0 script --package shake
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Data.Monoid ((<>))

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Development.Shake.Classes

buildDir :: FilePath
buildDir = ".shake_build"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do
    want [ "commit" ]
    phony "clean" $ do
        putNormal $ "Cleaning files in " <> show buildDir
        removeFilesAfter buildDir ["//*"]
        putNormal $ "Cleaning files in " <> "_site"
        removeFilesAfter "_site" ["//*"]
        putNormal $ "Cleaning files in " <> "www"
        removeFilesAfter "www" ["//*"]

    phony "build" $
        cmd_ "stack" "exec" "--" "primetype" "rebuild"
    phony "rebuild" $
        need ["build"]
    "www/.git/HEAD" %> \_ -> do
        cmd_ "rm -rf www"
        cmd_ "git" "clone" "git@github.com:primetype/primetype.github.io.git" "www"
    phony "commit" $ do
        need ["rebuild", "www/.git/HEAD"]
        cmd_ (Cwd "www") "rm" "-r" "-f" "*"
        cmd_ (Cwd "www") "touch" ".nojekyll"
        writeFile' "www/CNAME" "www.primetype.co.uk"
        c <- fmap (\c -> "../_site" </> c) <$> getDirectoryContents "_site"
        cmd_ (Cwd "www") "cp" "-r" c "."
        cmd_ (Cwd "www") "git" "add" "."
        cmd_ (Cwd "www") "git" "commit" ["--message=\"Update website\""]
        cmd_ (Cwd "www") "git" "push" "origin" "master"
