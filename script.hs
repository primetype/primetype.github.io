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

    "_site/.travis.yaml" %> \fp -> do
        need [ ".travis.yaml" ]
        copyFile'  ".travis.yaml" fp
    phony "rebuild" $
        cmd_ "stack" "exec" "--" "primetype" "rebuild"
    phony "build" $
        need [ "_site/.travis.yaml", "rebuild"]
