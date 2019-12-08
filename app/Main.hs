module Main (main) where

import System.IO (hSetEncoding, utf8)

import Hit (hit)


main :: IO ()
main = hSetEncoding stdout utf8 >> hit
