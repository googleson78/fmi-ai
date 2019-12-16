{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Xno

import Test.Hspec

main :: IO ()
main = hspec $ describe "xno" do
  it "detects winners" do
    let initial = GameState
          { currPlayer = X
          , currBoard = Board
              [ [Just X, Just O, Just X]
              , [Just O, Just X, Just O]
              , [Just X, Just X, Just O]
              ]
          }
    evalOnce start `shouldBe` Win
