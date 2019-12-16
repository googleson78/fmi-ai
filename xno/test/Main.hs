{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Xno

import Test.Hspec

main :: IO ()
main = hspec $ describe "xno" do
  fromStartSpec
  winnerSpec
  loserSpec

winnerSpec :: Spec
winnerSpec = describe "detects winners" do
  it "on reverse diagonals" do
    let initial = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just X, Just O, Just X]
              , [Just O, Just X, Just O]
              , [Just X, Just X, Just O]
              ]
          }
    evalOnce initial `shouldBe` Win

  it "on rows" do
    let initial0 = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just X, Just X, Just X]
              , [Just O, Just O, Just X]
              , [Just X, Just O, Just O]
              ]
          }
        initial1 = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just O, Just O, Just X]
              , [Just X, Just O, Just O]
              , [Just X, Just X, Just X]
              ]
          }
    evalOnce initial0 `shouldBe` Win
    evalOnce initial1 `shouldBe` Win

  it "on cols" do
    let initial0 = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just X, Just X, Just O]
              , [Just X, Just O, Just X]
              , [Just X, Just O, Just O]
              ]
          }
        initial1 = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just X, Just O, Just X]
              , [Just O, Just O, Just X]
              , [Just O, Just X, Just X]
              ]
          }
    evalOnce initial0 `shouldBe` Win
    evalOnce initial1 `shouldBe` Win

loserSpec :: Spec
loserSpec = describe "detects losers" do
  it "on reverse diagonals" do
    let initial = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just O, Just X, Just O]
              , [Just X, Just O, Just X]
              , [Just O, Just O, Just X]
              ]
          }
    evalOnce initial `shouldBe` Loss

  it "on rows" do
    let initial0 = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just O, Just O, Just O]
              , [Just X, Just X, Just O]
              , [Just O, Just X, Just X]
              ]
          }
        initial1 = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just X, Just X, Just O]
              , [Just O, Just X, Just X]
              , [Just O, Just O, Just O]
              ]
          }
    evalOnce initial0 `shouldBe` Loss
    evalOnce initial1 `shouldBe` Loss

  it "on cols" do
    let initial0 = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just O, Just O, Just X]
              , [Just O, Just X, Just O]
              , [Just O, Just X, Just X]
              ]
          }
        initial1 = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just O, Just X, Just O]
              , [Just X, Just X, Just O]
              , [Just X, Just O, Just O]
              ]
          }
    evalOnce initial0 `shouldBe` Loss
    evalOnce initial1 `shouldBe` Loss

fromStartSpec :: Spec
fromStartSpec = describe "a game from the start" do
  it "should win on a 2x2" do
    let initial = GameState
          { currPlayer = X
          , currBoard = Board
              [ [Nothing, Nothing]
              , [Nothing, Nothing]
              ]
          }
    maximumTree (fmap evalOnce $ allGames initial) `shouldBe` Win
  it "should draw on a 3x3" do
    let initial = GameState
          { currPlayer = X
          , currBoard = Board
              [ [Nothing, Nothing, Nothing]
              , [Nothing, Nothing, Nothing]
              , [Nothing, Nothing, Nothing]
              ]
          }
    maximumTree (fmap evalOnce $ allGames initial) `shouldBe` Drawn