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
    evalOnce X initial `shouldBe` Win

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
    evalOnce X initial0 `shouldBe` Win
    evalOnce X initial1 `shouldBe` Win

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
    evalOnce X initial0 `shouldBe` Win
    evalOnce X initial1 `shouldBe` Win

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
    evalOnce X initial `shouldBe` Loss

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
    evalOnce X initial0 `shouldBe` Loss
    evalOnce X initial1 `shouldBe` Loss

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
    evalOnce X initial0 `shouldBe` Loss
    evalOnce X initial1 `shouldBe` Loss

fromStartSpec :: Spec
fromStartSpec = describe "a game from the start" do
  describe "on a 2x2" do
    let initial = GameState
          { currPlayer = X
          , currBoard = Board
              [ [Nothing, Nothing]
              , [Nothing, Nothing]
              ]
          }
    it "should win if it plays as X" $
      play X initial `shouldBe` Win
  describe "on a non-empty 2x2" do
    let initial = GameState
          { currPlayer = O
          , currBoard = Board
              [ [Just X, Nothing]
              , [Nothing, Nothing]
              ]
          }
    it "should lose if it plays as O" $
      play O initial `shouldBe` Loss
  describe "on a 3x3" do
    describe "from a blank board" do
      let initial = GameState
            { currPlayer = X
            , currBoard = Board
                [ [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                ]
            }
      it "should draw if it plays as X" $
        play X initial `shouldBe` Drawn
    describe "from a position of one placed X" do
      let initial = GameState
            { currPlayer = O
            , currBoard = Board
                [ [Just X, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                , [Nothing, Nothing, Nothing]
                ]
            }
      it "should draw if it plays as O" $
        play O initial `shouldBe` Drawn
    describe "from an obviously winning position for X" do
      let initial = GameState
            { currPlayer = X
            , currBoard = Board
                [ [Just X, Just X, Just O]
                , [Just X, Just O, Just O]
                , [Nothing, Just O, Just X]
                ]
            }
      it "should win if it plays as X" $
        play X initial `shouldBe` Win
    describe "from an obviously losing position for O" do
      let initial = GameState
            { currPlayer = O
            , currBoard = Board
                [ [Just X, Just X, Nothing]
                , [Just X, Just O, Just O]
                , [Nothing, Just X, Just X]
                ]
            }
      it "should lose if it plays as O" $
        play O initial `shouldBe` Loss
    describe "from a forced winning position for X" do
      let initial = GameState
            { currPlayer = X
            , currBoard = Board
                [ [Just X, Just X, Nothing]
                , [Just X, Just O, Just O]
                , [Nothing, Nothing, Just X]
                ]
            }
      it "should win if it plays as X" $
        play X initial `shouldBe` Win
    describe "from a winning position for X" do
      let initial = GameState
            { currPlayer = X
            , currBoard = Board
                [ [Just X, Nothing, Just O]
                , [Nothing, Just O, Nothing]
                , [Nothing, Nothing, Just X]
                ]
            }
      it "should win if it plays as X" $
        play X initial `shouldBe` Win
    describe "from a winning position for X" do
      let initial = GameState
            { currPlayer = X
            , currBoard = Board
                [ [Just X, Nothing, Nothing]
                , [Nothing, Just O, Nothing]
                , [Just O, Nothing, Just X]
                ]
            }
      it "should win if it plays as X" $
        play X initial `shouldBe` Win
    describe "from a winning position for X" do
      let initial = GameState
            { currPlayer = X
            , currBoard = Board
                [ [Just X, Just O, Just X]
                , [Nothing, Just O, Nothing]
                , [Just O, Nothing, Just X]
                ]
            }
      it "should win if it plays as X" $
        play X initial `shouldBe` Win
    describe "from a draw position for O" do
      let initial = GameState
            { currPlayer = O
            , currBoard = Board
                [ [Just X, Just O, Just X]
                , [Nothing, Just O, Nothing]
                , [Just O, Just X, Just X]
                ]
            }
      it "should draw if it plays as O" $
        play O initial `shouldBe` Drawn
    describe "from a losing position for O" do
      let initial = GameState
            { currPlayer = O
            , currBoard = Board
                [ [Just X, Nothing, Just X]
                , [Nothing, Just O, Nothing]
                , [Just O, Nothing, Just X]
                ]
            }
      it "should lose if it plays as O" $
        play O initial `shouldBe` Loss
    describe "from a losing position for O" do
      let initial = GameState
            { currPlayer = O
            , currBoard = Board
                [ [Nothing, Just X, Just O]
                , [Just X, Just X, Nothing]
                , [Just O, Nothing, Nothing]
                ]
            }
      it "should lose if it plays as O" $
        play O initial `shouldBe` Loss
    describe "from a winning position for X" do
      let initial = GameState
            { currPlayer = X
            , currBoard = Board
                [ [Nothing, Just X, Just O]
                , [Just X, Just X, Just O]
                , [Just O, Nothing, Nothing]
                ]
            }
      it "should win if it plays as X" $
        play X initial `shouldBe` Win
    describe "from a winning position for X" do
      let initial = GameState
            { currPlayer = X
            , currBoard = Board
                [ [Nothing, Just X, Just O]
                , [Just X, Nothing, Nothing]
                , [Just O, Nothing, Nothing]
                ]
            }
      it "should win if it plays as X" $
        play X initial `shouldBe` Win
