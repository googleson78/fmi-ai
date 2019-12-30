{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Test.Hspec

import Bfs (Board(..), Spot(..), findPath)

main :: IO ()
main = hspec $ describe "bfs" do
  it "works on an example" do
    let board = Board $
          [ [Normal, Normal, Normal]
          , [Normal, Blocked, Normal]
          , [Normal, Normal, Normal]
          ]
        start = (2, 2)
        end = (0, 1)
    findPath board start end `shouldBe`
      Just [(2, 2), (1, 2), (0, 2), (0, 1)]

  it "works on one step example" do
    let board = Board $
          [ [Normal, Normal, Normal]
          , [Normal, Blocked, Normal]
          , [Normal, Normal, Normal]
          ]
        start = (2, 2)
        end = (2, 1)
    findPath board start end `shouldBe`
      Just [(2, 2), (2, 1)]

  it "works on an example where a portal path is shorter" do
    let board = Board $
          [ [Normal, Portal (1, 2), Normal]
          , [Normal, Blocked, Portal (0,1)]
          , [Normal, Normal, Normal]
          ]
        start = (2, 2)
        end = (0, 0)
    findPath board start end `shouldBe`
      Just [(2, 2), (1, 2), (0, 1), (0, 0)]

  it "works on an example where a portal path is longer" do
    let board = Board $
          [ [Portal (1, 2), Normal, Normal]
          , [Normal, Blocked, Portal (0, 0)]
          , [Normal, Normal, Normal]
          ]
        start = (2, 2)
        end = (2, 0)
    findPath board start end `shouldBe`
      Just [(2, 2), (2, 1), (2, 0)]

  it "works on an example where a portal path is longer" do
    let board = Board $
          [ [Portal (1, 2), Normal, Normal]
          , [Normal, Blocked, Portal (0, 0)]
          , [Normal, Normal, Normal]
          ]
        start = (2, 2)
        end = (2, 0)
    findPath board start end `shouldBe`
      Just [(2, 2), (2, 1), (2, 0)]

  it "finds an identity path" do
    let board = Board $
          [ [Normal, Normal, Normal]
          , [Blocked, Blocked, Blocked]
          , [Normal, Normal, Normal]
          ]
        start = (2, 2)
        end = (2, 2)
    findPath board start end `shouldBe`
      Just [(2, 2)]

  it "can't find a path when there is none" do
    let board = Board $
          [ [Normal, Normal, Normal]
          , [Blocked, Blocked, Blocked]
          , [Normal, Normal, Normal]
          ]
        start = (2, 2)
        end = (0, 0)
    findPath board start end `shouldBe`
      Nothing

  it "can't find a path to a blocked spot" do
    let board = Board $
          [ [Normal, Normal, Normal]
          , [Normal, Blocked, Normal]
          , [Normal, Normal, Normal]
          ]
        start = (2, 2)
        end = (1, 1)
    findPath board start end `shouldBe`
      Nothing
