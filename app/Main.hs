{-# LANGUAGE OverloadedStrings #-}
module Main where

import SXML
import Sudoku
import Web.Scotty
import qualified Data.Text.Lazy as T

makeCell :: T.Text -> T.Text -> SXML
makeCell name value =
  SXML
  [ Tag "input"
  , Attr [ ("type",      "text")
         , ("id",        name)
         , ("name",      name)
         , ("maxlength", "1")
         , ("size",      "1")
         , ("style",     "width: 25px")
         , ("value",     value)
         ]
  ]

buildGrid :: [T.Text] -> [SXML]
buildGrid values = addDivider rowDiv $ toGrid [] $ buildCells 1 1 values []
  where buildCells 10 _ _ acc =
          reverse acc
        buildCells row 10 vals acc =
          buildCells (succ row) 1 vals acc
        buildCells row col (val:vals) acc =
          buildCells row (succ col) vals (SXML
                                          [ Tag "td"
                                          , makeCell
                                            (mconcat
                                             $ map (T.pack . show) [row, col]) val]:acc)
        toGrid acc [] =
          reverse acc
        toGrid acc g =
          toGrid ((SXML
                   (Tag "tr":(addDivider (SXML
                                           [ Tag "td"
                                           , Attr [("style",
                                                     "text-align: center")]
                                           , Data "|"])
                               (take 9 g)))):acc) (drop 9 g)
        addDivider _ list@[_,_,_] =
          list
        addDivider divd list =
          (take 3 list) ++ [divd] ++ (addDivider divd (drop 3 list))
        rowPartial = replicate 9 (SXML
                                   [ Tag "td"
                                   , Attr [("style", "text-align: center")]
                                   , Data "----"])
        rowDiv = SXML $ Tag "tr" : (addDivider (SXML
                                                 [ Tag "td"
                                                 , Attr [("style", "text-align: center")]
                                                 , Data "+"]) rowPartial)

page :: Eq a => [(a, T.Text)] -> SXML
page prms =
  SXML [ Tag "head"
       , SXML [ Tag "title"
              , Data "Sudoku Solver"
              ]
       , SXML [ Tag "style"
              , Attr [ ("type", "text/css") ]
              , Data "\n.content {\n\tmargin: auto;\n\tmax-height: 500px;\n\tmax-width: 500px;\n\t}\n"
              ]
       , SXML [ Tag "body"
              , SXML [ Tag "div"
                     , Attr [ ("class", "content" ) ]
                     , SXML [ Tag "h1", Data "Sudoku" ]
                     , SXML [ Tag "p"
                            , Data "Enter numbers into the cells in the grid below" ]
                     , SXML [ Tag "form"
                            , SXML [ Tag "table"
                                   , Attr [ ("style", "width:20%") ]
                                   , SXML (if length prms < 9*9
                                           then buildGrid $ replicate (9 * 9) (T.pack "")
                                           else buildSBoard solved)
                                   ]
                            , SXML (if prms == []
                                    then [Tag "br"]
                                    else impossibleOrBreak solved)
                            , SXML [ Tag "input"
                                   , Attr [ ("type", "submit")
                                          , ("value", "Solve")
                                          , ("style", "width: 100px")
                                          ]
                                   ]
                            ]
                     , SXML [ Tag "form",
                              SXML[ Tag "input"
                                  , Attr [ ("type", "submit")
                                         , ("value", "Clear")
                                         , ("style", "width: 100px")
                                         ]
                                  ]
                            ]
                     ]
              ]
       ]
  where board = map (\(_,n) ->
                       if not (n `elem` ["1","2","3","4","5","6","7","8","9"])
                       then "."
                       else n)
                prms
        solved = solve $ T.unpack $ mconcat board
        buildSBoard (Left _) = buildGrid $ map (\(_,n) -> n) prms
        buildSBoard (Right b) = buildGrid $ map (T.pack . (:[])) b
        impossibleOrBreak (Left _) = [ Tag "p"
                                     , Data "The board is "
                                     , SXML [ Tag "em"
                                            , Data "impossible"
                                            ]
                                     ]
        impossibleOrBreak (Right _) = [Tag "br"]

main :: IO ()
main = scotty 8080 $
  get "/" $ do
  prms <- params
  html $ sxmlShow $ page prms
