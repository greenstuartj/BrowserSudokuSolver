{-# LANGUAGE OverloadedStrings #-}
module SXML where

import qualified Data.Text.Lazy as T

data SXML = Tag T.Text
          | Attr [(T.Text, T.Text)]
          | Data T.Text
          | SXML [SXML]

escape =
  (T.replace "\"" "&quot;")
  . (T.replace "\'" "&apos;")
  . (T.replace "<" "&lt;")
  . (T.replace ">" "&gt;")
  . (T.replace "&" "&amp;")

sxmlShow :: SXML -> T.Text
sxmlShow (Attr pairs) =
  T.intercalate " " [ mconcat [escape a,"=","\"", escape v,"\""]
                     | (a,v) <- pairs]
sxmlShow (Data text) =
  escape text
sxmlShow (Tag tag) =
  mconcat ["<", escape tag, ">"]
sxmlShow (SXML []) =
  ""
sxmlShow (SXML [Tag tag]) =
  sxmlShow (Tag tag)
sxmlShow (SXML (Tag tag:Attr pairs:rest))
  = mconcat [ mconcat [ "<"
                      , escape tag
                      , " "
                      , sxmlShow (Attr pairs)
                      , ">"
                      ]
            , sxmlShow (SXML rest)
            , "</"
            , escape tag
            , ">"
            ]
sxmlShow (SXML (Tag tag:rest)) =
  mconcat [ sxmlShow (Tag tag)
          , sxmlShow (SXML rest)
          , "</"
          , escape tag
          , ">"
          ]
sxmlShow (SXML (first:rest)) =
  mconcat [ sxmlShow first
          , sxmlShow (SXML rest)
          ]

instance Show SXML where
  show = T.unpack . sxmlShow
