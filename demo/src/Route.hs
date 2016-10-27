{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Route
    ( Route(..)
    , parseRoute
    ) where

import           BasicPrelude

import           Data.Attoparsec.ByteString.Char8

data Route = Home | Message Int

parseRoute :: ByteString -> Either String Route
parseRoute = parseOnly parser

parser :: Parser Route
parser = choice
    [ string "/" <* endOfInput >> return Home
    , string "/messages/" >> fmap Message (decimal <* endOfInput)
    ]
