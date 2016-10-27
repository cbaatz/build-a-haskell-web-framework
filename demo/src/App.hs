{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module App
    ( app
    ) where

import           BasicPrelude

import           Control.Monad.Reader       (asks)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Types
import           Network.Wai

import           Handler
import           Route

app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app req respond = do
    let methodEither = parseMethod (requestMethod req)
        routeEither  = parseRoute (rawPathInfo req)
        env = Env req
    case (methodEither, routeEither) of
        (Right method, Right route) -> do
            runHandler (router method route) env >>= respond
        _ -> runHandler notFound env >>= respond

router :: StdMethod -> Route -> Handler L.ByteString
router GET Home = do
    req <- asks envRequest
    setStatus status200
    return (L8.pack (show $ remoteHost req))
router GET (Message i) = do
    when (i == 3) (redirect "/")
    return (L8.pack ("Message #" <> (show i)))
router _ _ = notFound

notFound :: Handler L.ByteString
notFound = do
    addHeader ("Content-type", "text/html")
    setStatus status404
    return $ "<html><body><h1>404 NOT FOUND</h1></body></html>"
