{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Handler
    ( Handler
    , runHandler
    , Env(..)
    , addHeader
    , setStatus
    , redirect
    ) where

import           BasicPrelude

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Types
import           Network.Wai

data Env = Env { envRequest :: Request }
data Meta = Meta { metaHeaders :: [Header], metaStatus :: Maybe Status }
data Bail = BailNotFound | BailRedirect ByteString

newtype Handler a = Handler (StateT Meta (ExceptT Bail (ReaderT Env IO)) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadState Meta, MonadError Bail)

runHandler :: Handler L.ByteString -> Env -> IO Response
runHandler (Handler stateT) env = do
    let exceptT = runStateT stateT (Meta [] Nothing)
        readerT = runExceptT exceptT
    resultEither <- runReaderT readerT env
    case resultEither of
        Right (body, meta) -> do
            let headers = metaHeaders meta
                status  = maybe status200 id (metaStatus meta)
            return $ responseLBS status headers body
        Left BailNotFound -> do
            return $ responseLBS status404 [] "404 NOT FOUND"
        Left (BailRedirect url) -> do
            return $ responseLBS status307 [("Location", url)] ""

redirect :: ByteString -> Handler ()
redirect url = throwError (BailRedirect url)

addHeader :: Header -> Handler ()
addHeader header = modify $ \meta -> meta { metaHeaders = metaHeaders meta <> [header] }

setStatus :: Status -> Handler ()
setStatus status = modify $ \meta -> meta { metaStatus = Just status }
