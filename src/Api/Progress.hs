{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Api.Progress
    ( ProgressAPI
    , progressServer
    )
where

import           Api.Types
import           Control.Monad.Error.Class      ( MonadError )
import           Data.Either.Combinators        ( fromRight
                                                , rightToMaybe
                                                )
import           Data.Int                       ( Int32 )
import           Database.Types
import           Database.Session
import           Servant

-- brittany-disable-next-binding
type ProgressAPI = Capture "id" Int32 :> Get '[ JSON] Progress
                   :<|> ReqBody '[JSON] Progress :> Post '[ JSON] Int32

progressServer :: (MonadDB m, MonadError ServerError m) => ServerT ProgressAPI m
progressServer = progress :<|> newProgress

progress :: (MonadDB m, MonadError ServerError m) => Int32 -> m Progress
progress i = do
    found <- runSession (progressById i)
    -- handle query errors correctly
    maybe (throwError err404) pure $ fromRight Nothing found

newProgress :: (MonadDB m, MonadError ServerError m) => Progress -> m Int32
newProgress p = do
    newId <- runSession (insertProgress p)
    maybe (throwError err404) pure $ rightToMaybe newId
