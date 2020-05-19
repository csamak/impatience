{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Api.Job
    ( JobAPI
    , jobServer
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
type JobAPI = Capture "id" Int32 :> Get '[ JSON] Job
              :<|> ReqBody '[JSON] Job :> Post '[ JSON] Int32
              :<|> Capture "id" Int32 :> "progresses" :> Get '[ JSON] [Progress]

jobServer :: (MonadDB m, MonadError ServerError m) => ServerT JobAPI m
jobServer = job :<|> newJob :<|> jobProgresses

job :: (MonadDB m, MonadError ServerError m) => Int32 -> m Job
job i = do
    found <- runSession (jobById i)
    maybe (throwError err404) pure $ fromRight Nothing found

newJob :: (MonadDB m, MonadError ServerError m) => Job -> m Int32
newJob j = do
    newId <- runSession (insertJob j)
    maybe (throwError err404) pure $ rightToMaybe newId

jobProgresses :: (MonadDB m, MonadError ServerError m) => Int32 -> m [Progress]
jobProgresses i = do
    found <- runSession (progressesByJob i)
    maybe (throwError err404) pure $ rightToMaybe found
