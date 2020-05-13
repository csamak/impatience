{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Job
    ( JobAPI
    , jobServer
    )
where

import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Either.Combinators        ( fromRight
                                                , rightToMaybe
                                                )
import           Data.Int                       ( Int32 )
import           Database.Types
import           Database.Session
import           Hasql.Connection               ( Connection )
import qualified Hasql.Session                 as Session
                                                ( run )
import           Servant

instance ToJSON Job
instance FromJSON Job

-- brittany-disable-next-binding
type JobAPI = Capture "id" Int32 :> Get '[ JSON] Job
              :<|> ReqBody '[JSON] Job :> Post '[ JSON] Int32
              :<|> Capture "id" Int32 :> "progresses" :> Get '[ JSON] [Progress]

jobServer :: Connection -> Server JobAPI
jobServer conn = job conn :<|> newJob conn :<|> jobProgresses conn

job :: Connection -> Int32 -> Handler Job
job conn i = do
    found <- liftIO $ Session.run (jobById i) conn
    maybe (throwError err404) pure $ fromRight Nothing found

newJob :: Connection -> Job -> Handler Int32
newJob conn j = do
    newId <- liftIO $ Session.run (insertJob j) conn
    maybe (throwError err404) pure $ rightToMaybe newId

jobProgresses :: Connection -> Int32 -> Handler [Progress]
jobProgresses conn id = do
    found <- liftIO $ Session.run (progressesByJob id) conn
    maybe (throwError err404) pure $ rightToMaybe found
