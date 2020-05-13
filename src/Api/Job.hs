{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Job where

import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
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

-- brittany-disable-next-binding
type JobAPI = Capture "id" Int32 :> Get '[ JSON] Job
              :<|> ReqBody '[JSON] Job :> Post '[ JSON] Int32

jobServer :: Connection -> Server JobAPI
jobServer conn = job conn :<|> newJob conn

job :: Connection -> Int32 -> Handler Job
job conn i = do
    found <- liftIO $ Session.run (jobById i) conn
    maybe (throwError err404) pure $ fromRight Nothing found

newJob :: Connection -> Job -> Handler Int32
newJob conn j = do
    newId <- liftIO $ Session.run (insertJob j) conn
    maybe (throwError err404) pure $ rightToMaybe newId
