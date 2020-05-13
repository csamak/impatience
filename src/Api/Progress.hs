{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Progress
    ( ProgressAPI
    , progressServer
    )
where

import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Either.Combinators        ( fromRight
                                                , rightToMaybe
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Int                       ( Int32 )
import           Database.Types
import           Database.Session
import           Hasql.Connection               ( Connection )
import qualified Hasql.Session                 as Session
                                                ( run )
import           Servant

instance ToJSON Progress
instance FromJSON Progress

-- brittany-disable-next-binding
type ProgressAPI = Capture "id" Int32 :> Get '[ JSON] Progress
                   :<|> ReqBody '[JSON] Progress :> Post '[ JSON] Int32

progressServer :: Connection -> Server ProgressAPI
progressServer conn = progress conn :<|> newProgress conn

progress :: Connection -> Int32 -> Handler Progress
progress conn i = do
    found <- liftIO $ Session.run (progressById i) conn
    -- handle query errors correctly
    maybe (throwError err404) pure $ fromRight Nothing found

newProgress :: Connection -> Progress -> Handler Int32
newProgress conn p = do
    newId <- liftIO $ Session.run (insertProgress p) conn
    maybe (throwError err404) pure $ rightToMaybe newId
