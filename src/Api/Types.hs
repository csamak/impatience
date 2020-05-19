{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Api.Types where
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                )
import           Hasql.Session                  ( Session )
import           Hasql.Pool                     ( Pool
                                                , UsageError
                                                , use
                                                )
import           Servant                        ( ServerError
                                                , Handler
                                                )
import           Control.Monad.Except           ( MonadError )

class MonadIO m => MonadDB m where
  runSession :: Session a -> m (Either UsageError a)

newtype AppM a = AppM { runAppM :: ReaderT Pool Handler a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError ServerError)

instance MonadDB AppM where
    runSession s = AppM $ do
        pool   <- ask
        result <- liftIO $ use pool s
        runAppM $ pure result
