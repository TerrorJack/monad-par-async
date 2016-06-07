module Control.Monad.Par.Async where

import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Par.Class
import Control.Monad.Par.IO
import Data.Semigroup

newtype Async a = Async { unAsync :: ParIO (Either SomeException a) }

instance Functor Async where
    fmap f m = Async $ fmap (fmap f) $ unAsync m

instance Applicative Async where
    pure = return

    f <*> a = Async $ do
        fi <- spawn_ $ unAsync f
        ai <- spawn_ $ unAsync a
        fr <- get fi
        case fr of
            Left e -> return $ Left e
            Right f' -> do
                ar <- get ai
                case ar of
                    Left e' -> return $ Left e'
                    Right a' -> return $ Right $ f' a'

instance Monad Async where
    return = Async . return . Right

    m >>= f = Async $ do
        r <- unAsync m
        case r of
            Left e -> return $ Left e
            Right a -> unAsync $ f a

instance MonadIO Async where
    liftIO = Async . liftIO . try

data AsyncEmptyException = AsyncEmptyException
    deriving (Exception, Show)

instance Alternative Async where
    empty = Async $ return $ Left $ SomeException AsyncEmptyException

    a <|> b = Async $ do
        ai <- spawn_ $ unAsync a
        bi <- spawn_ $ unAsync b
        ar <- get ai
        case ar of
            Left _ -> get bi >>= return
            Right a' -> return $ Right a'

instance Semigroup a => Semigroup (Async a) where
    a <> b = (<>) <$> a <*> b

instance Monoid a => Monoid (Async a) where
    mempty = return mempty
    a `mappend` b = mappend <$> a <*> b