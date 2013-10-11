{-# LANGUAGE RankNTypes #-}

module Pipes.Example where

import Prelude hiding (take, print)
import Control.Monad
import Pipes
import Pipes.Prelude
import Pipes.Safe

fromList :: Monad m => [a] -> Producer a m ()
fromList l = forM_ l yield

safePrint :: (Show a) => ConsumerSafe a IO r
safePrint = bracket
            (putStrLn "safePrint in")
            (\() -> putStrLn "safePrint out")
            (\() -> hoist lift print)

mainEffect :: EffectSafe IO ()
mainEffect = producer >-> consumer
  where
    producer = do
      hoist lift $ fromList [1..7 :: Int]
      liftIO $ putStrLn "Producer done."

    consumer = forever $ do
      runSafeLocal $ take 2 >-> safePrint

main :: IO ()
main = runSafeT $ runEffect $ mainEffect
