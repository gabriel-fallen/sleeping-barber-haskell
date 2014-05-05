module Main where

import Control.Concurrent
import Control.Concurrent.MVar


newtype Client = Client Int

maxlen = 5

mythread :: MVar [Client] -> IO ()
mythread queue = do
	putStrLn "Barber checks queue"
	((Client delay):rest) <- takeMVar queue
	if null rest
		then do
			threadDelay delay
			putStrLn "Client is ready"
			mythread queue
		else do
			putMVar queue rest
			threadDelay delay
			putStrLn "Client is ready"
			mythread queue


controller :: MVar [Client] -> IO ()
controller queue = do
	threadDelay 500
	let client = Client 800
	isEmpty <- isEmptyMVar queue
	if isEmpty
		then putMVar queue [client] >> controller queue
		else do
			q <- takeMVar queue
			if (length q) >= maxlen
				then putMVar queue q >> controller queue
				else putMVar queue (q ++ [client]) >> controller queue

main = do
	queue <- newEmptyMVar :: IO (MVar [Client])
	forkIO $ mythread queue
	putStrLn $ "Starting client producement"
	controller queue
