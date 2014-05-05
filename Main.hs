module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar


newtype Client = Client Int

maxlen = 5

mythread :: Chan Client -> MVar Int -> IO ()
mythread queue counter = do
	putStrLn "Barber checks queue"
	(Client delay) <- readChan queue
	threadDelay delay
	modifyMVar_ counter (\i -> return $ i-1)
	putStrLn "Client is ready"
	mythread queue counter

controller :: Chan Client -> MVar Int -> IO ()
controller queue counter = do
	threadDelay 500
	let client = Client 800
	i <- takeMVar counter
	if i < maxlen
		then do
			writeChan queue client
			putMVar counter $ i + 1
		else do
			putMVar counter i
			putStrLn "Queue is full"
	controller queue counter

main = do
	queue <- newChan
	counter <- newMVar 0
	forkIO $ mythread queue counter
	putStrLn $ "Starting client producement"
	controller queue counter


