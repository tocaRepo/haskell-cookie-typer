module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)
import System.IO (hFlush, stdout)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')

-- Game state
data GameState = GameState
  { cookies :: Int
  , cps :: Int -- Cookies per second
  } deriving (Show)

-- Initial game state
initialState :: GameState
initialState = GameState { cookies = 0, cps = 1 }

-- Display the current state to the player
displayState :: GameState -> IO ()
displayState state = do
  putStrLn "─────────────────────────────────────"
  putStrLn $ "Cookies: " ++ show (cookies state)
  putStrLn $ "Cookies per second (CPS): " ++ show (cps state)
  putStrLn "Actions: (b)ake, (u)pgrade cps, (q)uit"
  putStrLn "─────────────────────────────────────"
  putStr "Choose an action: \n"

-- Main game loop
gameLoop :: IORef GameState -> IO ()
gameLoop stateRef = do
  state <- readIORef stateRef
  displayState state
  hFlush stdout
  action <- getLine
  case action of
    "b" -> modifyIORef' stateRef click >> gameLoop stateRef
    "u" -> modifyIORef' stateRef upgrade >> gameLoop stateRef
    "q" -> putStrLn "Goodbye!"
    _   -> do
      putStrLn "Invalid action, try again."
      gameLoop stateRef

-- Handle a click/bake, increasing cookies, leaving click as i might migrate to an actual ui
click :: GameState -> GameState
click state = state { cookies = cookies state + 1 }

-- Handle an upgrade, increasing cookies per second
upgrade :: GameState -> GameState
upgrade state
  | cookies state >= upgradeCost = state { cookies = cookies state - upgradeCost, cps = cps state + 1 }
  | otherwise = state
  where
    upgradeCost = 10 * cps state -- Simple cost formula

-- Background process to generate cookies based on CPS
cookieGenerator :: IORef GameState -> IO ()
cookieGenerator stateRef = forever $ do
  threadDelay 1000000 -- 1 second
  modifyIORef' stateRef incrementCookies
  state <- readIORef stateRef
  displayState state

-- Increment cookies based on CPS
incrementCookies :: GameState -> GameState
incrementCookies state = state { cookies = cookies state + cps state }

-- Main function
main :: IO ()
main = do
  putStrLn "─────────────────────────────────────"
  putStrLn "        Welcome to Cookie Typer!"
  putStrLn "─────────────────────────────────────"
  stateRef <- newIORef initialState
  _ <- forkIO $ cookieGenerator stateRef -- Start background cookie generator
  gameLoop stateRef
