{-# LANGUAGE LambdaCase #-}

module Screenie
  ( withMonitoring
  , updateMonitoring
  , updateMonitoringProgress )
  where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception ( evaluate )
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.Fixed
import Data.Foldable ( for_ )
import Data.Int
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr
import Foreign.Storable ( peek, poke )
import Control.Monad.Catch
import Control.Monad.IO.Class
import GHC.Stats
import System.Console.ANSI
import System.IO ( hFlush, stdout )
import System.IO.Unsafe

foreign import ccall "get_window_size" c_get_window_size :: Ptr Int32 -> Ptr Int32 -> IO Int
foreign import ccall "set_raw_terminal_mode" c_set_raw_terminal_mode :: IO (Ptr Termios)
foreign import ccall "restore_terminal_mode" c_restore_terminal_mode :: Ptr Termios -> IO ()
foreign import ccall unsafe "mk_wcswidth" c_wcswidth :: Ptr Word32 -> CSize -> IO CInt

-- Wrapper around c_wcswidth, for Char only. Returns number of columns the
-- character occupies in a terminal. Or at least tries to.
charWidth :: Char -> Int
charWidth ch = unsafePerformIO $ alloca $ \ord_ptr -> do
  let ord_value = fromIntegral $ ord ch
  poke ord_ptr ord_value
  fromIntegral <$> c_wcswidth ord_ptr 1
{-# INLINE charWidth #-}

data MonitorStatus = MonitorStatus String !(Maybe Double) Bool

-- Opaque data used with cbits code (it's "struct termios")
data Termios

-- More haskell-like wrapper around get_window_size() C function
getWindowSize :: MonadIO m => m (Int, Int)
getWindowSize = liftIO $ alloca $ \w_ptr -> alloca $ \h_ptr -> do
  -- If there is a failure, don't crash. It's not supposed to be possible for
  -- the monitor to crash under normal circumstances. Fall back to 80x24 if
  -- getting window size fails.
  result <- c_get_window_size w_ptr h_ptr
  w <- peek w_ptr
  h <- peek h_ptr
  return $ if result /= 0 then (80, 24) else (fromIntegral w, fromIntegral h)

-- Monitoring action represents some action running with a descriptive name
-- (that may change because the name itself is being evaluated, hence it's in a
-- TVar). Monitorings can be nested per thread so this is list-like data.
--
-- Not exported.
data MonitoringAction = MonitoringAction (TVar MonitorStatus) (Maybe MonitoringAction)
  deriving ( Eq )

-- Takes a MonitoringAction and resolves it to a list, with action names
-- deciphered.
monitoringActionToList :: MonadIO m => MonitoringAction -> m [MonitorStatus]
monitoringActionToList (MonitoringAction action_name next) = liftIO $
  (:) <$>
    atomically (readTVar action_name) <*> case next of
      Nothing -> pure []
      Just maction -> monitoringActionToList maction

-- This data holds the global state on monitoring. There is only one of them
-- maintained per Haskell process.
--
-- Not exported.
data MonitoringState = MonitoringState
  { monitoredActions :: M.Map ThreadId MonitoringAction
  , dirty            :: Bool }
  deriving ( Eq )

emptyMonitoringState :: MonitoringState
emptyMonitoringState = MonitoringState
  { monitoredActions = M.empty
  , dirty            = True }

-- This is the TVar that holds global monitoring state.
monitoringState :: TVar MonitoringState
monitoringState = unsafePerformIO $ newTVarIO emptyMonitoringState
{-# NOINLINE monitoringState #-}

-- Adds a new monitoring action to MonitoringState
addMonitoringAction :: ThreadId -> MonitoringAction -> MonitoringState -> MonitoringState
addMonitoringAction tid maction mstate = mstate
  { monitoredActions = M.alter (Just . alterer) tid (monitoredActions mstate)
  , dirty            = True }
 where
  alterer Nothing           = maction
  alterer (Just old_action) = addToEnd old_action

  addToEnd (MonitoringAction name_of_action_tvar Nothing) =
    MonitoringAction name_of_action_tvar $ Just maction
  addToEnd (MonitoringAction name_of_action_tvar (Just next_action)) =
    MonitoringAction name_of_action_tvar (Just $ addToEnd next_action)

getTopMonitorTVar :: ThreadId -> MonitoringState -> Maybe (TVar MonitorStatus)
getTopMonitorTVar tid mstate = do
  chain <- M.lookup tid (monitoredActions mstate)
  return $ go chain
 where
  go (MonitoringAction _ (Just next)) = go next
  go (MonitoringAction state _) = state

-- Removes topmost action from MonitoringState
deleteMonitoringAction :: ThreadId -> MonitoringState -> MonitoringState
deleteMonitoringAction tid mstate = mstate
  { monitoredActions = M.alter alterer tid (monitoredActions mstate)
  , dirty            = True }
 where
  alterer Nothing = error "deleteMonitoringAction: no monitoring action; impossible!"

  -- Removing the monitoring action from root deletes the whole key
  alterer (Just (MonitoringAction _ Nothing)) = Nothing
  -- Go deeper if the monitoring actions...go deeper and then delete at the
  -- outermost MonitoringAction.
  alterer (Just (MonitoringAction name_of_action_tvar next)) =
    Just $ MonitoringAction name_of_action_tvar $ alterer next

-- | Runs an action and make it subject to monitoring.
withMonitoring :: (MonadMask m, MonadIO m)
               => String
               -> m a
               -> m a
withMonitoring name_of_action action = mask $ \restore -> do
  -- Launch the monitor if it's not running yet
  liftIO launchMonitoringViewIfNotRunning

  -- Sometimes evaluating the name_of_action actually takes a while.
  -- E.g. I "ran" this huge computation to find out value X and now I'm
  -- running another thing with monitoring on X and my name_of_action is
  -- something like ("Evaluating score on cohort " <> show x) where 'x' is huge
  -- computation.
  --
  -- In other words, this function does not trust that name_of_action is a
  -- quickly computed thing.
  --
  -- In this case, we show placeholder on the name until it's evaluated, while
  -- running the action itself in main thread.
  name_of_action_thread_id_mvar <- liftIO newEmptyMVar
  name_of_action_name_mvar_tvar <- liftIO newEmptyMVar
  name_of_action_async <- liftIO $ asyncWithUnmask $ \unmask -> unmask $ do
    putMVar name_of_action_thread_id_mvar =<< myThreadId
    tvar <- takeMVar name_of_action_name_mvar_tvar
    name_of_action_evaluated <- evaluate $ force name_of_action
    atomically $ do
      modifyTVar tvar $ \monitor_status@(MonitorStatus _ _ evaluating) ->
        if evaluating
          then MonitorStatus name_of_action_evaluated Nothing False
          else monitor_status
      modifyTVar monitoringState $ \old -> old { dirty = True }

  -- Crash if evaluating the name crashes; I think that would be intuitively
  -- expected and someone might decide to rely on it crashing.
  liftIO $ link name_of_action_async
  name_of_action_thread_id <- liftIO $ takeMVar name_of_action_thread_id_mvar
  name_of_action_tvar <- liftIO $ newTVarIO $
    MonitorStatus ("<evaluating name in thread " ++ show name_of_action_thread_id ++ ">") Nothing True
  liftIO $ putMVar name_of_action_name_mvar_tvar name_of_action_tvar

  -- At this point, name_of_action_tvar contains a TVar to the name we should
  -- show in monitoring, and it'll update to the actual name when it has been
  -- fully evaluated.

  -- Add the monitoring to the global table
  liftIO $ do
    tid <- myThreadId
    atomically $ modifyTVar monitoringState $
      addMonitoringAction tid $ MonitoringAction name_of_action_tvar Nothing

  -- Now, run the action, and at the end remove monitoring
  finally (restore action) $ liftIO $ do
    tid <- myThreadId
    atomically $ modifyTVar monitoringState (deleteMonitoringAction tid)

-- | Updates the text on a monitor. Should be run inside `withMonitoring`. If
-- there is no active monitor for the current thread, does nothing.
updateMonitoring :: MonadIO m => String -> m ()
updateMonitoring new_name_of_action =
  updateMonitoringProgress' new_name_of_action Nothing

-- | Same as `updateMonitoring`, but also takes a progress argument. This is
-- visually displayed with a percentage on the monitoring.
updateMonitoringProgress :: MonadIO m => String -> Double -> m ()
updateMonitoringProgress txt progress =
  updateMonitoringProgress' txt (Just progress)

updateMonitoringProgress' :: MonadIO m => String -> Maybe Double -> m ()
updateMonitoringProgress' new_name_of_action progress = liftIO $ do
  tid <- myThreadId
  atomically $ do
    old_map <- readTVar monitoringState
    case getTopMonitorTVar tid old_map of
      Nothing -> return ()
      Just state_tvar -> do
        writeTVar state_tvar $ MonitorStatus new_name_of_action progress False
        writeTVar monitoringState $ old_map { dirty = True }

-- This function sets terminal into a mode that can be controlled by single
-- keypresses (which usually requires setting terminal parameters with ioctl()
-- on low-level).
--
-- Restores old settings when it exits.
withInteractiveTerminal :: IO a -> IO a
withInteractiveTerminal action = mask $ \restore -> do
  tos <- c_set_raw_terminal_mode
  finally (restore action) $ when (tos /= nullPtr) $
    c_restore_terminal_mode tos

-- This MVar is used to ensure only one instance of doMonitoringView code is
-- launched.
monitoringViewThread :: MVar ()
monitoringViewThread = unsafePerformIO $ newMVar ()
{-# NOINLINE monitoringViewThread #-}

launchMonitoringViewIfNotRunning :: IO ()
launchMonitoringViewIfNotRunning = mask_ $
  tryTakeMVar monitoringViewThread >>= \case
    Nothing -> return () -- already running
    Just () ->           -- not running yet
      void $ forkIOWithUnmask $ \unmask -> unmask doMonitoringView

-- This function implements the fancy view of things that are going on.
-- It is launched automatically at first call to `withMonitoring`.
doMonitoringView :: IO ()
doMonitoringView = withInteractiveTerminal $ do
  clearScreen
  showMonitor Nothing
 where
  showMonitor previous_monitor_state = do
    -- Get the current monitor state.
    -- We only continue from here if the state is different from last time
    -- (i.e. we have to update the screen for new new MonitoringState)
    new_monitor_state <- atomically $ do
      monitor_state <- readTVar monitoringState
      when (Just monitor_state == previous_monitor_state && not (dirty monitor_state))
        retry
      -- Clear dirty bit if it's there
      writeTVar monitoringState $ monitor_state { dirty = False }
      return monitor_state

    displayMonitor new_monitor_state

    threadDelay 100000
    showMonitor (Just new_monitor_state)

data DisplayState = DisplayState !Int !Int

-- Rounds nanoseconds into more readable fixed point value
round3 :: Int64 -> Fixed E3
round3 value = fromIntegral value / 1000000000

displayChar :: Char -> StateT DisplayState IO ()
displayChar ch = do
  DisplayState original_x original_y <- get
  liftIO $ putChar ch
  put $ DisplayState (original_x + charWidth ch) original_y

advanceLine :: StateT DisplayState IO ()
advanceLine = do
  DisplayState _ original_y <- get
  put $ DisplayState 0 (original_y+1)
  liftIO $ setCursorPosition (original_y+1) 0

moveCursor :: Int -> Int -> StateT DisplayState IO ()
moveCursor x y = do
  liftIO $ setCursorPosition y x
  put $ DisplayState x y

displayTruncatedString :: Int -> String -> StateT DisplayState IO ()
displayTruncatedString max_width str = do
  DisplayState original_x original_y <- get
  go str original_x
  moveCursor (original_x + max_width) original_y
 where
  go (char:rest) original_x = do
    DisplayState x _ <- get
    when (x + charWidth char <= original_x + max_width) $
      displayChar char >> go rest original_x
  go [] _ = return ()

displayProgressTruncatedString :: Int -> Int -> String -> StateT DisplayState IO ()
displayProgressTruncatedString num_progress_blocks max_width str = do
  DisplayState original_x original_y <- get
  liftIO $ setSGR [SetColor Background Vivid Black, SetColor Foreground Dull White]
  go str original_x 0
  liftIO $ setSGR [Reset]
  moveCursor (original_x + max_width) original_y
 where
  go (char:rest) original_x count = do
    DisplayState x _ <- get
    when (count == num_progress_blocks) $
      liftIO $ setSGR [Reset]
    when (x + charWidth char <= original_x + max_width) $
      displayChar char >> go rest original_x (count + charWidth char)
  go [] original_x count | count < num_progress_blocks = do
    DisplayState x _ <- get
    when (x + charWidth ' ' <= original_x + max_width) $
      displayChar ' ' >> go [] original_x (count + 1)
  go _ _ _ = return ()

-- Displays a list of things, in aligned columns.
displayAligned :: Int -> Int -> [(String, Maybe Double)] -> StateT DisplayState IO ()
displayAligned display_width things_per_row things =
  for_ grouped_things $ \group -> do
    liftIO $ clearLine
    for_ group $ \(text, maybe_progress) -> do
      case maybe_progress of
        Nothing -> do
          displayTruncatedString (width_per_item-1) text
          displayTruncatedString 1 "|"
        Just progress -> do
          let num_progress_blocks = round $ (fromIntegral $ width_per_item-1)*progress
          displayProgressTruncatedString num_progress_blocks (width_per_item-1) text
          displayTruncatedString 1 "|"
    advanceLine
 where
  width_per_item = max 1 $ display_width `div` things_per_row

  grouped_things :: [[(String, Maybe Double)]]
  grouped_things = go things
   where
    go [] = []
    go things =
      take things_per_row things:go (drop things_per_row things)

displayMonitor :: MonitoringState -> IO ()
displayMonitor monitor_state = flip evalStateT (DisplayState 0 0) $ do
  moveCursor 0 0

  -- Get display size. We'll know when we are clipping out by using this
  -- information.
  (display_width, display_height) <- getWindowSize
  liftIO $ clearScreen

  -- First display some misc information about the system, if the stats are
  -- available.
  rts_stats_enabled <- liftIO getRTSStatsEnabled
  when (rts_stats_enabled) $ do
    rts_stats <- liftIO getRTSStats
    displayAligned
                 display_width
                 4
                 (fmap (\x -> (x, Nothing)) $
                  ["GCs: "        ++ show (gcs rts_stats),
                   "Major GCs: "  ++ show (major_gcs rts_stats),
                   "Allocated: "  ++ show (allocated_bytes rts_stats),
                   "Live bytes: " ++ show (max_live_bytes rts_stats),
                   "GC time:    " ++ show (round3 $ gc_cpu_ns rts_stats),
                   "Total time: " ++ show (round3 $ cpu_ns rts_stats)])
    advanceLine

  -- Go through all active monitors and display on the screen. _nicely_
  for_ (M.assocs $ monitoredActions monitor_state) $ \(thread_id, chain) -> do
    -- Don't print anything if we are outside the displayable area
    DisplayState _cursor_x cursor_y <- get
    when (cursor_y >= 0 && cursor_y < display_height) $ do
      name_list <- monitoringActionToList chain
      displayAligned display_width
                     (length name_list)
                     (flip fmap name_list $ \(MonitorStatus name maybe_progress _) ->
                       case maybe_progress of
                         Nothing       -> (show thread_id <> ": " <> name, maybe_progress)
                         Just progress ->
                           let percentage' = show ((fromRational $ (toRational progress)*100 :: Fixed E3)) <> "%"
                               percentage_padded = replicate (8 - length percentage') ' ' <> percentage'
                            in (show thread_id <> ": [" <> percentage_padded <> "] " <> name, maybe_progress))

  liftIO $ hFlush stdout

