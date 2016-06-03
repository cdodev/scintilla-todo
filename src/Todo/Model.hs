{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Todo.Model (
    PomodoroId
  , Pomodoro
  , started, ended, scheduledLength
  , TodoItem
  , title, created, due, completed, pomodoros

  , main
  ) where
-- import           Control.Applicative
import           Control.Exception          (Exception (..))
import Data.Traversable
import           Control.Lens
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                 as Aeson
import Data.ByteString (ByteString)
import           Data.Data                  (Data, Typeable)
import qualified Data.Foldable as F
import Data.HList.HList
import Data.HList.Record
import Data.Maybe
import           Data.Text                  (Text)
--import qualified Data.Text                  as T
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding         as T
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
-- import qualified Data.UUID.V4               as UUID
import           GHC.Generics              (Generic)

import           Control.Arrow
import           Control.Category (id)
import qualified Data.Thyme as Time
import           Data.Thyme (Day, LocalTime, UTCTime, DiffTime)
import           Data.Int
import qualified Opaleye as O
import           Opaleye.SOT
import           Prelude hiding (id)

import Todo.DbTypes
import Scintilla.Query as Q hiding (main)

import Database.PostgreSQL.Simple
import Control.Monad.Reader
import           Control.Monad.Catch             (MonadThrow, MonadCatch, MonadMask, SomeException,
                                                  throwM)



data Pomodoro = Pomodoro {
    _started :: !UTCTime
  , _ended :: !(Maybe UTCTime)
  , _scheduledLength :: !PomodoroMinutes
   } deriving (Generic, Show)

makeLenses ''Pomodoro

instance ParseHsR TPomodoro Pomodoro where
  parseHsR r = return
           $ Pomodoro (r ^. col (C :: C "started_time"))
                      (r ^. col (C :: C "ended_time"))
                      (r ^. col (C :: C "scheduled_length"))


--------------------------------------------------------------------------------
data TodoItem = TodoItem {
   _title :: !Text
  , _created :: !UTCTime
  , _due :: !(Maybe UTCTime)
  , _completed :: !(Maybe UTCTime)
  , _pomodoros :: ![Pomodoro]
   } deriving (Generic, Show)

makeLenses ''TodoItem

instance ParseHsR TTodoItem TodoItem where
  parseHsR r  = return
            $ TodoItem (r ^. col (C::C "title"))
                       (r ^. col (C::C "created"))
                       (r ^. col (C::C "completed"))
                       (r ^. col (C::C "due"))
                       []

data CreateTodo = CreateTodo {
    _tiNewTitle :: !Text
  , _tiNewDue :: !(Maybe UTCTime)
  }

makeLenses ''CreateTodo

instance ToHsI TTodoItem CreateTodo where
  toHsI (CreateTodo t mDue) = mkHsI (T :: T TTodoItem) $ \set_ -> hBuild
    (set_ (C :: C "id") WDef)
    (set_ (C :: C "title") t)
    (set_ (C :: C "created") WDef)
    (set_ (C :: C "due") (WVal mDue))
    (set_ (C :: C "completed") WDef)

--------------------------------------------------------------------------------
-- QUERIES
todosByIds
  :: forall m ps. (MonadIO m, MonadThrow m, Allow 'Fetch ps)
  => [TodoItemId]
  -> DBMonad ps m [TodoItem]
todosByIds todoIds = catMaybes <$> q
  where
    q = runQuerying $
        withQuery qTodos $ \todos ->
          withQuery pomodorosByTodoIds $ \pomodoroLookup ->
            for todoIds $ \todoId ->
              let todo = Q.ask todos todoId
                  pomodoros' = Q.ask pomodoroLookup todoId
              in set (_Just.pomodoros) <$> (fromMaybe [] <$> pomodoros') <*> todo
    qTodos :: [TodoItemId] -> DBMonad ps m [(TodoItemId, TodoItem)]
    qTodos tIds = Q.queryDb tiQ
      where
        tiQ :: O.Query (PgR TTodoItem)
        tiQ = proc () -> do
          t <- queryTabla T -< ()
          restrict -< (t ^. col (C::C "id")) `member` (kol <$> tIds)
          id -< t

pomodorosByTodoIds
  :: (MonadIO m, MonadThrow m, Allow 'Fetch ps)
  => [TodoItemId]
  -> DBMonad ps m [(TodoItemId, [Pomodoro])]
pomodorosByTodoIds todoIds = group <$> Q.queryDb q
  where
    group :: [(TodoItemId, Pomodoro)] -> [(TodoItemId, [Pomodoro])]
    group = Map.toList . F.foldr f (mempty :: Map.Map TodoItemId [Pomodoro])
    f pom = Map.insertWith (++) (pom ^. _1) [pom ^. _2]
    q = proc () -> do
      pomodoro <- queryTabla (T :: T TPomodoro) -< ()
      let tId = pomodoro ^. col(C::C "todo_id")
      restrict -< tId `member` (kol <$> todoIds)
      returnA -< pomodoro

--------------------------------------------------------------------------------
-- WRITE
saveNewTodo
  :: ( MonadIO m, MonadThrow m
     , Allow '[ 'Fetch, 'Insert ] ps)
  => Text -> Maybe UTCTime -> DBMonad ps m TodoItemId
saveNewTodo t mDue = do
  res <- insertDB (T :: T TTodoItem) [CreateTodo t mDue]
  case res of
    [] -> chuck "No Rows Returned"
    [r] -> return r
    _ -> chuck "More than one row"
  where
    chuck = throwM . toException . MyErr

data MyErr = MyErr String deriving Show

instance Exception MyErr
--------------------------------------------------------------------------------
-- QueryDB
main :: IO ()
main = run
  where
    run :: (MonadIO m, MonadThrow m, MonadMask m) => m ()
    run = do
      con <- connect' "dbname=todo"
      -- let m = Q.queryDb q -- :: m [TodoItem]
      -- TODO: fix types embed read in write.
      (res :: [TodoItem]) <- runMonadWrite con $ do
         newID <- saveNewTodo "Test TODO" Nothing
         todosByIds tids
      liftIO $ print res
    tids = catMaybes [UUID.fromString "860d491e-0b6c-11e6-a934-6b21654c78a7" ^? _Just._Unwrapped']


