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
import Data.HList.Record
import           Data.Text                  (Text)
--import qualified Data.Text                  as T
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

type PkType t pk = Col_HsRType (Col_ByName t pk)

-- class 
class (UnHsR t a, HasField (TC t pk) (Record (Cols_HsR t)) (PkType t pk)) => ReadKV t pk a where

-- readKV :: forall t a pk. (UnHsR t a, HasField (TC t pk) (Record (Cols_HsR t)) (PkType t pk)) => HsR t -> Either SomeException (PkType t pk, a)
readKV row = do
  a <- unHsR' row
  return (row ^. cola (C :: C pk), a)

data Pomodoro = Pomodoro {
    _started :: !UTCTime
  , _ended :: !(Maybe UTCTime)
  , _scheduledLength :: !PomodoroMinutes
   } deriving (Generic, Show)

makeLenses ''Pomodoro

instance UnHsR TPomodoro Pomodoro where
  unHsR' r = return
           $ Pomodoro (r ^. cola (C :: C "started_time"))
                      (r ^. cola (C :: C "ended_time"))
                      (r ^. cola (C :: C "scheduled_length"))


data TodoItem = TodoItem {
   _title :: !Text
  , _created :: !UTCTime
  , _due :: !(Maybe UTCTime)
  , _completed :: !(Maybe UTCTime)
  , _pomodoros :: ![Pomodoro]
   } deriving (Generic, Show)

makeLenses ''TodoItem

instance UnHsR TTodoItem TodoItem where
  unHsR' r  = return
            $ TodoItem (r ^. cola (C::C "title"))
                       (r ^. cola (C::C "created"))
                       (r ^. cola (C::C "completed"))
                       (r ^. cola (C::C "due"))
                       []
--------------------------------------------------------------------------------
-- QUERIES
pomodorosForTodoQ :: O.QueryArr TodoItemId (PgR TPomodoro)
pomodorosForTodoQ = proc pId -> do
  pomodoro <- queryTabla' -< ()
  restrict -< eq (pomodoro ^. col(C::C "todo_id")) (kol pId)
  returnA -< pomodoro

todosQ :: O.Query (PgR TTodoItem, PgR TPomodoro)
todosQ = proc () -> do
  todo <- queryTabla' -< ()
  pomodoro <- queryTabla' -< ()
  restrict -< eq
    (todo ^. col (C::C "id"))
    (pomodoro ^. col (C::C "todo_id"))
  id -< (todo, pomodoro)

todoByIds :: [TodoItemId] -> O.Query (PgR TTodoItem)
todoByIds tIds = proc () -> do
  t <- queryTabla' -< ()
  restrict -<  in_ (kol tIds) (t ^. col (C::C "id"))
  id -< t

getTodos :: (MonadIO m, MonadThrow m, Allow 'Fetch ps) => DBMonad ps m [TodoItem]
getTodos = Q.queryDb (queryTabla (T :: T TTodoItem))

todosByIDs
  :: (MonadIO m, MonadThrow m, Allow 'Fetch ps)
  => [TodoItemId]
  -> DBMonad ps m [TodoItem]
todosByIDs todoIds =
  runQuerying $
    withQuery qTodos $ \todos ->
      withQuery qPomodoros $ \pomodoros ->
        for todoIds $ \todoId ->
          let todo = Q.ask todos todoId
              pomodoros' = Q.ask pomodoros todoId
          in todo & pomodoros .~ pomodoros'
  where
    qTodos = extractId . queryDb <$> todoByIds
    qPomodoros = _
    extractId l a = _ 

pomodorosByTodoId
  :: (MonadIO m, MonadThrow m, Allow 'Fetch ps)
  => TodoItemId
  -> DBMonad ps m [Pomodoro]
pomodorosByTodoId todoId = Q.queryDb q
  where
    q = proc () -> do
      pomodoro <- queryTabla (T :: T TPomodoro) -< ()
      restrict -< eq (pomodoro ^. col(C::C "todo_id")) (kol todoId)
      returnA -< pomodoro
--------------------------------------------------------------------------------
-- QueryDB
main :: IO ()
main = run
  where
    run :: (MonadIO m, MonadThrow m, MonadMask m) => m ()
    run = do
      con <- connect' "dbname=todo"
      let m = Q.queryDb q -- :: m [TodoItem]
      (res :: [TodoItem]) <- runMonadQuery con m
      liftIO $ print res
      where
        q :: O.Query (PgR TTodoItem)
        q = queryTabla'



