{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
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
  ) where
-- import           Control.Applicative
import           Control.Exception          (Exception (..))
import           Control.Lens
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                 as Aeson
import Data.ByteString (ByteString)
import           Data.Data                  (Data, Typeable)
import           Data.Text                  (Text)
--import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.UUID                  (UUID)
import qualified Data.UUID                  as UUID
-- import qualified Data.UUID.V4               as UUID
import           GHC.Generics              (Generic)

import           Control.Arrow
import           Control.Category (id)
import           Control.Lens
import qualified Data.Thyme as Time
import           Data.Thyme (Day, LocalTime, UTCTime, DiffTime)
import           Data.Int
import qualified Opaleye as O
import           Opaleye.SOT
import           Prelude hiding (id)

import Todo.DbTypes

data Pomodoro = Pomodoro {
    _started :: !UTCTime
  , _ended :: !(Maybe UTCTime)
  , _scheduledLength :: !PomodoroMinutes
   } deriving (Generic)

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
   } deriving (Generic)

makeLenses ''TodoItem


--------------------------------------------------------------------------------
-- QUERIES
pomodorosForTodoQ :: O.QueryArr TodoItemId (PgR TPomodoro)
pomodorosForTodoQ = proc pId -> do
  pomodoro <- queryTabla' -< ()
  restrict -< eq (pomodoro ^. col(C::C "todo_id")) (kol pId)
  returnA -< pomodoro

-- todosQ :: O.QueryArr () (PgR (TTodoItem, [TPomodoro]))
todosQ = proc () -> do
  todo <- queryTabla' -< ()
  pomodoros <- pomodorosForTodoQ -< (todo ^. col (C::C "id"))
  returnA -< (todo, pomodoros)
