{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Todo.DbTypes
    ( TodoDb

    , TPomodoro
    , PomodoroId
    , PomodoroMinutes

    , TTodoItem
    , TodoItemId
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
import           GHC.Generics

import           Control.Arrow
import           Control.Category (id)
import           Control.Lens
import qualified Data.Thyme as Time
import           Data.Thyme (Day, LocalTime, UTCTime, DiffTime)
import           Data.Thyme.Time.Core (Thyme, fromThyme)
import           Data.Int
import qualified Opaleye as O
import           Opaleye.SOT
import           Prelude hiding (id)

--------------------------------------------------------------------------------
-- THYME ORPHANS
thymeKol :: (Thyme time thyme, ToKol time pg) => thyme -> Kol pg
thymeKol = kol . fromThyme

-- instance (Thyme time thyme, ToKol time pg) => ToKol thyme pg where
--   kol = kol . fromThyme

instance ToKol UTCTime O.PGTimestamptz where kol = thymeKol

--------------------------------------------------------------------------------
data TodoDb

--------------------------------------------------------------------------------
newtype PomodoroId = PomodoroId { unPomodoroId :: UUID }

instance Wrapped PomodoroId where
  type Unwrapped PomodoroId = UUID
  _Wrapped' = iso unPomodoroId PomodoroId

instance ToKol PomodoroId O.PGUuid

--------------------------------------------------------------------------------
newtype PomodoroMinutes = PomodoroMinutes { unMins :: DiffTime }

instance Wrapped PomodoroMinutes where
  type Unwrapped PomodoroMinutes = DiffTime
  _Wrapped' = iso unMins PomodoroMinutes

instance ToKol PomodoroMinutes O.PGFloat8 where
  kol = kol . O.pgDouble . fromRational . (*60) . toRational . fromThyme . unMins

--------------------------------------------------------------------------------
data TPomodoro
instance Tabla TPomodoro where
  type Database TPomodoro = TodoDb
  type SchemaName TPomodoro = "todo"
  type TableName TPomodoro = "pomodoro"
  type Cols TPomodoro =
    [ 'Col "id" 'WD 'R O.PGUuid PomodoroId
    , 'Col "todo_id" 'W 'R O.PGUuid TodoItemId
    , 'Col "started_time" 'W 'R O.PGTimestamptz UTCTime
    , 'Col "ended_time" 'W 'RN O.PGTimestamptz UTCTime
    , 'Col "scheduled_length" 'W 'R O.PGFloat8 PomodoroMinutes
    ]


 --------------------------------------------------------------------------------
newtype TodoItemId = TodoItemId { unTodoItemId :: UUID }

instance Wrapped TodoItemId where
  type Unwrapped TodoItemId = UUID
  _Wrapped' = iso unTodoItemId TodoItemId

instance ToKol TodoItemId O.PGUuid

data TTodoItem
instance Tabla TTodoItem where
  type Database TTodoItem = TodoDb
  type SchemaName TTodoItem = "todo"
  type TableName TTodoItem = "todo-item"
  type Cols TTodoItem =
    [ 'Col "id" 'WD 'R O.PGUuid TodoItemId
    , 'Col "title" 'W 'R O.PGText Text
    , 'Col "created" 'WD 'R O.PGTimestamptz UTCTime
    , 'Col "due" 'WD 'RN O.PGTimestamptz UTCTime
    , 'Col "completed" 'WD 'RN O.PGTimestamptz UTCTime
    ]


instance Comparable TTodoItem "id" TPomodoro "todo_id"
