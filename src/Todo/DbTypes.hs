{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import qualified Data.Time as Time'
import qualified Data.Thyme as Time
import           Data.Thyme (Day, LocalTime, UTCTime, DiffTime)
import           Data.Thyme.Time.Core (Thyme, fromThyme, toThyme, secondsToDiffTime)
import           Data.Int
import Database.PostgreSQL.Simple.FromField
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

instance O.QueryRunnerColumnDefault O.PGTimestamptz UTCTime where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn

instance O.QueryRunnerColumnDefault O.PGUuid TodoItemId where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn

instance O.QueryRunnerColumnDefault O.PGUuid PomodoroId where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn

instance FromField UTCTime where
  fromField f mb = toThyme <$> (fromField :: FieldParser Time'.UTCTime) f mb

--------------------------------------------------------------------------------
data TodoDb

--------------------------------------------------------------------------------
newtype PomodoroId = PomodoroId { unPomodoroId :: UUID } deriving (FromField)

instance Wrapped PomodoroId where
  type Unwrapped PomodoroId = UUID
  _Wrapped' = iso unPomodoroId PomodoroId

-- instance (FromField (Unwrapped a), Wrapped a) => FromField a where
--   fromField f mb = view _Unwrapped' <$> fromField f mb

instance ToKol PomodoroId O.PGUuid

instance FromField PomodoroMinutes where
  fromField f mb = PomodoroMinutes . convert <$> fromField f mb
    where convert = secondsToDiffTime . (*60)

instance O.QueryRunnerColumnDefault O.PGFloat8 PomodoroMinutes where
  queryRunnerColumnDefault = O.fieldQueryRunnerColumn


--------------------------------------------------------------------------------
newtype PomodoroMinutes = PomodoroMinutes { unMins :: DiffTime } deriving Show

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
newtype TodoItemId = TodoItemId { unTodoItemId :: UUID } deriving (FromField)

instance Wrapped TodoItemId where
  type Unwrapped TodoItemId = UUID
  _Wrapped' = iso unTodoItemId TodoItemId

instance ToKol TodoItemId O.PGUuid

data TTodoItem
instance Tabla TTodoItem where
  type Database TTodoItem = TodoDb
  type SchemaName TTodoItem = "todo"
  type TableName TTodoItem = "todo_item"
  type Cols TTodoItem =
    [ 'Col "id" 'WD 'R O.PGUuid TodoItemId
    , 'Col "title" 'W 'R O.PGText Text
    , 'Col "created" 'WD 'R O.PGTimestamptz UTCTime
    , 'Col "due" 'WD 'RN O.PGTimestamptz UTCTime
    , 'Col "completed" 'WD 'RN O.PGTimestamptz UTCTime
    ]


instance Comparable TTodoItem "id" TPomodoro "todo_id"
