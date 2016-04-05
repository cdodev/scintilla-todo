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
import qualified Data.Time as Time
import           Data.Time (Day, LocalTime, UTCTime, DiffTime)
import           Data.Int
import qualified Opaleye as O
import           Opaleye.SOT
import           Prelude hiding (id)

data TodoDb

newtype PomodoroId = PomodoroId { unPomodoroId :: UUID }
instance Wrapped PomodoroId where { type Unwrapped PomodoroId = UUID; _Wrapped' = iso unPomodoroId PomodoroId }
instance ToKol PomodoroId O.PGUuid


data TPomodoro
instance Tabla TPomodoro where
  type Database TPomodoro = TodoDb
  type SchemaName TPomodoro = "todo"
  type TableName TPomodoro = "pomodoro"
  type Cols TPomodoro =
    [ 'Col "pomodoro_id" 'WD 'R O.PGUuid PomodoroId
    , 'Col "started_time" 'W 'R O.PGTimestamptz UTCTime
    , 'Col "ended_time" 'W 'RN O.PGTimestamptz UTCTime
    , 'Col "scheduled_length" 'W 'R O.PGFloat8 DiffTime
    ]

data Pomodoro = Pomodoro {
    _started :: !UTCTime
  , _ended :: !UTCTime
  , _scheduledLength :: !DiffTime
   } deriving (Generic)

makeLenses ''Pomodoro

data TodoItem = TodoItem {
    _title :: Text
  , _created :: UTCTime
  , _due :: Maybe (UTCTime)
  , _pomodoros :: [Pomodoro]
   } deriving (Generic)

makeLenses ''TodoItem


