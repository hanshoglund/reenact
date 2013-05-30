
module Control.Reactive.Midi (
        module Codec.Midi,
        
        -- * Basic types
        Midi.MidiTime,
        Midi.MidiMessage,

        -- * Sources and destinations
        MidiSource,
        MidiDestination,
        midiSources,
        midiDestinations,
        findSource,
        findDestination,
        
        -- * Sending and receiving
        midiIn,
        midiIn',
        midiOut,
  ) where

import Data.Monoid  
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import System.IO.Unsafe (unsafePerformIO)

import Control.Reactive
import Control.Reactive.Util

import Codec.Midi hiding (Time, Track)
import qualified System.Midi as Midi

type MidiSource      = Midi.Source
type MidiDestination = Midi.Destination

midiSources :: Reactive [MidiSource]
midiSources = eventToReactive 
        (pollE $ threadDelay 1 >> Midi.sources >>= return . Just)

midiDestinations :: Reactive [MidiDestination]
midiDestinations = eventToReactive 
        (pollE $ threadDelay 1 >> Midi.destinations >>= return . Just)

findSource :: Reactive String -> Reactive (Maybe MidiSource)
findSource nm = g <$> nm <*> midiSources
    where
        g = (\n -> listToMaybe . filter (\d -> isSubstringOfNormalized n $ unsafePerformIO (Midi.name d)))

findDestination :: Reactive String -> Reactive (Maybe MidiDestination)
findDestination nm = g <$> nm <*> midiDestinations
    where
        g = (\n -> listToMaybe . filter (\d -> isSubstringOfNormalized n $ unsafePerformIO (Midi.name d)))


midiIn :: MidiSource -> Event Midi.MidiMessage
midiIn dev = snd <$> midiIn' dev

midiIn' :: MidiSource -> Event (Midi.MidiTime, Midi.MidiMessage)
midiIn' dev = unsafePerformIO $ do
    (k, e) <- newSource
    str <- Midi.openSource dev (Just $ curry k)
    Midi.start str
    return e

midiOut :: MidiDestination -> Event Midi.MidiMessage -> Event Midi.MidiMessage
midiOut dest = putE $ \msg -> do
    Midi.send dest' msg
    where
        dest' = unsafePerformIO $ do
            -- putStrLn "Midi.openDestination"
            Midi.openDestination dest








---------

eventToReactive :: Event a -> Reactive a
eventToReactive = stepper (error "eventToReactive: ")

