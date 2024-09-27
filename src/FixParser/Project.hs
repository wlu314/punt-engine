{-# OPTIONS_GHC -Wno-orphans #-}

module FixParser.Project where

import Clash.Prelude
import Data.Char (isDigit, ord)
import Data.Maybe (fromMaybe)

createDomain vSystem {vName = "Dom50", vPeriod = hzToPeriod 50e6}

-- Clash looks for [`topEntity`] when building rtl
-- see pin assignments below
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Byte -> -- Input stream
  Signal System (Maybe ParserOutput) -- Output stream
topEntity clk rst en inputStream = parser inputStream
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "FixParserTop",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "DIN"
          ],
        t_output = PortName "DOUT"
      }
  )
  #-}
{-# OPAQUE topEntity #-}

data ParserState
  = WaitingForStart
  | ReadingTag
  | ReadingValue
  | ValidatingChecksum
  | MessageComplete
  | ErrorState
  deriving (Generic, NFDataX)

data ErrorState
  = InvalidTag
  | InvalidValue
  | ChecksumMismatch
  | UnexpectedByte
  deriving (Generic, NFDataX, Show)

data ParserOutput
  = ParsedMessage MessageData
  | ParserError ErrorType
  deriving (Generic, NFDataX, Show)

-- we need discrete values. I'm unsure if this is the right one
type MaxValueLength = 32

-- we need discrete values. I'm unsure if this is the right one
type MaxTags = 64

-- FIX 4.2 has 446 tags (1), as well as user-defined tags in the 5000-10000 range (2). hence use an unsigned 16
-- 1: https://www.onixs.biz/fix-dictionary/4.2/fields_by_tag.html
-- 2: https://www.fixtrading.org/standards/user-defined-fields/
type Tag = Unsigned 16

type Byte = Unsigned 8

data Value = Value (Vec MaxValueLength Byte)
  deriving (Generic, NFDataX)

data MessageData = MessageData
  { tags :: Vec MaxTags (Tag, Value)
  }
  deriving (Generic, NFDataX, Show)

type InputStream = Signal System Byte

type OutputStream = Signal System (Maybe ParserOutput)

parser :: (HiddenClockResetEnable dom) => InputStream -> OutputStream
parser input = mealy parserT initialState input
  where
    initialState = (WaitingForStart, initialParserState)

type ParserInternalState = (ParserState, ParserData)

data ParserData = ParserData
  { currentTag :: Tag,
    currentValue :: Vec MaxValueLength Byte,
    valueIndex :: Index MaxValueLength,
    tagsAccum :: Vec MaxTags (Tag, Value),
    tagIndex :: Index MaxTags,
    checksum :: Unsigned 8, -- FIX checksums are the sum of all message bytes mod 256
    calcCheckSum :: Unsigned 8
  }
  deriving (Generic, NFDataX)

-- this mealy machine is the heart of the parser
-- note that parserT denotes parserTransition it's an implementation of our state machine
parserT ::
  (ParserState, ParserData) ->
  Byte ->
  ((ParserState, ParserData), Maybe ParserOutput)
parserT (state, pdata@ParserData {..}) byte = case state of
  -- our default/reset state. we loop here until we get our start byte
  WaitingForStart ->
    if byte == asciiSOH
      then ((WaitingForStart, pdata), Nothing) -- ignore leading SOH
      else
        -- check for start of message (8=value)
        if byte == asciiOf '8'
          then
            let pdata' =
                  pdata
                    { currentTag = 8, -- remember, we're storing tag=value pairs. 8=... is no different
                      currentValue = repeat 0,
                      valueIndex = 0,
                      calcChecksum = 0
                    }
             in ((ReadingValue, pdata'), Nothing)
          else
            ((WaitingForStart, pdata), Nothing)
  -- we're reading tag in tag=value. eat chars until the '=' sign
  ReadingTag ->
    if byte == asciiOf '='
      then
        -- tag is exhausted, time for value
        let pdata' = pdata {valueIndex = 0}
         in ((ReadingValue, pdata'), Nothing)
      else
        if isDigitByte byte
          then
            -- accumulate tag digits
            let newTag = currentTag * 10 + (byte - asciiOf '0')
                pdata' = pdata {currentTag = newTag}
             in ((ReadingTag, pdata'), Nothing)
          else
            ((ErrorState, pdata), Just (ParserError InvalidTag))
  -- eat value until a byte indicating its end
  ReadingValue ->
    if byte == asciiSOH
      then
        -- stop eating and store tag-value pair
        let value = Value (take valueIndex currentValue)
            tagsAccum' = replace tagIndex (currentTag, value) tagsAccum
            pdata' =
              pdata
                { tagsAccum = tagsAccum',
                  tagIndex = tagIndex + 1,
                  currentTag = 0,
                  currentValue = repeat 0,
                  valueIndex = 0
                }
            nextState =
              if currentTag == 10 -- check if tag is checksum
                then ValidatingChecksum
                else ReadingTag
         in ((nextState, pdata'), Nothing)
      else
        -- accumulate value bytes
        if valueIndex < maxBound
          then
            let currentValue' = replace valueIndex byte currentValue
                pdata' =
                  pdata
                    { currentValue = currentValue',
                      valueIndex = valueIndex + 1
                    }
                -- update calculated checksum
                calcChecksum' = calcChecksum + byte
                pdata'' = pdata' {calcChecksum = calcChecksum'}
             in ((ReadingValue, pdata''), Nothing)
          else
            -- error: value too long
            ((ErrorState, pdata), Just (ParserError InvalidValue))
  ValidatingChecksum ->
    -- parse checksum value (assumed to be last)
    let receivedChecksum = parseChecksum currentValue valueIndex
     in if receivedChecksum == calcChecksum
          then
            let message = MessageData {tags = take tagIndex tagsAccum}
             in ((MessageComplete, pdata), Just (ParsedMessage message))
          else
            ((ErrorState, pdata), Just (ParserError ChecksumMismatch))
  MessageComplete ->
    -- peset parser state for next message
    let pdata' =
          pdata
            { currentTag = 0,
              currentValue = repeat 0,
              valueIndex = 0,
              tagsAccum = repeat (0, Value (repeat 0)),
              tagIndex = 0,
              checksum = 0,
              calcChecksum = 0
            }
     in ((WaitingForStart, pdata'), Nothing)
  ErrorState ->
    -- stay in ErrorState until reset (auto-recovery later)
    ((ErrorState, pdata), Nothing)
