module Day16
  ( part1,
    part2,
  )
where

import Control.Monad (liftM, replicateM)
import Control.Monad.State (MonadState (get, put), State, evalState, execState, gets, mapState, runState)
import Debug.Trace (traceShow)
import Parsing (Parser, parseWith)
import Text.Parsec (char, many1, (<|>))
import Utils (BinaryNumber, binValue)
import qualified Utils (Bit (..))

-- Parsing

data HexDigit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | A
  | B
  | C
  | D
  | E
  | F

type HexNumber = [HexDigit]

hexDigit :: Parser HexDigit
hexDigit =
  (char '0' >> return Zero)
    <|> (char '1' >> return One)
    <|> (char '2' >> return Two)
    <|> (char '3' >> return Three)
    <|> (char '4' >> return Four)
    <|> (char '5' >> return Five)
    <|> (char '6' >> return Six)
    <|> (char '7' >> return Seven)
    <|> (char '8' >> return Eight)
    <|> (char '9' >> return Nine)
    <|> (char 'A' >> return A)
    <|> (char 'B' >> return B)
    <|> (char 'C' >> return C)
    <|> (char 'D' >> return D)
    <|> (char 'E' >> return E)
    <|> (char 'F' >> return F)

hexNumber :: Parser HexNumber
hexNumber = many1 hexDigit

parse :: String -> HexNumber
parse = parseWith hexNumber

-- Part 1

toBin :: HexNumber -> BinaryNumber
toBin = concatMap toBin'
  where
    toBin' :: HexDigit -> BinaryNumber
    toBin' Zero = [Utils.Zero, Utils.Zero, Utils.Zero, Utils.Zero]
    toBin' One = [Utils.Zero, Utils.Zero, Utils.Zero, Utils.One]
    toBin' Two = [Utils.Zero, Utils.Zero, Utils.One, Utils.Zero]
    toBin' Three = [Utils.Zero, Utils.Zero, Utils.One, Utils.One]
    toBin' Four = [Utils.Zero, Utils.One, Utils.Zero, Utils.Zero]
    toBin' Five = [Utils.Zero, Utils.One, Utils.Zero, Utils.One]
    toBin' Six = [Utils.Zero, Utils.One, Utils.One, Utils.Zero]
    toBin' Seven = [Utils.Zero, Utils.One, Utils.One, Utils.One]
    toBin' Eight = [Utils.One, Utils.Zero, Utils.Zero, Utils.Zero]
    toBin' Nine = [Utils.One, Utils.Zero, Utils.Zero, Utils.One]
    toBin' A = [Utils.One, Utils.Zero, Utils.One, Utils.Zero]
    toBin' B = [Utils.One, Utils.Zero, Utils.One, Utils.One]
    toBin' C = [Utils.One, Utils.One, Utils.Zero, Utils.Zero]
    toBin' D = [Utils.One, Utils.One, Utils.Zero, Utils.One]
    toBin' E = [Utils.One, Utils.One, Utils.One, Utils.Zero]
    toBin' F = [Utils.One, Utils.One, Utils.One, Utils.One]

data Packet = Packet
  { packetVersion :: Int,
    packetValue :: PacketValue
  }
  deriving (Show)

data OperatorTypecode
  = Sum
  | Product
  | Minimum
  | Maximum
  | GreaterThan
  | LessThan
  | EqualTo
  deriving (Show)

data PacketValue
  = Literal Int
  | Operator OperatorTypecode [Packet]
  deriving (Show)

type PacketParsingState = State BinaryNumber

takeBit :: PacketParsingState Utils.Bit
takeBit = head <$> takeBits 1

takeBits :: Int -> PacketParsingState BinaryNumber
takeBits n = do
  bn <- get
  let bits = take n bn
  put . drop n $ bn
  return bits

takeBitsAsInt :: Int -> PacketParsingState Int
takeBitsAsInt n = binValue <$> takeBits n

parseLiteralGroups :: PacketParsingState BinaryNumber
parseLiteralGroups = do
  continueCode <- takeBit
  group <- takeBits 4
  rest <- case continueCode of
    Utils.Zero -> return []
    Utils.One -> parseLiteralGroups
  return $ group ++ rest

parseLiteralValue :: PacketParsingState PacketValue
parseLiteralValue = Literal . binValue <$> parseLiteralGroups

parseNPackets :: Int -> PacketParsingState [Packet]
parseNPackets n = replicateM n parsePacket'

parsePacketsOfLength :: Int -> PacketParsingState [Packet]
parsePacketsOfLength 0 = return []
parsePacketsOfLength n = do
  initLen <- gets length
  p <- parsePacket'
  finalLen <- gets length
  let n' = n - (initLen - finalLen)
  rest <- parsePacketsOfLength n'
  return $ p : rest

parseOperatorPackets :: PacketParsingState [Packet]
parseOperatorPackets = do
  lengthTypeID <- takeBit
  case lengthTypeID of
    Utils.Zero -> takeBitsAsInt 15 >>= parsePacketsOfLength
    Utils.One -> takeBitsAsInt 11 >>= parseNPackets

parsePacket' :: PacketParsingState Packet
parsePacket' = do
  pv <- takeBitsAsInt 3
  pt <- takeBitsAsInt 3
  v <-
    if pt == 4
      then parseLiteralValue
      else Operator (decodeTypecode pt) <$> parseOperatorPackets
  return Packet {packetVersion = pv, packetValue = v}

parsePacket :: BinaryNumber -> Packet
parsePacket = evalState parsePacket'

versionSum :: Packet -> Int
versionSum Packet {packetVersion = v, packetValue = val} =
  case val of
    Literal _ -> v
    Operator _ ps -> v + (sum . map versionSum $ ps)

part1 :: String -> Int
part1 = versionSum . parsePacket . toBin . parse

-- Part 2

decodeTypecode :: Int -> OperatorTypecode
decodeTypecode 0 = Sum
decodeTypecode 1 = Product
decodeTypecode 2 = Minimum
decodeTypecode 3 = Maximum
decodeTypecode 5 = GreaterThan
decodeTypecode 6 = LessThan
decodeTypecode 7 = EqualTo
decodeTypecode _ = error "Invalid operator typecode"

evalPacket :: Packet -> Int
evalPacket Packet {packetVersion = _, packetValue = val} =
  case val of
    Literal n -> n
    Operator t ps ->
      let vs = map evalPacket ps
       in case t of
            Sum         -> sum vs
            Product     -> product vs
            Minimum     -> minimum vs
            Maximum     -> maximum vs
            GreaterThan -> if (vs !! 0) >  (vs !! 1) then 1 else 0
            LessThan    -> if (vs !! 0) <  (vs !! 1) then 1 else 0
            EqualTo     -> if (vs !! 0) == (vs !! 1) then 1 else 0

part2 :: String -> Int
part2 = evalPacket . parsePacket . toBin . parse
