{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wall #-}

module Main where

import           Control.Applicative
import           Data.Foldable (find)
import           Data.Kind (Type)
import           Data.List (stripPrefix)
import qualified Data.List.NonEmpty          as N
import           Data.Maybe
import           GHC.Read (readPrec)
import qualified Text.ParserCombinators.ReadPrec       as RP

data GreekAlphabet = A | Aa | B | G | D | E | Z | H | Th | I | Ii | K | L | M | N | X | O | P | R | S | T | U | Uu | Ph | Kh | Ps | W
  deriving (Eq,Ord,Enum)

greekCharPairs :: [(GreekAlphabet, Char)]
greekCharPairs = [ (A , 'α')
                 , (Aa, 'ᾱ')
                 , (B , 'β')
                 , (G , 'γ')
                 , (D , 'δ')
                 , (E , 'ε')
                 , (Z , 'ζ')
                 , (H , 'η')
                 , (Th, 'θ')
                 , (I , 'ι')
                 , (Ii, 'ῑ')
                 , (K , 'κ')
                 , (L , 'λ')
                 , (M , 'μ')
                 , (N , 'ν')
                 , (X , 'ξ')
                 , (O , 'ο')
                 , (P , 'π')
                 , (R , 'ρ')
                 , (S , 'σ')
                 , (T , 'τ')
                 , (U , 'υ')
                 , (Uu, 'ῡ')
                 , (Ph, 'φ')
                 , (Kh, 'χ')
                 , (Ps, 'ψ')
                 , (W , 'ω')
                 ]

greekChar :: GreekAlphabet -> Char
greekChar g = snd $ fromJust $ find ((g ==) . fst) greekCharPairs

instance Show GreekAlphabet where
  show = pure . greekChar

instance Read GreekAlphabet where
  readPrec = RP.get >>= \c ->
    maybe RP.pfail (return . fst) $ find ((c ==) . snd) greekCharPairs

iotaSubscripts :: [([GreekAlphabet], Char)]
iotaSubscripts = [ ([Aa, I], 'ᾳ')
                 , ([ H, I], 'ῃ')
                 , ([ W, I], 'ῳ')
                 ]

-- >>> subscriptIota [W, I, D, H, I]
-- "ῳδῃ"
subscriptIota :: [GreekAlphabet] -> String
subscriptIota []        = []
subscriptIota ls@(g:gs) =
  fromMaybe (greekChar g : subscriptIota gs) $ asum prefixSubstitutions
  where
    prefixSubstitutions :: [Maybe String]
    prefixSubstitutions = (\(pat, c) -> ((c:) . subscriptIota) <$> stripPrefix pat ls)
                       <$> iotaSubscripts

-- >>> read "ζῳον" :: GreekWord
-- GreekWord (N N.:| [O, I, W, Z])
newtype GreekWord = GreekWord { unGreekWord :: N.NonEmpty GreekAlphabet } deriving Eq

toGreekWord :: [GreekAlphabet] -> GreekWord
toGreekWord = GreekWord . N.reverse . N.fromList

fromGreekWord :: GreekWord -> [GreekAlphabet]
fromGreekWord = N.toList . N.reverse . unGreekWord

instance Ord GreekWord where
  compare w1 w2 = compare (fromGreekWord w1) (fromGreekWord w2)

instance Show GreekWord where
  show (GreekWord (S N.:| ls)) = subscriptIota (reverse ls) ++ "ς"
  show w                       = subscriptIota $ fromGreekWord w

eof :: RP.ReadPrec ()
eof = RP.look >>= \case
  [] -> return ()
  _  -> RP.pfail

instance Read GreekWord where
  readPrec = toGreekWord <$> do
    h  <- letter
    ls <- letters
    return $ h ++ ls
    where
      letter :: RP.ReadPrec [GreekAlphabet]
      letter = fmap pure readPrec <|> do
        c <- RP.get
        maybe RP.pfail (return . fst) $ find ((c ==) . snd) iotaSubscripts
      letters :: RP.ReadPrec [GreekAlphabet]
      letters = do
          l  <- letter
          ls <- letters
          return $ l ++ ls
        <|> do
          RP.get >>= \case
            'ς' -> return [S]
            _   -> RP.pfail
        <|> do
          eof
          return []

vowels :: [GreekAlphabet]
vowels = [A, Aa, E, H, I, Ii, O, U, Uu, W]

isVowel :: GreekAlphabet -> Bool
isVowel x | x `elem` vowels = True
          | otherwise       = False

isConsonant :: GreekAlphabet -> Bool
isConsonant = not . isVowel

longVowels :: [GreekAlphabet]
longVowels = [Aa, H, Ii, Uu, W]

-- | 二重母音
diphthongs :: [[GreekAlphabet]]
diphthongs = [ [A , I]
             , [E , I]
             , [O , I]
             , [U , I]
             , [Aa, I]
             , [H , I]
             , [W , I]
             , [A , U]
             , [E , U]
             , [O , U]
             , [H , U]
             ]

stripLongVowelOrDiphthongPrefix :: [GreekAlphabet]
                                -> Maybe ([GreekAlphabet], [GreekAlphabet])
stripLongVowelOrDiphthongPrefix ls =
  asum $ (\pat -> (pat,) <$> stripPrefix pat ls) <$> diphthongs ++ fmap pure longVowels

-- | 母音融合
contract :: GreekAlphabet -> [GreekAlphabet] -> N.NonEmpty GreekAlphabet
contract A (E:ls)   = Aa N.:| ls
contract A (H:ls)   = Aa N.:| ls
contract A (O:U:ls) =  W N.:| ls
contract A (O:ls)   =  W N.:| ls
contract E ls | isJust $ stripLongVowelOrDiphthongPrefix ls = N.fromList ls -- safe
contract E (E:ls)   =  E N.:| I : ls
contract E (O:ls)   =  O N.:| U : ls
contract O (E:I:ls) =  O N.:| I : ls
contract O (O:I:ls) =  O N.:| I : ls
contract O (H:I:ls) =  O N.:| I : ls
contract O (E:ls)   =  O N.:| U : ls
contract O (O:U:ls) =  O N.:| U : ls
contract O (O:ls)   =  O N.:| U : ls
contract O (H:ls)   =  W N.:| ls
contract O (W:ls)   =  W N.:| ls
contract x ls       =  x N.:| ls

-- | 唇音
labials :: [GreekAlphabet]
labials = [P, B, Ph]

-- | 口蓋音
palatals :: [GreekAlphabet]
palatals = [K, G, Kh]

-- | 歯音
dentals :: [GreekAlphabet]
dentals = [T, D, Th]

-- | 流音
liquids :: [GreekAlphabet]
liquids = [L, M, N, R]

data T = Primary
       | Secondary
       deriving (Show,Eq)

data Time :: T -> Type where
  FutureTime  :: Time 'Primary
  PresentTime :: Time 'Primary
  PastTime    :: Time 'Secondary

deriving instance Show (Time t)
deriving instance Eq   (Time t)

data Tense :: forall t. Time t -> Type where
  Future        :: Tense 'FutureTime
  FuturePerfect :: Tense 'FutureTime
  Present       :: Tense 'PresentTime
  Perfect       :: Tense 'PresentTime
  Aorist        :: Tense 'PastTime
  Imperfect     :: Tense 'PastTime
  Pluperfect    :: Tense 'PastTime

deriving instance Show (Tense t)
deriving instance Eq   (Tense t)

data Aspect = PerfectiveAspect   -- 完結相、一回限り、点的
            | ImperfectiveAspect -- 非完結相、継続、進行、反復、線的
            | PerfectAspect      -- 完了相
            deriving (Show,Eq)

aspect :: Tense t -> [Aspect]
aspect Future        = [PerfectiveAspect, ImperfectiveAspect]
aspect FuturePerfect = [PerfectAspect]
aspect Present       = [PerfectiveAspect, ImperfectiveAspect]
aspect Perfect       = [PerfectAspect]
aspect Aorist        = [PerfectiveAspect]
aspect Imperfect     = [ImperfectiveAspect]
aspect Pluperfect    = [PerfectAspect]

data Mood = Indicative
          | Subjunctive
          | Optative
          | Imperative
          deriving (Show,Eq)

data Voice = Active
           | Middle
           | Passive
           deriving (Show,Eq)

data Person = First
            | Second
            | Third
            deriving (Show,Eq,Ord,Enum,Bounded)

data Number = Singular
            | Plural
            deriving (Show,Eq,Ord,Enum,Bounded)

-- | 教える
paideu :: GreekWord
paideu = read "παιδευ"

-- | 愛する
phile :: GreekWord
phile = read "φιλε"

-- | 送る
pemp :: GreekWord
pemp = read "πεμπ"

-- | 書く
graph :: GreekWord
graph = read "γραφ"

-- | 導く
ag :: GreekWord
ag = read "αγ"

-- | 説得する
peith :: GreekWord
peith = read "πειθ"

-- | 聞く
akou :: GreekWord
akou = read "ακου"

addEnding :: GreekWord -> [GreekAlphabet] -> GreekWord
addEnding stem ending = GreekWord
  $ (\(stemEnding N.:| ls) -> N.reverse (contract stemEnding ending) `N.appendList` ls)
  $ unGreekWord stem

type PersonalEnding  = Number -> Person -> [GreekAlphabet]
type StemConstructor = GreekWord -> GreekWord

conjugationTable :: (StemConstructor, PersonalEnding) -> GreekWord -> [GreekWord]
conjugationTable (f, ending) stem =
  [ f stem `addEnding` ending n p | n <- [minBound..maxBound], p <- [minBound..maxBound] ]

thematicPrimary :: PersonalEnding
thematicPrimary Singular First  = []
thematicPrimary Singular Second = [S]
thematicPrimary Singular Third  = [S, I]
thematicPrimary Plural   First  = [M, E, N]
thematicPrimary Plural   Second = [T, E]
thematicPrimary Plural   Third  = [N, S, I]

athematicPrimary :: PersonalEnding
athematicPrimary Singular First  = [M, I]
athematicPrimary Singular Second = [Th, A]
athematicPrimary Plural   Third  = [Aa, S, I]
athematicPrimary n        p      = thematicPrimary n p

thematicSecondary :: PersonalEnding
thematicSecondary Singular First  = [N]
thematicSecondary Singular Second = [S]
thematicSecondary Singular Third  = []
thematicSecondary Plural   First  = [M, E, N]
thematicSecondary Plural   Second = [T, E]
thematicSecondary Plural   Third  = [N]

athematicSecondary :: PersonalEnding
athematicSecondary Plural   Third  = [S, A, N]
athematicSecondary n        p      = athematicSecondary n p

addThematicVowel :: [GreekAlphabet] -> [GreekAlphabet]
addThematicVowel ending@(M:_) = N.toList $ contract O ending
addThematicVowel ending@(N:_) = N.toList $ contract O ending
addThematicVowel ending@(I:_) = N.toList $ contract O ending
addThematicVowel ending@_     = N.toList $ contract E ending

thematicIndActPrimary :: PersonalEnding
thematicIndActPrimary Singular First  = [W]
thematicIndActPrimary Singular Second = [E, I, S]
thematicIndActPrimary Singular Third  = [E, I]
thematicIndActPrimary Plural   Third  = [O, U, S, I]
thematicIndActPrimary n        p      = addThematicVowel $ thematicPrimary n p

thematicActPrimaryInfinitive :: [GreekAlphabet]
thematicActPrimaryInfinitive = addThematicVowel [E, N]

addS :: StemConstructor
addS (GreekWord (x N.:| stemInit)) | x `elem` labials  = GreekWord $ Ps N.:| stemInit
addS (GreekWord (x N.:| stemInit)) | x `elem` palatals = GreekWord $ X  N.:| stemInit
addS (GreekWord (x N.:| stemInit)) | x `elem` dentals  = GreekWord $ S  N.:| stemInit
addS (GreekWord (x N.:| stemInit)) | x `elem` liquids  = GreekWord $ S  N.:| E : stemInit
addS (GreekWord stem)                                  = GreekWord $ S  N.<| stem

-- TODO: ρρ
addAugment :: StemConstructor
addAugment = GreekWord . N.reverse . addAugment' . N.reverse . unGreekWord
  where
    addAugment' (O N.:| (U : stemTail))                    = O  N.:| U : stemTail
    addAugment' (x N.:| stemTail) | x `elem` [A, Aa, E, H] = H  N.:| stemTail
    addAugment' (x N.:| stemTail) | x `elem` [I, Ii]       = Ii N.:| stemTail
    addAugment' (x N.:| stemTail) | x `elem` [O, W]        = W  N.:| stemTail
    addAugment' (x N.:| stemTail) | x `elem` [U, Uu]       = Uu N.:| stemTail
    addAugment' stem                                       = E  N.<| stem

thematicIndActImpf :: PersonalEnding
thematicIndActImpf n p = addThematicVowel $ thematicSecondary n p

thematicIndActAor :: PersonalEnding
thematicIndActAor Singular   First   = [A]
thematicIndActAor n@Singular p@Third = E : thematicSecondary n p
thematicIndActAor n          p       = A : thematicSecondary n p

lenthenThematicVowel :: [GreekAlphabet] -> [GreekAlphabet]
lenthenThematicVowel (O:ending) = W : ending
lenthenThematicVowel (E:ending) = H : ending
lenthenThematicVowel x          = x

thematicSubjActPrimary :: PersonalEnding
thematicSubjActPrimary n p = w $ lenthenThematicVowel $ thematicIndActPrimary n p
  where
    w (W:U:ending) = W : ending
    w x            = x

-- TODO: 母音融合動詞で処理を変える方法
addOptativeSuffix :: [GreekAlphabet] -> [GreekAlphabet]
addOptativeSuffix = (I:)

optativePE :: PersonalEnding
optativePE Singular First = [M, I]
optativePE Plural   Third = [E, N]
optativePE n'       p'    = thematicSecondary n' p'

thematicOptActPrimary :: PersonalEnding
thematicOptActPrimary n p = addThematicVowel $ addOptativeSuffix $ optativePE n p

stemAndThPE :: Mood -> Voice -> Tense t -> (StemConstructor, PersonalEnding)
stemAndThPE Indicative  Active Present   = (id               , thematicIndActPrimary)
stemAndThPE Indicative  Active Future    = (addS             , thematicIndActPrimary)
stemAndThPE Indicative  Active Imperfect = (addAugment       , thematicIndActImpf)
stemAndThPE Indicative  Active Aorist    = (addAugment . addS, thematicIndActAor)
stemAndThPE Subjunctive Active Present   = (id               , thematicSubjActPrimary)
stemAndThPE Subjunctive Active Aorist    = (addS             , thematicSubjActPrimary)
stemAndThPE Optative    Active Present   = (id               , thematicOptActPrimary)
stemAndThPE _           _      _         = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
