-- Step 1: ("Luca", "Stefano")
-- Step 2: ("Francesco", "Stefano")

module Main where

import Data.List
import Test.Hspec

type InputString = String
type CensoredWord = String
type OutputString = String

subs :: CensoredWord -> InputString -> OutputString
subs "" rest = rest
subs _ "" = ""
subs censored word | censored `isPrefixOf` word = 'X' : subs (tail censored) (tail word)
                   | otherwise = (head word) : subs censored (tail word)

multisubs :: [CensoredWord] -> InputString -> OutputString
multisubs censoreds sentence = foldl (flip subs) sentence censoreds

--

test :: IO ()
test = hspec spec

spec :: Spec
spec = do
  let blacklist = ["nice", "pony", "sun", "light", "fun", "happy", "funny", "joy"]
  let censoring = "Such a nice day with a bright sun, makes me happy"
  let expected = "Such a XXXX day with a bright XXX, makes me XXXXX"
  describe "subs" $ do
    it "replaces lbe in albero" $
      subs "lbe" "albero" `shouldBe` "aXXXro"
  describe "multisubs" $ do
    it "replaces nice words" $
      multisubs blacklist censoring `shouldBe` expected
