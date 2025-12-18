-- AI-generated tests based on Parser.hs
-- (Might require some fine-tuning)

{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Text.Megaparsec (runParser)
import Fountain.Parser
import Fountain.AST
import Data.Text (Text)

spec :: Spec
spec = do

  describe "Title Page" $ do
    it "parses a simple title field" $
      runParser titlePage "" "Title: My Screenplay\n\n"
        `shouldBe` Right (TitlePage [TitleField "Title" [[TextNode "My Screenplay"]]])

  describe "SceneHeading" $ do
    it "parses INT. scene heading" $
      runParser sceneHeading "" "INT. HOUSE\n"
        `shouldBe` Right (SceneHeading "INT. HOUSE" False Nothing)

    it "parses forced .EXT. scene heading with number" $
      runParser sceneHeading "" ".EXT. PARK #12#\n"
        `shouldBe` Right (SceneHeading "EXT. PARK" True (Just (SceneNumber "#12#")))

  describe "Action" $ do
    it "parses action line" $
      runParser action "" "He walks into the room.\n"
        `shouldBe` Right (Action [[TextNode "He walks into the room."]] False)

    it "parses forced action line" $
      runParser action "" "!Explosion rocks the building.\n"
        `shouldBe` Right (Action [[TextNode "Explosion rocks the building."]] True)

  describe "Character" $ do
    it "parses character name" $
      runParser character "" "JOHN\n"
        `shouldBe` Right (Character "JOHN" False Nothing False)

    it "parses forced character name" $
      runParser character "" "@MARY\n"
        `shouldBe` Right (Character "MARY" True Nothing False)

  describe "Dialogue Block" $ do
    it "parses dialogue lines" $
      runParser dialogue "" "JOHN\nHello!\n"
        `shouldBe` Right (FEDialogue (DialogueBlock (Character "JOHN" False Nothing False)
                                        [DialogueText [TextNode "Hello!"]]))

  describe "Parenthetical" $ do
    it "parses parenthetical" $
      runParser parenthetical "" "(whispering)\n"
        `shouldBe` Right (Parenthetical [TextNode "whispering"])

  describe "DualDialogue" $ do
    it "parses dual dialogue (0 whitespace)" $
      runParser dialogue "" "JOHN\nHello!\n\nMARY^\nHi!\n"
        `shouldBe` Right (FEDualDialogue (DualDialogue
          (DialogueBlock (Character "JOHN" False Nothing False) [DialogueText [TextNode "Hello!"]])
          (DialogueBlock (Character "MARY" False Nothing True) [DialogueText [TextNode "Hi!"]])))

    it "parses dual dialogue (1 whitespace)" $
      runParser dialogue "" "JOHN\nHello!\n\nMARY ^\nHi!\n"
        `shouldBe` Right (FEDualDialogue (DualDialogue
          (DialogueBlock (Character "JOHN" False Nothing False) [DialogueText [TextNode "Hello!"]])
          (DialogueBlock (Character "MARY" False Nothing True) [DialogueText [TextNode "Hi!"]])))

    it "parses dual dialogue (> 1 whitespace)" $
      runParser dialogue "" "JOHN\nHello!\n\nMARY    ^\nHi!\n"
        `shouldBe` Right (FEDualDialogue (DualDialogue
          (DialogueBlock (Character "JOHN" False Nothing False) [DialogueText [TextNode "Hello!"]])
          (DialogueBlock (Character "MARY" False Nothing True) [DialogueText [TextNode "Hi!"]])))
    
    it "parses dual dialogue (Trailing whitespace)" $
      runParser dialogue "" "JOHN\nHello!\n\nMARY ^   \nHi!\n"
        `shouldBe` Right (FEDualDialogue (DualDialogue
          (DialogueBlock (Character "JOHN" False Nothing False) [DialogueText [TextNode "Hello!"]])
          (DialogueBlock (Character "MARY" False Nothing True) [DialogueText [TextNode "Hi!"]])))

  describe "Transition" $ do
    it "parses transition" $
      runParser transition "" "CUT TO:\n"
        `shouldBe` Right (Transition "CUT TO:" False)

    it "parses forced transition" $
      runParser transition "" ">FADE OUT:\n"
        `shouldBe` Right (Transition "FADE OUT:" True)

  describe "CentredText" $ do
    it "parses centred text" $
      runParser centredText "" ">THE END<\n"
        `shouldBe` Right (CentredText [TextNode "THE END"])

  describe "Lyric" $ do
    it "parses lyric line" $
      runParser lyric "" "~La la la\n"
        `shouldBe` Right (Lyric [TextNode "La la la"])

  describe "Section" $ do
    it "parses section heading" $
      runParser section "" "## Act One\n"
        `shouldBe` Right (Section 2 "Act One")

  describe "Synopsis" $ do
    it "parses synopsis line" $
      runParser synopsis "" "=This is a synopsis\n"
        `shouldBe` Right (Synopsis "This is a synopsis")

  describe "Note" $ do
    it "parses note" $
      runParser note "" "[[This is a note]]\n"
        `shouldBe` Right (Note "This is a note")

  describe "Boneyard" $ do
    it "parses boneyard" $
      runParser boneyard "" "/*This is ignored*/\n"
        `shouldBe` Right (Boneyard "This is ignored")

  describe "PageBreak" $ do
    it "parses page break" $
      runParser pageBreak "" "=====\n"
        `shouldBe` Right PageBreak
-- Unit tests for individual parsers using hspec or tasty.
