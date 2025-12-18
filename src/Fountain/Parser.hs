{-# LANGUAGE OverloadedStrings #-}

-- Implement Megaparsec parsers for each construct.

module Fountain.Parser
  ( parseFountain
  , fountainDocument
  , titlePage
  , fountainScript
  , sceneHeading
  , action
  , character
  , dialogue
  , parenthetical
  , transition
  , centredText
  , lyric
  , section
  , synopsis
  , note
  , boneyard
  , pageBreak
  , inlineText
  ) where

import Fountain.AST
import Control.Monad (when, guard)
import Data.Text (Text, isPrefixOf, isSuffixOf)
import Data.Char (isAlpha, isAsciiLower)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Functor (($>))

type Parser = Parsec Void Text

-- | Parse an entire Fountain document
parseFountain :: FilePath -> Text -> Either String FountainDocument
parseFountain path input =
  case runParser fountainDocument path input of
    Left err -> Left (errorBundlePretty err)
    Right doc -> Right doc

-- | Top-level parse: An optional title page, then the screenplay
fountainDocument :: Parser FountainDocument
fountainDocument = do
  tp <- optional titlePage
  FountainDocument tp <$> fountainScript

-- | The title page: A sequence of key: value pairs until a blank line
titlePage :: Parser TitlePage
titlePage = do
  fields <- some titleField
  _ <- many eol
  return $ TitlePage fields

titleField :: Parser TitleField
titleField = do
  key <- takeWhile1P Nothing (\c -> c /= ':' && c /= '\n')
  _ <- char ':'
  -- Either inline or an indented block
  inlineVal <- optional (hspace *> takeWhileP Nothing (/= '\n'))
  _ <- eol
  indentedVals <- many (try indentedLine)
  let vals = case inlineVal of
                Just v -> inlineText v : indentedVals
                Nothing -> indentedVals
  return $ TitleField (T.strip key) vals

indentation :: Parser ()
indentation =
  (try (count 3 (char ' ')) Data.Functor.$> ()) <|> (tab Data.Functor.$> ())

indentedLine :: Parser RichText
indentedLine = do
  indentation
  txt <- takeWhileP Nothing (/= '\n')
  _ <- eol
  return (inlineText txt)

-- | The script (many elements)
fountainScript :: Parser FountainScript
fountainScript = do
  _ <- many blankLine
  manyTill (fountainElement <* many blankLine) eof
-- fountainScript = manyTill fountainElement eof

-- Ordered by precedence (most specific to least specific)
fountainElement :: Parser FountainElement
fountainElement = choice
  [ FESceneHeading <$> try sceneHeading
  , FETransition <$> try transition
  , FEParenthetical <$> parenthetical
  , FECentred <$> centredText
  , FELyric <$> lyric
  , FESection <$> section
  , FESynopsis <$> synopsis
  , FENote <$> note
  , FEBoneyard <$> boneyard
  , FEPageBreak <$> pageBreak
  , FEBlankLine <$> blankLine
  , try dialogue
  , FECharacter <$> try character -- Characters introduce dialogue => Not standalone elements, but we need this fallback as a safe option
  , FEAction <$> action
  ]

-- Blank lines are assumed to be intentional
blankLine :: Parser BlankLine
blankLine = do
  _ <- many (char ' ' <|> char '\t')
  _ <- eol
  return BlankLine

-- Scene Headings --
sceneHeading :: Parser SceneHeading
sceneHeading = do
  forced <- optional (char '.')
  -- Require INT./EXT. etc. at the start, unless forced
  keyword <- choice
    [ string' "INT"
    , string' "EXT"
    , string' "EST"
    , string' "INT./EXT"
    , string' "INT/EXT"
    , string' "I/E"
    ]
  sep <- oneOf ['.', ' '] -- Keyword followed by a '.' or a space
  rest <- takeWhileP Nothing (/= '\n')
  _ <- eol
  let txt = T.concat [keyword, T.singleton sep, rest]
      (heading, num) =
        case T.breakOn "#" txt of
          (h, "") -> (h, Nothing)
          (h, n) -> (h, Just (SceneNumber n))
  return $ SceneHeading (T.strip heading) (forced == Just '.') num

-- Action --
action :: Parser Action
action = do
  forced <- optional (char '!')
  line <- takeWhileP Nothing (/= '\n')
  _ <- eol
  return $ Action [inlineText line] (forced == Just '!')

-- Character and Dialogue --
character :: Parser Character
character = do
  forced <- optional (char '@')
  -- indentation
  _ <- hspace
  -- name + optional extension
  raw <- takeWhileP Nothing (\c -> c /= '^' && c /= '\n')
  dual <- option False (hspace *> char '^' *> hspace $> True)
  _ <- eol

  let (nm, ext) =
        case T.breakOn "(" raw of
          (n, "") -> (n, Nothing)
          (n, e) -> (n, Just (T.strip e))
      strippedName = T.strip nm
      -- Must contain at least one alphabetic character
      hasAlpha = T.any isAlpha strippedName
      -- Uppercase, or forced
      validCase = forced == Just '@' || strippedName == T.toUpper strippedName
      -- Extension must be all uppercase (can include spaces, punctuation) - helps disambiguate from Action lines introducint a NEW CHARACTER
      extUppercase = maybe True (not . T.any isAsciiLower) ext

  guard hasAlpha
  guard validCase
  guard extUppercase
  return $ Character strippedName (forced == Just '@') ext dual

dialogueBlock :: Parser DialogueBlock
dialogueBlock = do
  spk <- character
  dialogueLines <- some dialogueLine
  return $ DialogueBlock spk dialogueLines

dialogueLine :: Parser DialogueLine
dialogueLine = choice
  [ DialogueParen <$> parenthetical
  , DialogueText <$> nonEmptyDialogueText
  ]

-- Non-empty text line for dialogues (a blank line ends the dialogue block)
nonEmptyDialogueText :: Parser RichText
nonEmptyDialogueText = do
  -- Require at least one non-newline character
  txt <- takeWhile1P (Just "dialogue text") (/= '\n')
  _ <- eol
  return (inlineText txt)

{-
-- End-of-dialogue sentinel
-- Stops on a blank line, or when another element begins, or at eof
endOfDialogue :: Parser ()
endOfDialogue = choice
  [ lookAhead blankLine
  , lookAhead (void sceneHeading)
  , lookAhead (void transition)
  , lookAhead (void centredText)
  , lookAhead (void lyric)
  , lookAhead (void section)
  , lookAhead (void synopsis)
  , lookAhead (void note)
  , lookAhead (void boneyard)
  , lookAhead (void pageBreak)
  , eof
  ]

-- A blank line is a single eol at the beginning of a line
blankLine :: Parser()
blankLine = void eol
-}

parenthetical :: Parser Parenthetical
parenthetical = do
  _ <- char '('
  txt <- takeWhileP Nothing (/= ')')
  _ <- char ')'
  _ <- eol
  return $ Parenthetical (inlineText txt)

-- Dialogue Block or Dual Dialogue
dialogue :: Parser FountainElement
dialogue = do
  first <- dialogueBlock
  msecond <- optional (try dualDialogueTail)
  case msecond of
    Just second -> return $ FEDualDialogue (DualDialogue first second)
    Nothing -> return $ FEDialogue first

-- Parse the second half of a dual dialogue (with the ^)
dualDialogueTail :: Parser DialogueBlock
dualDialogueTail = do
  _ <- many eol
  spk <- character
  if charDual spk -- Only valid if a caret was present
    then DialogueBlock spk <$> some dialogueLine
    else fail "Expected dual character (with ^)"

-- Transitions --
transition :: Parser Transition
transition = do
  forced <- optional (char '>')
  when (forced /= Just '>') $ do
    -- Validate without consuming
    lookAhead $ do
      line <- takeWhileP Nothing (/= '\n')
      let stripped = T.strip line
      guard ("TO:" `isSuffixOf` T.toUpper stripped
             || "FADE IN" `isPrefixOf` T.toUpper stripped
             || "FADE OUT" `isPrefixOf` T.toUpper stripped)
  txt <- takeWhileP Nothing (/= '\n')
  _ <- eol
  let stripped = T.strip txt
  return $ Transition stripped (forced == Just '>')

-- Centred Text --
centredText :: Parser CentredText
centredText = do
  _ <- char '>'
  txt <- takeWhileP Nothing (/= '<')
  _ <- char '<'
  _ <- eol
  return $ CentredText (inlineText txt)

-- Lyrics --
lyric :: Parser Lyric
lyric = do
  _ <- char '~'
  txt <- takeWhileP Nothing (/= '\n')
  _ <- eol
  return $ Lyric (inlineText txt)

-- Sections and Synopses --
section :: Parser Section
section = do
  hashes <- some (char '#')
  txt <- takeWhileP Nothing (/= '\n')
  _ <- eol
  return $ Section (length hashes) (T.strip txt)

synopsis :: Parser Synopsis
synopsis = do
  _ <- char '='
  txt <- takeWhileP Nothing (/= '\n')
  _ <- eol
  return $ Synopsis (T.strip txt)

-- Notes --
note :: Parser Note
note = do
  _ <- string "[["
  txt <- manyTill anySingle (try (string "]]"))
  _ <- eol
  return $ Note (T.pack txt)

-- Boneyard --
boneyard :: Parser Boneyard
boneyard = do
  _ <- string "/*"
  txt <- manyTill anySingle (try (string "*/"))
  _ <- eol
  return $ Boneyard (T.pack txt)

-- Page Breaks --
pageBreak :: Parser PageBreak
pageBreak = do
  _ <- count 3 (char '=')
  _ <- many (char '=')
  _ <- eol
  return PageBreak

-- Inline Emphasis --
inlineText :: Text -> RichText
inlineText t = [TextNode (T.strip t)]
