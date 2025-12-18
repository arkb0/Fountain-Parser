-- File for Data Types

module Fountain.AST
  ( -- Top-level document
    FountainDocument(..)
  , TitlePage(..), TitleField(..)
  , FountainScript
  , FountainElement(..)

    -- Scene headings and numbers
  , SceneHeading(..)
  , SceneNumber(..)

    -- Characters, dialogue, parentheticals, dual dialogue
  , Character(..)
  , DialogueBlock(..)
  , DialogueLine(..)
  , Parenthetical(..)
  , DualDialogue(..)

    -- Transitions, action, centred text, lyrics
  , Transition(..)
  , Action(..)
  , CentredText(..)
  , Lyric(..)

    -- Sections and synopses
  , Section(..)
  , Synopsis(..)

    -- Notes, boneyard, page breaks
  , Note(..)
  , Boneyard(..)
  , PageBreak(..)
  , BlankLine(..)

    -- Emphasis (optional inline modelling)
  , Inline(..)
  , RichText
  ) where

import Data.Text (Text)

-- | A complete Fountain document with an optional title page and a screenplay body
data FountainDocument = FountainDocument
  { fdTitlePage :: Maybe TitlePage
  , fdScript :: FountainScript
  } deriving (Eq, Show)

-- Title Page --

-- | The (optional) title page is ust a list of fields, preserving order
newtype TitlePage = TitlePage { tpFields :: [TitleField] }
  deriving (Eq, Show)

-- | A single key-value entry in the title page
data TitleField = TitleField
  { tfKey :: Text
  , tfValues :: [RichText]
  } deriving (Eq, Show)

-- | A sequence of screenplay elements
type FountainScript = [FountainElement]

-- | The full range of Fountain block-level elements
data FountainElement
  = FESceneHeading SceneHeading
  | FEAction Action
  | FECharacter Character
  | FEDialogue DialogueBlock
  | FEParenthetical Parenthetical
  | FEDualDialogue DualDialogue
  | FETransition Transition
  | FECentred CentredText
  | FELyric Lyric
  | FENote Note
  | FESection Section
  | FESynopsis Synopsis
  | FEBoneyard Boneyard
  | FEPageBreak PageBreak
  | FEBlankLine BlankLine
  deriving (Eq, Show)

-- Blank lines
data BlankLine = BlankLine
  deriving (Eq, Show)

-- Scene Headings --

-- | Scene headings can be automatic (INT/EXT/etc.) or forced (leading '.')
data SceneHeading = SceneHeading
  { shText :: Text -- The raw heading text, sans the forcing dot
  , shForced :: Bool -- True if forced by a leading '.'
  , shNumber :: Maybe SceneNumber -- Optional #...# scene number
  } deriving (Eq, Show)

-- | Scene numbers: ALphanumerics + dashes or full stops, wrapped in #
newtype SceneNumber = SceneNumber { unSceneNumber :: Text }
  deriving (Eq, Show)

-- Action --

-- | Action Paragraphs; preserves writer-controlled line breaks and indentation
data Action = Action
  { actionLines :: [RichText] -- Lines within the action block
  , actionForcedBang :: Bool -- True if forced by a leading '!'
  } deriving (Eq, Show)

-- Characters, Dialoge, Parentheticals --

-- | Chaacter lines can be forced by '@' and may carry an extension.
data Character = Character
  { charName :: Text -- Display name (case preserved)
  , charForcedAt :: Bool -- True if forced by a leading '@'
  , charExtension :: Maybe Text -- Optional inline extension, e.g. (O.S.), (on the radio)
  , charDual :: Bool -- True if ending with a '^' (for dual dialogues)
  } deriving (Eq, Show)

-- | Dialogue follows a character (and may include parentheticals between lines)
data DialogueBlock = DialogueBlock
  { dlgSpeaker :: Character
  , dlgLines :: [DialogueLine]
  } deriving (Eq, Show)

-- | A single dialogue line or an inline parenthetical within dialogue
data DialogueLine
  = DialogueText RichText
  | DialogueParen Parenthetical
  deriving (Eq, Show)

-- | Parentheticals ae wrapped in brackets and may appear after character or inside dialogue.
newtype Parenthetical = Parenthetical { unParenthetical :: RichText }
  deriving (Eq, Show)

-- Dual Dialogue --

-- | Dual/simultaneous dialogue: The second character line is marked with '^'
data DualDialogue = DualDialogue
  { ddLeft :: DialogueBlock
  , ddRight :: DialogueBlock
  } deriving (Eq, Show)

-- Tansitions --

-- | Transitions: Uppercase, blank lines around, ending in 'TO:', or forced with '>'
data Transition = Transition
  { trText :: Text -- e.g., 'CUT TO:'
  , trForcedGt :: Bool -- True if forced by a leading '>'
  } deriving (Eq, Show)

-- Centred Text --

-- | Centred text is action styled as centred, bracketed with '>' and '<'
newtype CentredText = CentredText { ctText :: RichText }
  deriving (Eq, Show)

-- Lyrics --

-- | Lyrics lines start with '~' and are always forced.
newtype Lyric = Lyric { unLyric :: RichText }
  deriving (Eq, Show)

-- Sections and Synopses --

-- | Sections are writer-only structural markers '#'..'####'. Ignored in the output.
data Section = Section
  { secLevel :: Int -- Number of #s (1..N)
  , secText :: Text
  } deriving (Eq, Show)

-- | Synopsis lines are prefixed with '=' (writer-only, ignored in the output).
newtype Synopsis = Synopsis { unSynopsis :: Text }
  deriving (Eq, Show)

-- Notes --

-- | Notes are enclosed in double square brackets [[ ... ]], may be inline o standalone
newtype Note = Note { unNote :: Text }
  deriving (Eq, Show)

-- Boneyard --

-- | Boneyard ignores wrapped content: /* ... */ (may span multiple lines)
newtype Boneyard = Boneyard { unBoneyard :: Text }
  deriving (Eq, Show)

-- Page Breaks --

-- | Page breaks are lines of '===' (three or more equals signs)
data PageBreak = PageBreak
  deriving (Eq, Show)

-- Inline Emphasis --

-- | Optional inline rich text representation to model emphasis.
-- (Emphasis can be parsed later during rendering)
type RichText = [Inline]

data Inline
  = TextNode Text -- Plain text
  | EmItalic [Inline] -- *Italics*
  | EmBold [Inline] -- **Bold**
  | EmBoldItalics [Inline] -- ***Bold italics***
  | EmUnderline [Inline] -- _underline_
  | EmEscaped Text -- \Backslash-escaped literal
  | EmNoteInline Text -- [[ Inline note ]] embedded within a line
  deriving (Eq, Show)
