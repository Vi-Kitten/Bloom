import Bloom.Frontend.Lexer
import Bloom.Basic

inductive Token
  -- misc
  | docComment : String -> Token -- "#| "
  | openDecorator -- "#["
  | indentation     : String -> Token
  | reservedKeyword : String -> Token -- error to be embeded in AST
  -- identifiers
  | symbolicIdentifier   : String -> Token
  | snakeCaseIdentifier  : String -> Token
  | pascalCaseIdentifier : String -> Token
  -- literals
  | natural         : Nat -> Token -- "10"
  | naturalWithUnit : Nat -> String -> Token -- "10s"
  | string                   : String -> Token
  | interpolatedStringStart  : String -> Token
  | interpolatedStringMiddle : String -> Token
  | interpolatedStringEnd    : String -> Token
  -- symbolic syntax
  | dot -- "."
  | spacedDot -- " ."
  | bind -- "?"
  | spacedBind -- " ?"
  | openParens -- "("
  | spacedOpenParens -- " ("
  | closeParens -- ")"
  | openSquare -- "["
  | spacedOpenSquare -- " ["
  | closeSquare -- "]"
  | openCurly -- "{"
  | closeCurly -- "}"
  | functionFn -- "->"
  | functionFnMut -- "~>"
  | functionFnOnce -- "-+"
  | functionFnMust -- "-*"
  | equals -- "="
  | bindEquals -- "?="
  | arrow -- "=>"
  | gaurd -- "|"
  | isType -- ":"
  | subDefinition -- "::"
  | comma -- ","
  | semicolon -- ";"
  -- ref types
  | refInout -- "&inout"
  | refMut -- "&mut"
  | refPin -- "&pin"
  | ref -- "&"
  -- imports
  | modKW -- "mod"
  | pubKW -- "pub"
  | protKW -- "prot"
  | useKW -- "use"
  | asKW -- "as"
  | allKW -- "all"
  -- modifiers
  | moveKW
  | inoutKW
  | mutKW
  | pinKW
  | refKW
  | staticKW
  -- general
  | defKW
  | whereKW
  | newKW
  | fnKW
  | underscoreKW
  -- interfaces and traits
  | selfKW
  | selfTypeKW -- "Self"
  | interfaceKW
  | traitKW
  | implKW
  | deriveKW
  | isKW
  -- control flow
  | doKW
  | letKW
  | forKW
  | inKW
  | loopKW
  | matchKW
  | ifKW
  | thenKW
  | elseKW
  deriving Repr

def composite : List LexerToken -> Id (List Token) := fun

  -- spacing is kept track of for tokens where it **may** matter
  -- parsers can always be made to ignore this later

  | .whiteSpace :: .symbolic "?." :: ts => .spacedBind :$: .spacedDot :$: composite ts
  | .whiteSpace :: .symbolic "?"  :: ts => .spacedBind                :$: composite ts
  | .whiteSpace :: .symbolic "."  :: ts => .spacedDot                 :$: composite ts
  | .whiteSpace :: .openParens    :: ts => .spacedOpenParens          :$: composite ts
  | .whiteSpace :: .openSquare    :: ts => .spacedOpenSquare          :$: composite ts

  | .indentation str :: .symbolic "?." :: ts => .indentation str :$: .spacedBind :$: .spacedDot :$: composite ts
  | .indentation str :: .symbolic "?"  :: ts => .indentation str :$: .spacedBind                :$: composite ts
  | .indentation str :: .symbolic "."  :: ts => .indentation str :$: .spacedDot                 :$: composite ts
  | .indentation str :: .openParens    :: ts => .indentation str :$: .spacedOpenParens          :$: composite ts
  | .indentation str :: .openSquare    :: ts => .indentation str :$: .spacedOpenSquare          :$: composite ts

  -- literals

  | .natural n :: .snake unit    :: ts => .naturalWithUnit n unit       :$: composite ts
  | .natural n                   :: ts => .natural n                    :$: composite ts
  | .stringLiteral str           :: ts => .string str                   :$: composite ts
  | .stringInterpolateStart str  :: ts => .interpolatedStringStart str  :$: composite ts
  | .StringInterpolateMiddle str :: ts => .interpolatedStringMiddle str :$: composite ts
  | .stringInterpolateEnd str    :: ts => .interpolatedStringEnd str    :$: composite ts

  -- keyword identification and forwarding

  | .documentationComment com :: ts => .docComment com  :$: composite ts
  | .decorator                :: ts => .openDecorator   :$: composite ts
  | .indentation str          :: ts => .indentation str :$: composite ts

  | .symbolic "?." :: ts => .bind  :$: .dot :$: composite ts
  | .symbolic "?"  :: ts => .bind           :$: composite ts
  | .symbolic "."  :: ts => .dot            :$: composite ts
  | .openParens    :: ts => .openParens     :$: composite ts
  | .closeParens   :: ts => .closeParens    :$: composite ts
  | .openSquare    :: ts => .openSquare     :$: composite ts
  | .closeSquare   :: ts => .closeSquare    :$: composite ts
  | .openCurly     :: ts => .openCurly      :$: composite ts
  | .closeCurly    :: ts => .closeCurly     :$: composite ts
  | .symbolic "->" :: ts => .functionFn     :$: composite ts
  | .symbolic "~>" :: ts => .functionFnMut  :$: composite ts
  | .symbolic "-+" :: ts => .functionFnOnce :$: composite ts
  | .symbolic "-*" :: ts => .functionFnMust :$: composite ts
  | .symbolic "="  :: ts => .equals         :$: composite ts
  | .symbolic "?=" :: ts => .bindEquals     :$: composite ts
  | .symbolic "=>" :: ts => .arrow          :$: composite ts
  | .symbolic "|"  :: ts => .gaurd          :$: composite ts
  | .symbolic ":"  :: ts => .isType         :$: composite ts
  | .symbolic "::" :: ts => .subDefinition  :$: composite ts
  | .comma         :: ts => .comma          :$: composite ts
  | .semicolon     :: ts => .semicolon      :$: composite ts

  | .symbolic "&" :: .snake "inout" :: ts => .refInout  :$: composite ts
  | .symbolic "&" :: .snake "mut"   :: ts => .refMut    :$: composite ts
  | .symbolic "&" :: .snake "pin"   :: ts => .refPin    :$: composite ts
  | .symbolic "&"                   :: ts => .ref       :$: composite ts

  | .snake "mod"  :: ts => .modKW  :$: composite ts
  | .snake "pub"  :: ts => .pubKW  :$: composite ts
  | .snake "prot" :: ts => .protKW :$: composite ts
  | .snake "use"  :: ts => .useKW  :$: composite ts
  | .snake "as"   :: ts => .asKW   :$: composite ts
  | .snake "all"  :: ts => .allKW  :$: composite ts

  | .snake "move"   :: ts => .moveKW   :$: composite ts
  | .snake "inout"  :: ts => .inoutKW  :$: composite ts
  | .snake "mut"    :: ts => .mutKW    :$: composite ts
  | .snake "pin"    :: ts => .pinKW    :$: composite ts
  | .snake "ref"    :: ts => .refKW    :$: composite ts
  | .snake "static" :: ts => .staticKW :$: composite ts

  | .snake "def"   :: ts => .defKW        :$: composite ts
  | .snake "where" :: ts => .whereKW      :$: composite ts
  | .snake "new"   :: ts => .newKW        :$: composite ts
  | .snake "fn"    :: ts => .fnKW         :$: composite ts
  | .snake "_"     :: ts => .underscoreKW :$: composite ts

  | .snake "self"      :: ts => .selfKW      :$: composite ts
  | .pascal "Self"     :: ts => .selfTypeKW  :$: composite ts
  | .snake "interface" :: ts => .interfaceKW :$: composite ts
  | .snake "trait"     :: ts => .traitKW     :$: composite ts
  | .snake "impl"      :: ts => .implKW      :$: composite ts
  | .snake "derive"    :: ts => .deriveKW    :$: composite ts
  | .snake "is"        :: ts => .isKW        :$: composite ts

  | .snake "do"    :: ts => .doKW    :$: composite ts
  | .snake "let"   :: ts => .letKW   :$: composite ts
  | .snake "for"   :: ts => .forKW   :$: composite ts
  | .snake "in"    :: ts => .inKW    :$: composite ts
  | .snake "loop"  :: ts => .loopKW  :$: composite ts
  | .snake "match" :: ts => .matchKW :$: composite ts
  | .snake "if"    :: ts => .ifKW    :$: composite ts
  | .snake "then"  :: ts => .thenKW  :$: composite ts
  | .snake "else"  :: ts => .elseKW  :$: composite ts

  -- reservation

  | .symbolic "!."   :: ts => .reservedKeyword "!."      :$: composite ts
  | .symbolic "!"    :: ts => .reservedKeyword "!"       :$: composite ts
  | .snake "super"   :: ts => .reservedKeyword "super"   :$: composite ts
  | .pascal "Super"  :: ts => .reservedKeyword "Super"   :$: composite ts
  | .snake "inherit" :: ts => .reservedKeyword "inherit" :$: composite ts
  | .snake "class"   :: ts => .reservedKeyword "class"   :$: composite ts
  | .snake "mixin"   :: ts => .reservedKeyword "mixin"   :$: composite ts
  | .snake "extend"  :: ts => .reservedKeyword "extend"  :$: composite ts
  | .snake "with"    :: ts => .reservedKeyword "with"    :$: composite ts
  | .snake "open"    :: ts => .reservedKeyword "open"    :$: composite ts
  | .snake "close"   :: ts => .reservedKeyword "close"   :$: composite ts
  | .snake "fold"    :: ts => .reservedKeyword "fold"    :$: composite ts
  | .snake "weave"   :: ts => .reservedKeyword "weave"   :$: composite ts
  | .snake "effect"  :: ts => .reservedKeyword "effect"  :$: composite ts

  -- unless otherwise specified these are identifiers

  | .symbolic name :: ts => .symbolicIdentifier   name :$: composite ts
  | .snake name    :: ts => .snakeCaseIdentifier  name :$: composite ts
  | .pascal name   :: ts => .pascalCaseIdentifier name :$: composite ts

  -- unless otherwise specified whitespace is ignored

  | .whiteSpace :: ts => composite ts
  | [] => return []
