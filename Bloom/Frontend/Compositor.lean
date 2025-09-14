import Bloom.Frontend.Lexer
import Bloom.Frontend
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

def composite : List (Located LexerToken) -> Id (List <| Located Token) := fun

  -- spacing is kept track of for tokens where it **may** matter
  -- parsers can always be made to ignore this later

  | _ @: .whiteSpace :: x @: .symbolic "?." :: ts => x @: .spacedBind :$: x @: .spacedDot :$: composite ts
  | _ @: .whiteSpace :: x @: .symbolic "?"  :: ts => x @: .spacedBind                     :$: composite ts
  | _ @: .whiteSpace :: x @: .symbolic "."  :: ts => x @: .spacedDot                      :$: composite ts
  | _ @: .whiteSpace :: x @: .openParens    :: ts => x @: .spacedOpenParens               :$: composite ts
  | _ @: .whiteSpace :: x @: .openSquare    :: ts => x @: .spacedOpenSquare               :$: composite ts

  | x @: .indentation str :: y @: .symbolic "?." :: ts => x @: .indentation str :$: y @: .spacedBind :$: y @: .spacedDot :$: composite ts
  | x @: .indentation str :: y @: .symbolic "?"  :: ts => x @: .indentation str :$: y @: .spacedBind                     :$: composite ts
  | x @: .indentation str :: y @: .symbolic "."  :: ts => x @: .indentation str :$: y @: .spacedDot                      :$: composite ts
  | x @: .indentation str :: y @: .openParens    :: ts => x @: .indentation str :$: y @: .spacedOpenParens               :$: composite ts
  | x @: .indentation str :: y @: .openSquare    :: ts => x @: .indentation str :$: y @: .spacedOpenSquare               :$: composite ts

  -- literals

  | x @: .natural n :: y @: .snake unit :: ts => (x ++ y) @: .naturalWithUnit n unit :$: composite ts
  | x @: .natural n                     :: ts => x @: .natural n                     :$: composite ts
  | x @: .stringLiteral str             :: ts => x @: .string str                    :$: composite ts
  | x @: .stringInterpolateStart str    :: ts => x @: .interpolatedStringStart str   :$: composite ts
  | x @: .StringInterpolateMiddle str   :: ts => x @: .interpolatedStringMiddle str  :$: composite ts
  | x @: .stringInterpolateEnd str      :: ts => x @: .interpolatedStringEnd str     :$: composite ts

  -- keyword identification and forwarding

  | x @: .documentationComment com :: ts => x @: .docComment com  :$: composite ts
  | x @: .decorator                :: ts => x @: .openDecorator   :$: composite ts
  | x @: .indentation str          :: ts => x @: .indentation str :$: composite ts

  | x @: .symbolic "?." :: ts => x @: .bind :$: x @: .dot :$: composite ts
  | x @: .symbolic "?"  :: ts => x @: .bind               :$: composite ts
  | x @: .symbolic "."  :: ts => x @: .dot                :$: composite ts
  | x @: .openParens    :: ts => x @: .openParens         :$: composite ts
  | x @: .closeParens   :: ts => x @: .closeParens        :$: composite ts
  | x @: .openSquare    :: ts => x @: .openSquare         :$: composite ts
  | x @: .closeSquare   :: ts => x @: .closeSquare        :$: composite ts
  | x @: .openCurly     :: ts => x @: .openCurly          :$: composite ts
  | x @: .closeCurly    :: ts => x @: .closeCurly         :$: composite ts
  | x @: .symbolic "->" :: ts => x @: .functionFn         :$: composite ts
  | x @: .symbolic "~>" :: ts => x @: .functionFnMut      :$: composite ts
  | x @: .symbolic "-+" :: ts => x @: .functionFnOnce     :$: composite ts
  | x @: .symbolic "-*" :: ts => x @: .functionFnMust     :$: composite ts
  | x @: .symbolic "="  :: ts => x @: .equals             :$: composite ts
  | x @: .symbolic "?=" :: ts => x @: .bindEquals         :$: composite ts
  | x @: .symbolic "=>" :: ts => x @: .arrow              :$: composite ts
  | x @: .symbolic "|"  :: ts => x @: .gaurd              :$: composite ts
  | x @: .symbolic ":"  :: ts => x @: .isType             :$: composite ts
  | x @: .symbolic "::" :: ts => x @: .subDefinition      :$: composite ts
  | x @: .comma         :: ts => x @: .comma              :$: composite ts
  | x @: .semicolon     :: ts => x @: .semicolon          :$: composite ts

  | x @: .symbolic "&" :: y @: .snake "inout" :: ts => (x ++ y) @: .refInout :$: composite ts
  | x @: .symbolic "&" :: y @: .snake "mut"   :: ts => (x ++ y) @: .refMut   :$: composite ts
  | x @: .symbolic "&" :: y @: .snake "pin"   :: ts => (x ++ y) @: .refPin   :$: composite ts
  | x @: .symbolic "&"                        :: ts => x @: .ref             :$: composite ts

  | x @: .snake "mod"  :: ts => x @: .modKW  :$: composite ts
  | x @: .snake "pub"  :: ts => x @: .pubKW  :$: composite ts
  | x @: .snake "prot" :: ts => x @: .protKW :$: composite ts
  | x @: .snake "use"  :: ts => x @: .useKW  :$: composite ts
  | x @: .snake "as"   :: ts => x @: .asKW   :$: composite ts
  | x @: .snake "all"  :: ts => x @: .allKW  :$: composite ts

  | x @: .snake "move"   :: ts => x @: .moveKW   :$: composite ts
  | x @: .snake "inout"  :: ts => x @: .inoutKW  :$: composite ts
  | x @: .snake "mut"    :: ts => x @: .mutKW    :$: composite ts
  | x @: .snake "pin"    :: ts => x @: .pinKW    :$: composite ts
  | x @: .snake "ref"    :: ts => x @: .refKW    :$: composite ts
  | x @: .snake "static" :: ts => x @: .staticKW :$: composite ts

  | x @: .snake "def"   :: ts => x @: .defKW        :$: composite ts
  | x @: .snake "where" :: ts => x @: .whereKW      :$: composite ts
  | x @: .snake "new"   :: ts => x @: .newKW        :$: composite ts
  | x @: .snake "fn"    :: ts => x @: .fnKW         :$: composite ts
  | x @: .snake "_"     :: ts => x @: .underscoreKW :$: composite ts

  | x @: .snake "self"      :: ts => x @: .selfKW      :$: composite ts
  | x @: .pascal "Self"     :: ts => x @: .selfTypeKW  :$: composite ts
  | x @: .snake "interface" :: ts => x @: .interfaceKW :$: composite ts
  | x @: .snake "trait"     :: ts => x @: .traitKW     :$: composite ts
  | x @: .snake "impl"      :: ts => x @: .implKW      :$: composite ts
  | x @: .snake "derive"    :: ts => x @: .deriveKW    :$: composite ts
  | x @: .snake "is"        :: ts => x @: .isKW        :$: composite ts

  | x @: .snake "do"    :: ts => x @: .doKW                  :$: composite ts
  | x @: .snake "let"   :: ts => x @: .letKW                 :$: composite ts
  | x @: .snake "for"   :: ts => x @: .forKW                 :$: composite ts
  | x @: .snake "in"    :: ts => x @: .inKW                  :$: composite ts
  | x @: .snake "loop"  :: ts => x @: .loopKW                :$: composite ts
  | x @: .snake "match" :: ts => x @: .matchKW               :$: composite ts
  | x @: .snake "if"    :: ts => x @: .ifKW                  :$: composite ts
  | x @: .snake "then"  :: ts => x @: .thenKW                :$: composite ts
  | x @: .snake "else"  :: ts => x @: .elseKW                :$: composite ts
  | x @: .snake "elif"  :: ts => x @: .elseKW :$: x @: .ifKW :$: composite ts

  -- reservation

  | x @: .symbolic "!."   :: ts => x @: .reservedKeyword "!."      :$: composite ts
  | x @: .symbolic "!"    :: ts => x @: .reservedKeyword "!"       :$: composite ts
  | x @: .snake "super"   :: ts => x @: .reservedKeyword "super"   :$: composite ts
  | x @: .pascal "Super"  :: ts => x @: .reservedKeyword "Super"   :$: composite ts
  | x @: .snake "inherit" :: ts => x @: .reservedKeyword "inherit" :$: composite ts
  | x @: .snake "class"   :: ts => x @: .reservedKeyword "class"   :$: composite ts
  | x @: .snake "mixin"   :: ts => x @: .reservedKeyword "mixin"   :$: composite ts
  | x @: .snake "extend"  :: ts => x @: .reservedKeyword "extend"  :$: composite ts
  | x @: .snake "with"    :: ts => x @: .reservedKeyword "with"    :$: composite ts
  | x @: .snake "open"    :: ts => x @: .reservedKeyword "open"    :$: composite ts
  | x @: .snake "close"   :: ts => x @: .reservedKeyword "close"   :$: composite ts
  | x @: .snake "fold"    :: ts => x @: .reservedKeyword "fold"    :$: composite ts
  | x @: .snake "weave"   :: ts => x @: .reservedKeyword "weave"   :$: composite ts
  | x @: .snake "effect"  :: ts => x @: .reservedKeyword "effect"  :$: composite ts

  -- unless otherwise specified these are identifiers

  | x @: .symbolic name :: ts => x @: .symbolicIdentifier   name :$: composite ts
  | x @: .snake name    :: ts => x @: .snakeCaseIdentifier  name :$: composite ts
  | x @: .pascal name   :: ts => x @: .pascalCaseIdentifier name :$: composite ts

  -- unless otherwise specified whitespace is ignored

  | _ @: .whiteSpace :: ts => composite ts
  | [] => return []
