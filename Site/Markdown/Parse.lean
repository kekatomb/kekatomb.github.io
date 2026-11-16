import Site.Util
import Site.Markdown.Ast

/-!
# Markdown parser (a deliberate subset)

Blocks: ATX headings, paragraphs, fenced code blocks (```` ``` ````),
blockquotes (`>`), flat unordered (`-`, `*`, `+`) and ordered (`1.`, `1)`)
lists, and thematic breaks (`---`, `***`, `___`).

Inlines: `*em*` / `_em_`, `**strong**` / `__strong__`, `` `code` ``,
`[text](url)`, `![alt](src)`, margin notes (`^[note]`, as in Pandoc),
footnote references (`[^label]`), backslash escapes, and hard line breaks
(two trailing spaces or a trailing backslash).

Footnotes: `[^label]: text` on its own line, continued on indented lines.

Not supported: raw HTML (it is escaped), setext headings, indented code
blocks, nested lists, reference links, tables.
-/

namespace Site.Markdown
open Site

/-! ## Inlines -/

def isPunct (c : Char) : Bool := "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~".toList.contains c

def flushBuf (buf : List Char) (out : List Inline) : List Inline :=
  if buf.isEmpty then out else .text (String.ofList buf.reverse) :: out

/-- Split `cs` at the first occurrence of `delim`, with no escape handling
(used inside code spans and link destinations). -/
partial def splitPlain (delim : List Char) (cs : List Char) (acc : List Char := []) :
    Option (List Char × List Char) :=
  match cs with
  | [] => none
  | c :: rest =>
    if delim.isPrefixOf cs then some (acc.reverse, cs.drop delim.length)
    else splitPlain delim rest (c :: acc)

/-- Find a closing `**`/`__` that does not follow whitespace. Skips escapes. -/
partial def findStrongClose (d : Char) (cs : List Char) (acc : List Char := []) :
    Option (List Char × List Char) :=
  match cs with
  | [] => none
  | '\\' :: c :: rest => findStrongClose d rest (c :: '\\' :: acc)
  | c :: c2 :: rest =>
    if c == d && c2 == d && !acc.isEmpty && !isSpace acc.head! then some (acc.reverse, rest)
    else findStrongClose d (c2 :: rest) (c :: acc)
  | [c] => findStrongClose d [] (c :: acc)

/-- Find a single closing `*`/`_` that does not follow whitespace. Doubled
delimiters (`**`) belong to nested strong text and are skipped. -/
partial def findEmphClose (d : Char) (cs : List Char) (acc : List Char := []) :
    Option (List Char × List Char) :=
  match cs with
  | [] => none
  | '\\' :: c :: rest => findEmphClose d rest (c :: '\\' :: acc)
  | c :: rest =>
    if c != d then findEmphClose d rest (c :: acc)
    else match rest with
      | c2 :: rest2 =>
        if c2 == d then findEmphClose d rest2 (c2 :: c :: acc)
        else if acc.isEmpty || isSpace acc.head! then findEmphClose d rest (c :: acc)
        else some (acc.reverse, rest)
      | [] =>
        if acc.isEmpty || isSpace acc.head! then none else some (acc.reverse, [])

/-- Find the `]` that matches an already-consumed `[`. -/
partial def bracketClose (cs : List Char) (depth : Nat := 0) (acc : List Char := []) :
    Option (List Char × List Char) :=
  match cs with
  | [] => none
  | '\\' :: c :: rest => bracketClose rest depth (c :: '\\' :: acc)
  | ']' :: rest =>
    if depth == 0 then some (acc.reverse, rest) else bracketClose rest (depth - 1) (']' :: acc)
  | '[' :: rest => bracketClose rest (depth + 1) ('[' :: acc)
  | c :: rest => bracketClose rest depth (c :: acc)

/-- Find the `)` that closes a link destination. Balanced inner parentheses
are allowed, e.g. `(https://en.wikipedia.org/wiki/Lean_(proof_assistant))`. -/
partial def parenClose (cs : List Char) (depth : Nat := 0) (acc : List Char := []) :
    Option (List Char × List Char) :=
  match cs with
  | [] => none
  | ')' :: rest =>
    if depth == 0 then some (acc.reverse, rest) else parenClose rest (depth - 1) (')' :: acc)
  | '(' :: rest => parenClose rest (depth + 1) ('(' :: acc)
  | c :: rest => parenClose rest depth (c :: acc)

/-- After an opening `[`, parse `label](dest)`. Returns the label, the URL and
the remaining input. An optional title after the URL is ignored. -/
def linkParts (cs : List Char) : Option (List Char × String × List Char) := do
  let (label, after) ← bracketClose cs
  match after with
  | '(' :: rest =>
    let (dest, after2) ← parenClose rest
    let url := (trim dest).takeWhile (fun c => !isSpace c)
    let url := match url with
      | '<' :: u => u.takeWhile (· != '>')
      | u => u
    some (label, String.ofList url, after2)
  | _ => none

/-- Unescape backslash escapes (used for image alt text). -/
def unescape : List Char → List Char
  | '\\' :: c :: rest => if isPunct c then c :: unescape rest else '\\' :: unescape (c :: rest)
  | c :: rest => c :: unescape rest
  | [] => []

def isAlnum (c : Char) : Bool := c.isAlphanum

/-- The inline parser. `buf` collects literal text in reverse and `out` collects
finished inlines in reverse. -/
partial def inlines (cs : List Char) (buf : List Char := []) (out : List Inline := []) :
    List Inline :=
  match cs with
  | [] => (flushBuf buf out).reverse
  -- hard line break: backslash before newline
  | '\\' :: '\n' :: rest => inlines rest [] (.lineBreak :: flushBuf buf out)
  | '\\' :: c :: rest =>
    if isPunct c then inlines rest (c :: buf) out else inlines (c :: rest) ('\\' :: buf) out
  | '\n' :: rest =>
    let sp := (buf.takeWhile (· == ' ')).length
    if sp ≥ 2 then inlines (trimLeft rest) [] (.lineBreak :: flushBuf (buf.drop sp) out)
    else inlines (trimLeft rest) ('\n' :: buf.drop sp) out
  | '`' :: rest =>
    let n := 1 + (rest.takeWhile (· == '`')).length
    let rest := rest.drop (n - 1)
    let delim := List.replicate n '`'
    match splitPlain delim rest with
    | some (inner, after) =>
      let inner := inner.map (fun c => if c == '\n' then ' ' else c)
      -- strip one space on each side when both are present: `` ` `` ` `` → "`"
      let inner := match inner with
        | ' ' :: xs@(_ :: _) => if xs.getLast? == some ' ' then xs.dropLast else inner
        | _ => inner
      inlines after [] (.code (String.ofList inner) :: flushBuf buf out)
    | none => inlines rest (delim ++ buf) out
  | '!' :: '[' :: rest =>
    match linkParts rest with
    | some (label, url, after) =>
      inlines after [] (.image (String.ofList (unescape label)) url :: flushBuf buf out)
    | none => inlines ('[' :: rest) ('!' :: buf) out
  -- a margin note: `^[text]`, its text parsed like any other
  | '^' :: '[' :: rest =>
    match bracketClose rest with
    | some (inner, after) => inlines after [] (.note (inlines inner) :: flushBuf buf out)
    | none => inlines ('[' :: rest) ('^' :: buf) out
  -- a footnote reference: `[^label]`, the label without spaces
  | '[' :: '^' :: rest =>
    let label := rest.takeWhile (fun c => c != ']' && !isSpace c)
    match rest.drop label.length with
    | ']' :: after =>
      if label.isEmpty then inlines rest ('^' :: '[' :: buf) out
      else inlines after [] (.footRef (String.ofList label) :: flushBuf buf out)
    | _ => inlines rest ('^' :: '[' :: buf) out
  | '[' :: rest =>
    match linkParts rest with
    | some (label, url, after) => inlines after [] (.link (inlines label) url :: flushBuf buf out)
    | none => inlines rest ('[' :: buf) out
  | c :: c2 :: rest =>
    -- `***x***`: strong and emphasised at once
    let triple : Option (List Char × List Char) :=
      match rest with
      | c3 :: rest3 =>
        if (c == '*' || c == '_') && c2 == c && c3 == c && rest3.head?.any (!isSpace ·) then
          match splitPlain [c, c, c] rest3 with
          | some (inner, after) =>
            if !inner.isEmpty && !isSpace inner.getLast! then some (inner, after) else none
          | none => none
        else none
      | [] => none
    if let some (inner, after) := triple then
      inlines after [] (.strong [.emph (inlines inner)] :: flushBuf buf out)
    else if (c == '*' || c == '_') && c2 == c then
      let canOpen := (c == '*' || buf.head?.all (!isAlnum ·)) && rest.head?.any (!isSpace ·)
      match (if canOpen then findStrongClose c rest else none) with
      | some (inner, after) => inlines after [] (.strong (inlines inner) :: flushBuf buf out)
      | none => inlines rest (c :: c :: buf) out
    else if c == '*' || c == '_' then
      let canOpen := (c == '*' || buf.head?.all (!isAlnum ·)) && !isSpace c2
      match (if canOpen then findEmphClose c (c2 :: rest) else none) with
      | some (inner, after) =>
        if c == '_' && after.head?.any isAlnum then inlines (c2 :: rest) (c :: buf) out
        else inlines after [] (.emph (inlines inner) :: flushBuf buf out)
      | none => inlines (c2 :: rest) (c :: buf) out
    else inlines (c2 :: rest) (c :: buf) out
  | [c] => inlines [] (c :: buf) out

/-! ## Blocks -/

def heading? (l : List Char) : Option (Nat × List Char) :=
  let l := trimLeft l
  let n := (l.takeWhile (· == '#')).length
  let rest := l.drop n
  if 1 ≤ n && n ≤ 6 && (rest.isEmpty || isSpace rest.head!) then
    -- drop an optional closing sequence of #'s
    let t := trim rest
    let stripped := trimRight (t.reverse.dropWhile (· == '#')).reverse
    let t := if stripped.length < t.length && (stripped.isEmpty || isSpace t[stripped.length]!)
      then stripped else t
    some (n, t)
  else none

def isFence (l : List Char) : Bool := ['`', '`', '`'].isPrefixOf (trimLeft l)

def isHr (l : List Char) : Bool :=
  let t := l.filter (!isSpace ·)
  t.length ≥ 3 && (t.all (· == '-') || t.all (· == '*') || t.all (· == '_'))

def isQuote (l : List Char) : Bool := (trimLeft l).head? == some '>'

def stripQuote (l : List Char) : List Char :=
  match trimLeft l with
  | '>' :: ' ' :: rest => rest
  | '>' :: rest => rest
  | other => other

def ulItem? (l : List Char) : Option (List Char) :=
  match trimLeft l with
  | c :: s :: rest => if "-*+".toList.contains c && isSpace s then some (trimLeft rest) else none
  | _ => none

def olItem? (l : List Char) : Option (Nat × List Char) :=
  let l := trimLeft l
  let digits := l.takeWhile Char.isDigit
  match l.drop digits.length with
  | d :: s :: rest =>
    if 1 ≤ digits.length && digits.length ≤ 9 && (d == '.' || d == ')') && isSpace s then
      some ((String.ofList digits).toNat!, trimLeft rest)
    else none
  | _ => none

/-- A footnote's first line, `[^label]: text`: its label and text. -/
def footDef? (l : List Char) : Option (String × List Char) :=
  match trimLeft l with
  | '[' :: '^' :: rest =>
    let label := rest.takeWhile (fun c => c != ']' && !isSpace c)
    match rest.drop label.length with
    | ']' :: ':' :: text => if label.isEmpty then none else some (String.ofList label, trimLeft text)
    | _ => none
  | _ => none

def startsBlock (l : List Char) : Bool :=
  (footDef? l).isSome || isFence l || (heading? l).isSome || isHr l || isQuote l || (ulItem? l).isSome || (olItem? l).isSome

/-- Collect consecutive list items. An item continues over following non-blank
lines that don't start a new block. Blank lines between items are allowed. -/
partial def collectItems (item? : List Char → Option (List Char)) :
    List (List Char) → List (List Char) × List (List Char)
  | [] => ([], [])
  | l :: ls =>
    match item? l with
    | none => ([], l :: ls)
    | some first =>
      let (cont, rest) := ls.span (fun x => !isBlank x && !startsBlock x)
      let afterBlanks := rest.dropWhile isBlank
      let rest := match afterBlanks with
        | x :: _ => if (item? x).isSome then afterBlanks else rest
        | [] => rest
      let item := trimRight (joinLines (first :: cont.map trimLeft))
      let (items, rest') := collectItems item? rest
      (item :: items, rest')

partial def blocks : List (List Char) → List Block
  | [] => []
  | l :: ls =>
    if isBlank l then blocks ls
    else if isFence l then
      let lang := trim ((trimLeft l).drop 3)
      let (body, rest) := ls.span (fun x => !isFence x)
      let lang := if lang.isEmpty then none else some (String.ofList lang)
      .codeBlock lang (String.ofList (joinLines body)) :: blocks (rest.drop 1)
    else if let some (label, first) := footDef? l then
      -- continued on the indented lines that follow
      let (cont, rest) := ls.span (fun x => !isBlank x && x.head?.any isSpace)
      .footnote label (inlines (trimRight (joinLines (first :: cont.map trimLeft)))) :: blocks rest
    else if let some (n, t) := heading? l then
      .heading n (inlines t) :: blocks ls
    else if isHr l then .hr :: blocks ls
    else if isQuote l then
      let (qs, rest) := (l :: ls).span isQuote
      .quote (blocks (qs.map stripQuote)) :: blocks rest
    else if (ulItem? l).isSome then
      let (items, rest) := collectItems ulItem? (l :: ls)
      .ulist (items.map inlines) :: blocks rest
    else if let some (start, _) := olItem? l then
      let (items, rest) := collectItems (fun x => (olItem? x).map Prod.snd) (l :: ls)
      .olist start (items.map inlines) :: blocks rest
    else
      let (ps, rest) := ls.span (fun x => !isBlank x && !startsBlock x)
      .para (inlines (trimRight (joinLines ((l :: ps).map trimLeft)))) :: blocks rest

def parse (src : String) : List Block := blocks (toLines src)

end Site.Markdown
