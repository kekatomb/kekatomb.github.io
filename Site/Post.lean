import Site.Util
import Site.Markdown.Ast
import Site.Markdown.Parse

/-!
# Blog posts

A post is a Markdown file in `content/posts/`, and its file name (minus `.md`)
becomes its URL slug. The file must start with front matter:

```
---
title: Hello, world
date: 2026-11-14
summary: Optional one-liner shown in listings.
domo: Optional comment Domo makes when you open the post.
draft: true        (optional; drafts are skipped)
---
```
-/

namespace Site

structure Date where
  year : Nat
  month : Nat
  day : Nat
deriving BEq, Repr

namespace Date

def key (d : Date) : Nat := d.year * 10000 + d.month * 100 + d.day

def iso (d : Date) : String :=
  s!"{padLeft 4 (toString d.year)}-{padLeft 2 (toString d.month)}-{padLeft 2 (toString d.day)}"

def ordinalSuffix (n : Nat) : String :=
  if n % 100 ∈ [11, 12, 13] then "th"
  else match n % 10 with
    | 1 => "st"
    | 2 => "nd"
    | 3 => "rd"
    | _ => "th"

def longMonthNames : List String :=
  [ "January", "February", "March", "April", "May", "June", "July"
  , "August", "September", "October", "November", "December" ]

/-- Human-readable date, e.g. `August 23rd, 2026`. -/
def long (d : Date) : String :=
  s!"{longMonthNames[d.month - 1]!} {d.day}{ordinalSuffix d.day}, {d.year}"

def daysIn (year month : Nat) : Nat :=
  let leap := (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
  match month with
  | 2 => if leap then 29 else 28
  | 4 | 6 | 9 | 11 => 30
  | _ => 31

/-- The day of the week, from 0 (Sunday) to 6 (Saturday), by Sakamoto's
method. -/
def weekday (d : Date) : Nat :=
  let t := [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
  let y := if d.month < 3 then d.year - 1 else d.year
  (y + y / 4 - y / 100 + y / 400 + t[d.month - 1]! + d.day) % 7

def parse (s : String) : Except String Date :=
  match s.splitOn "-" with
  | [y, m, d] =>
    match y.toNat?, m.toNat?, d.toNat? with
    | some yn, some mn, some dn =>
      if y.length != 4 || m.length != 2 || d.length != 2 then
        throw s!"date must be YYYY-MM-DD, got '{s}'"
      else if mn < 1 || mn > 12 || dn < 1 || dn > daysIn yn mn then
        throw s!"no such date: '{s}'"
      else pure ⟨yn, mn, dn⟩
    | _, _, _ => throw s!"date must be YYYY-MM-DD, got '{s}'"
  | _ => throw s!"date must be YYYY-MM-DD, got '{s}'"

end Date

structure Post where
  slug : String
  /-- The collection it belongs to, which is also its URL segment:
  `"blog"` (`content/posts/`) or `"articles"` (`content/articles/`). -/
  kind : String := "blog"
  title : String
  date : Date
  summary : Option String
  /-- What Domo says when the post is opened, if anything. -/
  domo : Option String := none
  draft : Bool
  body : List Markdown.Block

def frontMatterKeys : List String := ["title", "date", "summary", "domo", "draft"]

def parsePair (l : List Char) : Except String (String × String) := do
  let key := l.takeWhile (· != ':')
  if key.length == l.length then throw s!"front matter line has no ':': '{String.ofList l}'"
  let value := trim (l.drop (key.length + 1))
  -- strip one pair of surrounding quotes
  let value := match value with
    | q :: rest@(_ :: _) =>
      if (q == '"' || q == '\'') && rest.getLast? == some q then rest.dropLast else value
    | _ => value
  let key := String.ofList (trim key)
  unless frontMatterKeys.contains key do
    throw s!"unknown front matter key '{key}' (known: {", ".intercalate frontMatterKeys})"
  pure (key, String.ofList value)

/-- Split a source file into front-matter pairs and the remaining body lines. -/
def splitFrontMatter (src : String) : Except String (List (String × String) × List (List Char)) := do
  match toLines src with
  | first :: rest =>
    if trim first != "---".toList then throw "file must start with a '---' front matter block"
    let (fm, after) := rest.span (fun l => trim l != "---".toList)
    if after.isEmpty then throw "front matter block is not closed with '---'"
    let pairs ← (fm.filter (!isBlank ·)).mapM parsePair
    pure (pairs, after.drop 1)
  | [] => throw "empty file"

def validSlug (s : String) : Bool :=
  !s.isEmpty && s.toList.all fun c => c.isLower || c.isDigit || c == '-'

def Post.parse (slug : String) (src : String) : Except String Post := do
  unless validSlug slug do
    throw s!"file name '{slug}' must use only a-z, 0-9 and '-' (it becomes the URL)"
  let (pairs, body) ← splitFrontMatter src
  let get? (k : String) := (pairs.find? (·.1 == k)).map (·.2)
  let some title := get? "title" | throw "missing 'title' in front matter"
  let some dateStr := get? "date" | throw "missing 'date' in front matter"
  let date ← Date.parse dateStr
  let draft ← match get? "draft" with
    | none | some "false" => pure false
    | some "true" => pure true
    | some v => throw s!"'draft' must be true or false, got '{v}'"
  pure { slug, title, date, draft, summary := get? "summary", domo := get? "domo",
         body := Markdown.blocks body }

end Site
