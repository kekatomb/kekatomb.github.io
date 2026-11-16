import Site.Html
import Site.Markdown.Ast
import Site.Markdown.Parse

namespace Site.Markdown
open Site Html

/-- An inline's plain text, without any formatting (or notes). -/
partial def Inline.plain : Inline → String
  | .text s | .code s => s
  | .emph xs | .strong xs | .link xs _ => String.join (xs.map Inline.plain)
  | .image alt _ => alt
  | .lineBreak => " "
  | .note _ | .footRef _ => ""

/-! ## Footnotes

A footnote is numbered by where it's first referenced: `[^label]` in the
text becomes a bracketed link, `[1]`, to the footnote's entry in a list at
the end, which links back. (Bracketed, so it isn't mistaken for a margin
note's small raised number.) A reference to a footnote that isn't defined
stays as written; a footnote that's never referenced is left out. -/

/-- Each referenced footnote's label and number. -/
abbrev Footnotes := List (String × Nat)

/-- The fragment ids of a footnote and of (the first) reference to it. -/
def fnId (label : String) : String := s!"fn-{label}"
def fnRefId (label : String) : String := s!"fnref-{label}"

partial def Inline.footRefs : Inline → List String
  | .footRef l => [l]
  | .emph xs | .strong xs | .link xs _ | .note xs => xs.flatMap Inline.footRefs
  | _ => []

partial def Block.footRefs : Block → List String
  | .heading _ xs | .para xs | .footnote _ xs => xs.flatMap Inline.footRefs
  | .quote bs => bs.flatMap Block.footRefs
  | .ulist items | .olist _ items => items.flatMap (·.flatMap Inline.footRefs)
  | _ => []

/-- The document's footnote definitions, in order. -/
def footnoteDefs (bs : List Block) : List (String × List Inline) :=
  bs.filterMap fun | .footnote l xs => some (l, xs) | _ => none

/-- Number the defined footnotes in the order they're first referenced
(references inside footnotes count after the text's). -/
def numberFootnotes (bs : List Block) : Footnotes :=
  let defined := (footnoteDefs bs).map (·.1)
  let refs := bs.flatMap Block.footRefs |>.filter defined.contains
  let firsts := refs.foldl (fun acc l => if acc.contains l then acc else acc ++ [l]) []
  firsts.zipIdx.map fun (l, i) => (l, i + 1)

/-- A margin note's invisible copy (see `.note` below): its text and images,
which take up the same room, but no links (nothing to tab to) and no alt
text (nothing to read out). -/
partial def Inline.room : Inline → List Html
  | .text s | .code s => [txt s]
  | .emph xs | .strong xs | .link xs _ => xs.flatMap Inline.room
  | .image _ src => [el "img" [("src", src), ("alt", "")]]
  | .lineBreak => [el "br"]
  | .footRef _ => [txt "[0]"]
  | .note _ => []

partial def Inline.toHtml (fns : Footnotes) : Inline → Html
  | .text s => .text s
  | .emph xs => el "em" [] (xs.map (Inline.toHtml fns))
  | .strong xs => el "strong" [] (xs.map (Inline.toHtml fns))
  | .code s => el "code" [] [txt s]
  | .link label url => el "a" [("href", url)] (label.map (Inline.toHtml fns))
  | .image alt src => el "img" [("src", src), ("alt", alt)]
  | .lineBreak => el "br"
  -- a margin note: a numbered mark in the text, and the note itself, which
  -- the stylesheet floats into the post's right-hand column (`.sidenote`).
  -- Beside the note's text is an empty cover, which hides the rest of an
  -- earlier note that runs on below this one. After the note is an
  -- invisible copy of it (`Inline.room`), which keeps room in the post for
  -- the whole note, images included. The numbers are CSS counters,
  -- so the mark is hidden from screen readers, which read the note in place.
  | .note xs => el "span" [("class", "note-anchor")] [
      el "span" [("class", "note-ref"), ("aria-hidden", "true")] [],
      el "small" [("class", "sidenote")] [
        el "span" [("class", "note-cover"), ("aria-hidden", "true")] [],
        el "span" [("class", "sidenote-text")] (xs.map (Inline.toHtml fns))
      ],
      el "small" [("class", "note-room"), ("aria-hidden", "true")] (xs.flatMap Inline.room)
    ]
  | .footRef l => match fns.lookup l with
    | some n => el "a" [("href", s!"#{fnId l}"), ("class", "footnote-ref"),
        ("aria-label", s!"Footnote {n}")] [txt s!"[{n}]"]
    | none => .text s!"[^{l}]"

partial def Block.toHtml (fns : Footnotes) : Block → Html
  | .heading n xs => el s!"h{n}" [] (xs.map (Inline.toHtml fns))
  | .para xs => el "p" [] (xs.map (Inline.toHtml fns))
  -- a code block sits in its own container, with a bar naming the language
  -- when the fence gives one (see `.codeblock` in style.css)
  | .codeBlock lang body =>
    let (attrs, bar) := match lang with
      | some l => ([("class", s!"language-{l}")], [el "div" [("class", "codeblock-bar")] [txt l]])
      | none => ([], [])
    el "div" [("class", "codeblock")] (bar ++ [el "pre" [] [el "code" attrs [txt body]]])
  | .quote bs => el "blockquote" [] (bs.map (Block.toHtml fns))
  | .ulist items => el "ul" [] (items.map fun xs => el "li" [] (xs.map (Inline.toHtml fns)))
  | .olist start items =>
    let attrs := if start == 1 then [] else [("start", toString start)]
    el "ol" attrs (items.map fun xs => el "li" [] (xs.map (Inline.toHtml fns)))
  | .hr => el "hr"
  -- footnotes are gathered at the end (`footnoteList`), not shown in place
  | .footnote _ _ => .text ""

/-- The footnotes, numbered, each linking back to where it's referenced. -/
def footnoteList (bs : List Block) (fns : Footnotes) : List Html :=
  if fns.isEmpty then [] else
  let defs := footnoteDefs bs
  [el "section" [("class", "footnotes"), ("aria-label", "Footnotes")] [
    el "ol" [] <| fns.map fun (l, n) =>
      el "li" [("id", fnId l)] ((defs.lookup l |>.getD []).map (Inline.toHtml fns) ++ [
        txt " ",
        el "a" [("href", s!"#{fnRefId l}"), ("class", "footnote-back"),
          ("aria-label", s!"Back to the text of footnote {n}")] [txt "↩"]
      ])
  ]]

/-- Give the first reference to each footnote the id its back link points
to. (Later references to the same footnote have none, so ids stay unique.) -/
partial def markFirstRefs : Html → StateM (List String) Html
  | .text s => pure (.text s)
  | .script src data => pure (.script src data)
  | .elem tag attrs children => do
    let attrs ← if tag == "a" && attrs.lookup "class" == some "footnote-ref" then
        match (attrs.lookup "href").map (·.drop 4 |>.toString) with  -- after "#fn-"
        | some l =>
          if (← get).contains l then pure attrs
          else do modify (· ++ [l]); pure (attrs ++ [("id", fnRefId l)])
        | none => pure attrs
      else pure attrs
    return .elem tag attrs (← children.mapM markFirstRefs)

/-- Render a document's blocks, then its footnotes. -/
def withFootnotes (bs : List Block) (render : Footnotes → List Html) : List Html :=
  let fns := numberFootnotes bs
  ((render fns ++ footnoteList bs fns).mapM markFirstRefs).run' []

def toHtml (bs : List Block) : List Html := withFootnotes bs fun fns => bs.map (Block.toHtml fns)

/-! ## Headings with ids, for a table of contents -/

/-- A URL fragment made from heading text: lowercase letters and digits,
everything else turned into single dashes. "Links, code & images" becomes
`links-code-images`. -/
def slugify (s : String) : String :=
  let cs := s.toList.map fun c => if c.isAlphanum then c.toLower else '-'
  -- collapse runs of dashes and trim them from both ends
  let collapsed := cs.foldr (fun c acc => match acc with
    | '-' :: _ => if c == '-' then acc else c :: acc
    | _ => c :: acc) []
  let trimmed := (collapsed.dropWhile (· == '-')).reverse.dropWhile (· == '-') |>.reverse
  if trimmed.isEmpty then "section" else String.ofList trimmed

/-- `base`, or `base-2`, `base-3`, … whichever isn't taken yet. -/
def uniqueId (base : String) (taken : List String) : String :=
  if !taken.contains base then base else go 2 (taken.length + 2)
where
  go (k fuel : Nat) : String :=
    match fuel with
    | 0 => s!"{base}-{k}"
    | fuel + 1 => if taken.contains s!"{base}-{k}" then go (k + 1) fuel else s!"{base}-{k}"

/-- One table-of-contents entry: a heading's level, text and id. -/
structure TocEntry where
  level : Nat
  title : String
  id : String
deriving BEq, Repr

/-- Like `toHtml`, but the document's own section headings (levels 2–4, not
those inside quotes) get ids, and are also returned, in order, for a table of
contents. -/
def toHtmlWithToc (bs : List Block) : List Html × List TocEntry :=
  let fns := numberFootnotes bs
  let (hs, toc, _) := bs.foldl (fun (hs, toc, taken) b =>
    match b with
    | .heading n xs =>
      if 2 ≤ n && n ≤ 4 then
        let title := String.join (xs.map Inline.plain)
        let id := uniqueId (slugify title) taken
        (hs ++ [el s!"h{n}" [("id", id)] (xs.map (Inline.toHtml fns))],
         toc ++ [{ level := n, title, id }], taken ++ [id])
      else (hs ++ [Block.toHtml fns b], toc, taken)
    | _ => (hs ++ [Block.toHtml fns b], toc, taken)) (([] : List Html), ([] : List TocEntry), ([] : List String))
  (withFootnotes bs (fun _ => hs), toc)

/-- Markdown source → HTML string (no document wrapper). Used by tests. -/
def render (src : String) : String := String.join ((toHtml (parse src)).map Html.render)

end Site.Markdown
