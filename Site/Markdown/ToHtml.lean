import Site.Html
import Site.Markdown.Ast
import Site.Markdown.Parse

namespace Site.Markdown
open Site Html

partial def Inline.toHtml : Inline → Html
  | .text s => .text s
  | .emph xs => el "em" [] (xs.map Inline.toHtml)
  | .strong xs => el "strong" [] (xs.map Inline.toHtml)
  | .code s => el "code" [] [txt s]
  | .link label url => el "a" [("href", url)] (label.map Inline.toHtml)
  | .image alt src => el "img" [("src", src), ("alt", alt)]
  | .lineBreak => el "br"

partial def Block.toHtml : Block → Html
  | .heading n xs => el s!"h{n}" [] (xs.map Inline.toHtml)
  | .para xs => el "p" [] (xs.map Inline.toHtml)
  -- a code block sits in its own container, with a bar naming the language
  -- when the fence gives one (see `.codeblock` in style.css)
  | .codeBlock lang body =>
    let (attrs, bar) := match lang with
      | some l => ([("class", s!"language-{l}")], [el "div" [("class", "codeblock-bar")] [txt l]])
      | none => ([], [])
    el "div" [("class", "codeblock")] (bar ++ [el "pre" [] [el "code" attrs [txt body]]])
  | .quote bs => el "blockquote" [] (bs.map Block.toHtml)
  | .ulist items => el "ul" [] (items.map fun xs => el "li" [] (xs.map Inline.toHtml))
  | .olist start items =>
    let attrs := if start == 1 then [] else [("start", toString start)]
    el "ol" attrs (items.map fun xs => el "li" [] (xs.map Inline.toHtml))
  | .hr => el "hr"

def toHtml (bs : List Block) : List Html := bs.map Block.toHtml

/-! ## Headings with ids, for a table of contents -/

/-- An inline's plain text, without any formatting. -/
partial def Inline.plain : Inline → String
  | .text s | .code s => s
  | .emph xs | .strong xs | .link xs _ => String.join (xs.map Inline.plain)
  | .image alt _ => alt
  | .lineBreak => " "

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
  let (hs, toc, _) := bs.foldl (fun (hs, toc, taken) b =>
    match b with
    | .heading n xs =>
      if 2 ≤ n && n ≤ 4 then
        let title := String.join (xs.map Inline.plain)
        let id := uniqueId (slugify title) taken
        (hs ++ [el s!"h{n}" [("id", id)] (xs.map Inline.toHtml)],
         toc ++ [{ level := n, title, id }], taken ++ [id])
      else (hs ++ [Block.toHtml b], toc, taken)
    | _ => (hs ++ [Block.toHtml b], toc, taken)) (([] : List Html), ([] : List TocEntry), ([] : List String))
  (hs, toc)

/-- Markdown source → HTML string (no document wrapper). Used by tests. -/
def render (src : String) : String := String.join ((toHtml (parse src)).map Html.render)

end Site.Markdown
