import Site.Util

/-!
# HTML tree and renderer

This module is where the "minimal JavaScript" rule lives. Nothing the pages
are built from (Markdown, titles, data from APIs) can add a script:

* There is no raw-HTML node. Every string gets escaped.
* Only tags in `allowedTags` are emitted. Any other element (`script`,
  `iframe`, `object`, …) is dropped and only its children are rendered.
* Only attributes in `allowedAttrs` are emitted, so `on*` event handlers and
  `style` can never appear.
* `href` and `src` values pass through `sanitizeUrl`, which only allows
  relative URLs and the `http`, `https` and `mailto` schemes.

Scripts are explicit: the `script` node, which renders only for a source
listed in `allowedScripts` (the site's own `site.js`, and giscus for
comments on posts). A script from anywhere else renders as nothing. Markdown
can never produce one; only the templates do. The deploy workflow checks the
built site for any other script.
-/

namespace Site

inductive Html where
  | elem (tag : String) (attrs : List (String × String)) (children : List Html)
  | text (s : String)
  /-- A script, from `allowedScripts` only, with its settings as `data-*`
  attributes. -/
  | script (src : String) (data : List (String × String))

instance : Inhabited Html := ⟨.text ""⟩

namespace Html

def allowedTags : List String :=
  [ "html", "head", "body", "meta", "title", "link"
  , "header", "nav", "main", "footer", "article", "section", "aside", "div", "span"
  , "h1", "h2", "h3", "h4", "h5", "h6", "p", "br", "hr"
  , "a", "em", "strong", "code", "pre", "blockquote", "img"
  , "ul", "ol", "li", "dl", "dt", "dd", "time", "small", "noscript" ]

def allowedAttrs : List String :=
  [ "href", "src", "alt", "title", "class", "id", "lang", "charset"
  , "name", "content", "datetime", "rel", "start", "width", "height"
  , "aria-current", "aria-label", "aria-hidden", "hidden" ]

/-- The only scripts the site may load: its own (`static/site.js`, which
refreshes the live parts of the home page), and giscus, for comments on
posts. -/
def allowedScripts : List String := ["/site.js", "https://giscus.app/client.js"]

def voidTags : List String := ["br", "hr", "img", "meta", "link"]

/-- Tags that get a newline after them, so the output stays readable. -/
def blockTags : List String :=
  [ "html", "head", "body", "meta", "title", "link", "header", "nav", "main", "aside"
  , "footer", "article", "section", "div", "h1", "h2", "h3", "h4", "h5", "h6"
  , "p", "pre", "blockquote", "ul", "ol", "li", "dl", "hr", "noscript" ]

def escape (s : String) : String :=
  String.join <| s.toList.map fun
    | '&' => "&amp;"
    | '<' => "&lt;"
    | '>' => "&gt;"
    | '"' => "&quot;"
    | '\'' => "&#39;"
    | c => c.toString

/-- Allow relative URLs and the `http`, `https` and `mailto` schemes.
Replace anything else (`javascript:`, `data:`, …) with `#`. -/
def sanitizeUrl (u : String) : String :=
  -- Browsers ignore whitespace and control characters inside a scheme, so
  -- strip them before looking at it.
  let cs := (u.toList.filter fun c => c.toNat > 32 && c.toNat != 127).map Char.toLower
  let scheme := cs.takeWhile fun c => c != ':' && c != '/' && c != '?' && c != '#'
  match cs.drop scheme.length with
  | ':' :: _ =>
    if ["http", "https", "mailto"].contains (String.ofList scheme) then u else "#"
  | _ => u

def safeAttr : String × String → Option (String × String)
  | (k, v) =>
    let k := k.toLower
    if !allowedAttrs.contains k then none
    else if k == "href" || k == "src" then some (k, sanitizeUrl v)
    else some (k, v)

partial def render : Html → String
  | .text s => escape s
  | .script src data =>
    if !allowedScripts.contains src then "" else
    -- only `data-` attributes, so its settings can't smuggle in handlers
    let as := String.join <| (data.filter (·.1.startsWith "data-")).map fun (k, v) =>
      s!" {k.toLower}=\"{escape v}\""
    -- the site's own script runs once the page is parsed; another site's
    -- loads without sending cookies, whenever it's ready
    if src.startsWith "/" then s!"<script src=\"{src}\"{as} defer></script>\n"
    else s!"<script src=\"{src}\"{as} crossorigin=\"anonymous\" async></script>\n"
  | .elem tag attrs children =>
    let inner := String.join (children.map render)
    let tag := tag.toLower
    if !allowedTags.contains tag then inner
    else
      let as := String.join <| (attrs.filterMap safeAttr).map fun (k, v) =>
        s!" {k}=\"{escape v}\""
      let nl := if blockTags.contains tag then "\n" else ""
      if voidTags.contains tag then s!"<{tag}{as}>{nl}"
      else s!"<{tag}{as}>{inner}</{tag}>{nl}"

def document (root : Html) : String := "<!doctype html>\n" ++ render root

/-! Small builders for the templates. -/

def el (tag : String) (attrs : List (String × String) := []) (children : List Html := []) : Html :=
  .elem tag attrs children

def txt (s : String) : Html := .text s

def link (href : String) (children : List Html) : Html := .elem "a" [("href", href)] children

end Html
end Site
