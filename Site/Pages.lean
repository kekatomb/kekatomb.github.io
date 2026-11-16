import Site.Html
import Site.Post
import Site.Markdown.ToHtml
import Site.LastFm
import Site.GitHub

/-! # Page templates

Every page has two panes:

* **Left** (always visible), in two parts:
  1. the head (`.pane-head`): the pixel Domo, the "be back later" note and a
     sitemap of the top-level pages;
  2. the lists (`.pane-lists`): a container for each collection (Blog,
     Articles), a header bar linking to its index above a scrollable list of
     its entries with their dates (the current page is marked), and a dark
     container with social link icons, pinned to the bottom.
  On phones the head goes above the page and the lists below it.
* **Right**: breadcrumbs (on every page but home), then the page's own content.

Styling lives in `static/style.css`. -/

namespace Site.Pages
open Site Html

def siteName : String := "lindg.re"

def postUrl (p : Post) : String := s!"/{p.kind}/{p.slug}/"

/-- A collection of dated entries: the blog (personal thoughts) or the
articles (formal engineering and research pieces). `key` is its URL segment
and `dir` its folder under `content/`. -/
structure Collection where
  key : String
  title : String
  dir : String
  /-- Shown when the collection is empty. -/
  empty : String
  entries : List Post := []

def blog : Collection :=
  { key := "blog", title := "Blog", dir := "posts", empty := "No posts yet." }
def articles : Collection :=
  { key := "articles", title := "Articles", dir := "articles", empty := "No articles yet." }

/-- Site-wide data every page needs: the collections, with their entries. -/
structure Ctx where
  blog : Collection := Pages.blog
  articles : Collection := Pages.articles

def Ctx.collections (ctx : Ctx) : List Collection := [ctx.blog, ctx.articles]

/-- The pixel Domo in the top-left: his frames stacked, for the stylesheet to
switch between (see `Site/Domo.lean` and `.domo` in style.css). Only the first
frame carries alt text, so screen readers announce him once.

`line` is what he says when the page loads, in a speech bubble; the "zzz" is
for when he falls asleep. Both are hidden from screen readers. -/
def domo (line : Option String) : Html :=
  let frames := ["open", "shut", "wide", "sleep"].map fun name =>
    el "img" [("src", s!"/img/domo-{name}.svg"), ("class", name),
      ("alt", if name == "open" then "lindgren" else ""), ("width", "36"), ("height", "42")]
  let speech := match line with
    | some l => [el "span" [("class", "speech"), ("aria-hidden", "true")] [txt l]]
    | none => []
  el "span" [("class", "domo")] <|
    frames ++ speech ++ [el "span" [("class", "zzz"), ("aria-hidden", "true")] [txt "zzz"]]

/-- Social links in the left pane's dark container, as `(label, href, icon)`.
The icon is an SVG in `static/icons/`; the label is its alt text and tooltip. -/
def socialLinks : List (String × String × String) :=
  [ ("GitHub", "https://github.com/kekatomb", "/icons/github.svg")
  , ("LinkedIn", "https://www.linkedin.com/in/philip-lindgren/", "/icons/linkedin.svg") ]

/-- The sitemap's top-level pages, as `(label, href)`. The blog and articles
have their own containers below it (`collectionIndex`). -/
def sitemapPages : List (String × String) :=
  [("Home", "/")]

/-- One sitemap link; the page we're on (`here`) is marked as current. -/
def sitemapLink (here label href : String) : Html :=
  if href == here then
    el "a" [("href", href), ("class", "current"), ("aria-current", "page")] [txt label]
  else link href [txt label]

/-- The sitemap. `here` is the current page's path. -/
def sitemap (here : String) : Html :=
  el "nav" [("class", "sitemap")] [
    el "ul" [] <| sitemapPages.map fun (label, href) => el "li" [] [sitemapLink here label href]
  ]

def dateTag (d : Date) : Html :=
  el "time" [("datetime", d.iso)] [txt d.long]

/-- A collection's container in the left pane: a header bar linking to its
index page, above a scrollable list of every entry, newest first, with its
date. The page at `here` is marked as current. -/
def collectionIndex (here : String) (c : Collection) : Html :=
  el "nav" [("class", "post-index"), ("aria-label", c.title)] [
    el "div" [("class", "post-index-bar")] [sitemapLink here c.title s!"/{c.key}/"],
    el "div" [("class", "post-index-list")] [
      if c.entries.isEmpty then el "p" [("class", "note")] [txt c.empty]
      else el "ul" [] <| c.entries.map fun p =>
        el "li" [] [sitemapLink here p.title (postUrl p), el "br",
          el "span" [("class", "note")] [dateTag p.date]]
    ]
  ]

/-- The left pane, for the page at `here`. `line` is what Domo says there. -/
def leftPane (ctx : Ctx) (here : String) (line : Option String) : Html :=
  el "aside" [("class", "pane-left")] [
    el "div" [("class", "pane-head")] [
      el "section" [("class", "intro")] [
        el "p" [] [domo line],
        el "p" [] [
          txt "My site will be back, sometime.", el "br",
          el "span" [("class", "note")] [txt "Don't bet your life on it."]
        ]
      ],
      el "section" [("class", "context")] [sitemap here]
    ],
    el "div" [("class", "pane-lists")] [
      collectionIndex here ctx.blog,
      collectionIndex here ctx.articles,
      el "section" [("class", "social")] [
        el "ul" [] <| socialLinks.map fun (label, href, icon) =>
          el "li" [] [
            el "a" [("href", href), ("title", label), ("class", "tile")] [
              el "img" [("src", icon), ("alt", label), ("width", "16"), ("height", "16")]
            ]
          ]
      ]
    ]
  ]

/-- Breadcrumbs above the page: the site's name (linking home), then each
step of `trail` as `(label, href)`. The last step is the page itself and is
not a link. Nothing on the home page (an empty trail). -/
def breadcrumbs (trail : List (String × Option String)) : List Html :=
  if trail.isEmpty then [] else
  let step : String × Option String → Html
    | (label, some href) => el "li" [] [link href [txt label]]
    | (label, none) => el "li" [] [el "span" [] [txt label]]
  [el "nav" [("class", "breadcrumbs"), ("aria-label", "Breadcrumbs")] [
    el "ol" [] (step (siteName, some "/") :: trail.map step)
  ]]

/-- The full HTML document for the page at path `here`: the left pane, then
`body` in the right pane under the breadcrumbs for `trail`, then `side` (the
table of contents, if any) in a third column. `line` is what Domo says on this
page, if anything. -/
def layout (ctx : Ctx) (here : String) (pageTitle : Option String)
    (body : List Html) (line : Option String := none) (side : List Html := [])
    (trail : List (String × Option String) := []) : String :=
  let t := match pageTitle with
    | some pt => s!"{pt} — {siteName}"
    | none => siteName
  document <| el "html" [("lang", "en")] [
    el "head" [] [
      el "meta" [("charset", "utf-8")],
      el "meta" [("name", "viewport"), ("content", "width=device-width, initial-scale=1")],
      el "title" [] [txt t],
      el "link" [("rel", "stylesheet"), ("href", "/style.css")]
    ],
    el "body" [] ([
      leftPane ctx here line,
      el "main" [("class", "pane-right")] (breadcrumbs trail ++ body)
    ] ++ side)
  ]

/-- A collection's entries as a forum's thread list: a header row naming the
columns, then one row per entry with its title and summary beside its date. -/
def postList (c : Collection) : Html :=
  el "div" [("class", "threads")] [
    el "div" [("class", "threads-head"), ("aria-hidden", "true")] [
      el "span" [] [txt "Title"], el "span" [] [txt "Date"]
    ],
    if c.entries.isEmpty then el "p" [("class", "note")] [txt c.empty]
    else el "ul" [] <| c.entries.map fun p =>
      let summary := match p.summary with
        | some s => [el "br", el "span" [("class", "note")] [txt s]]
        | none => []
      el "li" [] [
        el "div" [] ([link (postUrl p) [txt p.title]] ++ summary),
        el "span" [("class", "note")] [dateTag p.date]
      ]
  ]

/-- An image with the site's two-tone edge (see `.bordered` in style.css). Pass
the image's pixel size when known, so the page doesn't jump as it loads. -/
def borderedImage (src alt : String) (size : Option (Nat × Nat) := none) : Html :=
  let dims := match size with
    | some (w, h) => [("width", toString w), ("height", toString h)]
    | none => []
  el "p" [] [el "span" [("class", "bordered")] [el "img" ([("src", src), ("alt", alt)] ++ dims)]]

/-- One news item: its date and its Markdown body. -/
structure NewsItem where
  date : Date
  body : List Markdown.Block

/-- The news list: newest first, each item's date beside its text. -/
def newsSection (items : List NewsItem) : List Html :=
  if items.isEmpty then [] else [
    el "section" [("class", "news")] [
      el "h2" [] [txt "News"],
      el "ul" [] <| items.map fun n =>
        el "li" [] [
          el "span" [("class", "note")] [dateTag n.date],
          el "div" [] (Markdown.toHtml n.body)
        ]
    ]
  ]

/-- Recently played tracks, in a dark container like the blog's: a header
bar linking to the Last.fm profile, above the tracks. `now` (seconds since
1970) is when the site was built, for "3 hours ago". Nothing at all when the
tracks couldn't be fetched (no API key, or Last.fm unreachable); a note when
Last.fm answered but had no tracks. -/
def listeningSection (listening : Option (List LastFm.Track × Nat)) : List Html :=
  match listening with
  | none => []
  | some (tracks, now) => [
    el "section" [("class", "post-index panel listening dark"), ("aria-label", "Recently played")] [
      el "div" [("class", "post-index-bar")] [link LastFm.profileUrl [txt "Recently played"]],
      el "div" [("class", "post-index-list")] [
        if tracks.isEmpty then el "p" [("class", "note")] [txt "Nothing played recently."]
        else el "ul" [] <| tracks.map fun t =>
          let cover := match t.image with
            | some src => [el "img" [("src", src), ("alt", ""), ("width", "48"), ("height", "48")]]
            | none => []
          el "li" [] [
            el "span" [("class", "cover")] cover,
            el "span" [] [
              link t.url [txt t.name], el "br",
              txt t.artist, el "br",
              el "span" [("class", "note")] [txt (LastFm.ago now t)]
            ]
          ]
      ]
    ]
  ]

/-- The "now" line: plain text naming the most recently updated projects and
solutions (at most three of each), e.g. "Lately I've been working on this
site, lean, and cryptography, and writing up solutions to …". Either part is
left out when it has nothing to name. -/
def nowLine (recent : List GitHub.Repo) : List Html :=
  let (projects, solutions) := GitHub.nowParts recent
  let names (rs : List GitHub.Repo) :=
    GitHub.joinAnd (rs.map fun r => link r.url [txt (GitHub.nowLabel r)]) (txt ", ") (txt " and ") (txt ", and ")
  let sentence := match projects.isEmpty, solutions.isEmpty with
    | true, true => []
    | false, true => [txt "Lately I've been working on "] ++ names projects
    | true, false => [txt "Lately I've been writing up solutions to "] ++ names solutions
    | false, false =>
      [txt "Lately I've been working on "] ++ names projects ++
      [txt ", and writing up solutions to "] ++ names solutions
  if sentence.isEmpty then [] else [el "p" [("class", "now")] (sentence ++ [txt "."])]

/-- Pinned repositories, in the same dark container as Recently played. Nothing
when they couldn't be fetched (no token); a note when nothing is pinned. -/
def projectsSection (pinned : Option (List GitHub.Repo)) : List Html :=
  match pinned with
  | none => []
  | some repos => [
    el "section" [("class", "post-index panel projects"), ("aria-label", "Projects")] [
      el "div" [("class", "post-index-bar")] [link GitHub.profileUrl [txt "Projects"]],
      el "div" [("class", "post-index-list")] [
        if repos.isEmpty then el "p" [("class", "note")] [txt "Nothing pinned yet."]
        else el "ul" [] <| repos.map fun r =>
          let details := (r.language.toList ++ (if r.stars > 0 then [s!"★ {r.stars}"] else []))
          el "li" [] <|
            [link r.url [txt r.name]] ++
            (match r.description with | some d => [el "br", txt d] | none => []) ++
            (if details.isEmpty then [] else
              [el "br", el "span" [("class", "note")] [txt (" · ".intercalate details)]])
      ]
    ]
  ]

/-- Everything the home page shows besides the left pane. -/
structure Home where
  /-- The intro, from `content/intro.md`. -/
  intro : List Markdown.Block := []
  /-- The most recently updated repositories, for the "now" line. -/
  recent : List GitHub.Repo := []
  /-- Pinned repositories; `none` when they couldn't be fetched. -/
  pinned : Option (List GitHub.Repo) := none
  news : List NewsItem := []
  /-- Recent Last.fm tracks and the build time; `none` when not fetched. -/
  listening : Option (List LastFm.Track × Nat) := none

/-- The home page: the intro and the "now" line, the news, and the projects
and recently played side by side. -/
def home (ctx : Ctx) (h : Home := {}) : String :=
  let panels := projectsSection h.pinned ++ listeningSection h.listening
  layout ctx "/" none <|
    [el "section" [("class", "intro")] (Markdown.toHtml h.intro ++ nowLine h.recent)] ++
    newsSection h.news ++
    (if panels.isEmpty then [] else [el "div" [("class", "panels")] panels])

/-- The table of contents for a page's sections, or nothing when it has
fewer than two. A flat list; each entry's class (`d2`, `d3`, `d4`) is its
heading level, which sets its indent. It goes in the page's third column
(see `layout`). -/
def toc (entries : List Markdown.TocEntry) : List Html :=
  if entries.length < 2 then [] else
  [el "nav" [("class", "toc"), ("aria-label", "Table of contents")] [
    el "div" [("class", "toc-bar")] [txt "On this page"],
    el "ul" [] <| entries.map fun e =>
      el "li" [("class", s!"d{e.level}")] [link s!"#{e.id}" [txt e.title]]
  ]]

/-- A standalone page at `here` whose body is Markdown, e.g. the About page. -/
def page (ctx : Ctx) (here title : String) (body : List Markdown.Block) : String :=
  let (html, entries) := Markdown.toHtmlWithToc body
  layout ctx here (some title) [el "article" [] html] (side := toc entries)

/-- A collection's index page: every entry with its date and summary. -/
def collectionPage (ctx : Ctx) (c : Collection) : String :=
  layout ctx s!"/{c.key}/" (some c.title) [el "h1" [] [txt c.title], postList c]
    (trail := [(c.title, none)])

/-- Comments on posts, by giscus (https://giscus.app): each post's comments
are a GitHub Discussion in this site's repository, found by the post's path,
in the Announcements category (where only giscus and the owner can start
one). Giscus's script is the site's one exception to "no JavaScript" (see
`Html.allowedScripts`); it draws the comments in its own frame, styled by
`static/giscus.css`, which giscus fetches from the live site at `themeUrl`:
from another origin, so the server must allow that (GitHub Pages does), and
from a public address, since browsers won't let giscus.app fetch from your
own machine, so a local preview shows the deployed theme. -/
structure Giscus where
  repo : String := "kekatomb/kekatomb.github.io"
  repoId : String := "R_kgDOVd1fnA"
  category : String := "Announcements"
  categoryId : String := "DIC_kwDOVd1fnM4DKEFl"
  themeUrl : String := s!"https://{siteName}/giscus.css"

/-- The comments under a post: a bar, then the giscus frame in a well. -/
def comments (g : Giscus := {}) : Html :=
  el "section" [("class", "comments"), ("aria-label", "Comments")] [
    el "div" [("class", "comments-bar")] [txt "Comments"],
    el "div" [("class", "giscus")] [
      .script "https://giscus.app/client.js" [
        ("data-repo", g.repo), ("data-repo-id", g.repoId),
        ("data-category", g.category), ("data-category-id", g.categoryId),
        ("data-mapping", "pathname"), ("data-strict", "1"),
        ("data-reactions-enabled", "0"), ("data-emit-metadata", "0"),
        ("data-input-position", "bottom"), ("data-theme", g.themeUrl),
        ("data-lang", "en"), ("data-loading", "lazy")
      ]
    ]
  ]

/-- A post, laid out like the first post of a forum thread: the title, then
a box with the date in its bar, and the body, ending with its footnotes. The
box has a right-hand column for the post's margin notes (`^[…]` in Markdown). -/
def post (ctx : Ctx) (p : Post) : String :=
  let (html, entries) := Markdown.toHtmlWithToc p.body
  let coll := (ctx.collections.find? (·.key == p.kind)).map (·.title) |>.getD p.kind
  layout ctx (postUrl p) (some p.title) [
    el "article" [] [
      el "h1" [] [txt p.title],
      el "div" [("class", "forum-post")] [
        el "div" [("class", "forum-post-bar")] [
          dateTag p.date,
          el "a" [("href", postUrl p), ("title", "Link to this post")] [txt "Permalink"]
        ],
        el "div" [("class", "forum-post-body")] html
      ],
      comments
    ]
  ] (line := p.domo) (side := toc entries) (trail := [(coll, some s!"/{p.kind}/"), (p.title, none)])

end Site.Pages
