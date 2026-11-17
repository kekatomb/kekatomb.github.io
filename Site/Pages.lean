import Site.Html
import Site.Post
import Site.Markdown.ToHtml
import Site.LastFm
import Site.GitHub
import Site.Domo

/-! # Page templates

Every page has two panes:

* **Left** (always visible), in two parts, like the column beside a post on
  an old forum:
  1. the head (`.pane-head`): the owner's profile, as a forum's "postbit":
     their name, Domo as the avatar, how many posts, and buttons for their
     other profiles;
  2. the lists (`.pane-lists`): the site's navigation, a well with Home,
     each collection and the guestbook. The collection you're in unfolds to
     list its entries with their dates (the current page is marked). Below
     it, a search box (filled in by `site.js`) and a wall of 88×31 buttons.
  On phones the profile goes above the page and the lists below it.
* **Right**: breadcrumbs (on every page but home), then the page's own
  content. They overlap the navigation, but sit where the eye already is,
  for going up a level.
  The home page's is a board of widgets (see `home`).

Every page loads the site's own script, `static/site.js`, last: Domo's
dialogue, everywhere, and the live parts of the home page (recently played,
and the time in Who's online).

Styling lives in `static/style.css`. -/

namespace Site.Pages
open Site Html

def siteName : String := "lindg.re"

/-- The owner's name, at the top of the profile in the left pane, and the
line under it, in lighter text. -/
def ownerName : String := "lindgren"
def ownerNote : String := "Don't bet your life on it."

/-- The owner's time zone: for the clock on the home page, and for Domo, who
sleeps when it's night there. `site.js` reads it from the script tag. -/
def ownerTimeZone : String := "Europe/Stockholm"


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
  /-- What it holds, under its name on the home page's board. -/
  blurb : String := ""
  entries : List Post := []

def blog : Collection :=
  { key := "blog", title := "Blog", dir := "posts", empty := "No posts yet.",
    blurb := "Personal thoughts." }
def articles : Collection :=
  { key := "articles", title := "Articles", dir := "articles", empty := "No articles yet.",
    blurb := "Engineering and research write-ups." }

/-- Site-wide data every page needs: the collections, with their entries. -/
structure Ctx where
  blog : Collection := Pages.blog
  articles : Collection := Pages.articles

def Ctx.collections (ctx : Ctx) : List Collection := [ctx.blog, ctx.articles]

/-- Something the build fetches from elsewhere (GitHub): not asked for (no
key to ask with), fetched, or failed. A part of the page that's skipped is
left out; one that failed says so (see `status`). -/
inductive Fetched (α : Type) where
  | skipped
  | failed
  | ok (value : α)
  deriving Inhabited

/-- A parse result as fetched: an error is a failure. -/
def Fetched.ofExcept {ε α : Type} : Except ε α → Fetched α
  | .ok a => .ok a
  | .error _ => .failed

/-- The pixel Domo in the top-left: his frames stacked, for the stylesheet to
switch between (see `Site/Domo.lean` and `.domo` in style.css). Only the first
frame carries alt text, so screen readers announce him once.

`line` is what he says when the page loads, in a speech bubble; the "zzz" is
for when he falls asleep. Both are hidden from screen readers. With
JavaScript, `site.js` takes him over: what he says, when he sleeps, and the
frames where his eyes follow the pointer (`right`, `down`, `downright`). -/
def domo (line : Option String) : Html :=
  let frames := (Domo.frames.map (·.1)).map fun name =>
    el "img" [("src", s!"/img/domo-{name}.svg"), ("class", name),
      ("alt", if name == "open" then "lindgren" else ""), ("width", "36"), ("height", "42")]
  let speech := match line with
    | some l => [el "span" [("class", "speech"), ("aria-hidden", "true")] [txt l]]
    | none => []
  el "span" [("class", "domo")] <|
    frames ++ speech ++ [el "span" [("class", "zzz"), ("aria-hidden", "true")] [txt "zzz"]]

/-- The owner's other profiles, as `(label, href, icon)`: buttons in the
profile. The icon is a dark SVG in `static/icons/`, beside the label. -/
def socialLinks : List (String × String × String) :=
  [ ("GitHub", "https://github.com/kekatomb", "/icons/github-dark.svg")
  , ("LinkedIn", "https://www.linkedin.com/in/philip-lindgren/", "/icons/linkedin-dark.svg") ]

/-- A link in the navigation; the page we're on (`here`) is marked as current. -/
def sitemapLink (here label href : String) : Html :=
  if href == here then
    el "a" [("href", href), ("class", "current"), ("aria-current", "page")] [txt label]
  else link href [txt label]

def dateTag (d : Date) : Html :=
  el "time" [("datetime", d.iso)] [txt d.long]

/-- The owner's profile, as the column beside a post on an old forum (its
"postbit"): their name with a line under it, Domo as the avatar, how many
posts they've written, and buttons for their other profiles. `line` is what
Domo says on this page. -/
def profile (ctx : Ctx) (line : Option String) : Html :=
  let posts := (ctx.collections.map (·.entries.length)).foldl (· + ·) 0
  el "section" [("class", "profile"), ("aria-label", "About the owner")] [
    el "p" [("class", "profile-name")] [el "strong" [] [txt ownerName], el "br",
      el "span" [("class", "note")] [txt ownerNote]],
    el "p" [("class", "profile-avatar")] [domo line],
    el "p" [("class", "note")] [txt s!"Posts: {posts}"],
    el "ul" [("class", "profile-links")] <| socialLinks.map fun (label, href, icon) =>
      el "li" [] [
        el "a" [("href", href), ("title", label), ("class", "button icon-button")] [
          el "img" [("src", icon), ("alt", label), ("width", "14"), ("height", "14")]
        ]
      ]
  ]

/-- The site's navigation, for the page at `here`: Home, each collection,
then the guestbook. The collection `here` is in unfolds to list every entry,
newest first, with its date; the others stay folded (on the home page, all
of them: its board lists them). -/
def siteNav (ctx : Ctx) (here : String) : Html :=
  let entries (c : Collection) : List Html :=
    if !here.startsWith s!"/{c.key}/" then [] else [
      if c.entries.isEmpty then el "p" [("class", "note site-nav-entries")] [txt c.empty]
      else el "ul" [("class", "site-nav-entries")] <| c.entries.map fun p =>
        el "li" [] [sitemapLink here p.title (postUrl p), el "br",
          el "span" [("class", "note")] [dateTag p.date]]
    ]
  el "nav" [("class", "site-nav"), ("aria-label", "Site")] [
    el "div" [("class", "site-nav-bar")] [txt "Navigation"],
    el "ul" [] <|
      el "li" [] [sitemapLink here "Home" "/"] ::
      ctx.collections.map (fun c =>
        el "li" [] ([sitemapLink here c.title s!"/{c.key}/"] ++ entries c)) ++
      [el "li" [] [sitemapLink here "Guestbook" "/guestbook/"]]
  ]

/-- A status: Domo as a small pixel icon (see `Domo.statusIcons`) beside a
message, in place of something that can't be shown (yet). `kind` is
`loading` (it's on its way), `failed` (it didn't load) or `noscript` (it
needs JavaScript, which is off).
`site.js` builds the same markup (its `status`) for what fails in the
browser. -/
def status (kind : String) (message : String) : Html :=
  el "div" [("class", s!"status status-{kind}")] [
    el "img" [("src", s!"/img/status-{kind}.svg"), ("alt", ""),
      ("width", toString (12 * Domo.statusScale)), ("height", toString (14 * Domo.statusScale))],
    el "p" [] [txt message]
  ]

/-- A status for when JavaScript is off, which only then shows (it's in a
`<noscript>`). Put it beside what needs JavaScript, and give that the class
`script-only` so the stylesheet hides it then. -/
def noscriptStatus (message : String) : Html :=
  el "noscript" [] [status "noscript" message]

/-- A small well in the left pane, under the navigation: a bar above a body. -/
def sideWell (key title : String) (body : List Html) (hidden := false) : Html :=
  el "section" ([("class", s!"side-well {key}"), ("aria-label", title)] ++
      (if hidden then [("hidden", "")] else [])) [
    el "div" [("class", "side-well-bar")] [txt title],
    el "div" [("class", "side-well-body")] body
  ]

/-- Search. The build writes a stand-in for the box, its size, so nothing
moves when `site.js` puts the real one in its place; it then searches
`/search.json` (see `searchIndex`) as you type. Without JavaScript there's
nothing to search with, so a status says so instead. -/
def searchWidget : Html := sideWell "search" "Search" [
    el "div" [("class", "search-box script-only"), ("aria-hidden", "true")] [
      el "span" [("class", "search-stand-in")] [txt "Search posts…"]],
    noscriptStatus "Turn on JavaScript to search the posts."
  ]

/-- Buttons for the links wall, as `(label, href, image)`: 88×31 images, the
way personal sites linked to each other. This site's own comes first, for
others to copy (`Domo.button`, written to `/img/button-lindgre.svg`). Put
other sites' buttons in `static/buttons/` and list them here. -/
def linkButtons : List (String × String × String) :=
  [ (s!"{siteName}: link here with this button", "/", "/img/button-lindgre.svg") ]

/-- The links wall: each button as an 88×31 image link. -/
def linksWidget : Html :=
  sideWell "links" "Links" [
    el "ul" [] <| linkButtons.map fun (label, href, img) =>
      el "li" [] [el "a" [("href", href), ("title", label)] [
        el "img" [("src", img), ("alt", label), ("width", "88"), ("height", "31")]
      ]]
  ]

/-- The left pane, for the page at `here`. `line` is what Domo says there. -/
def leftPane (ctx : Ctx) (here : String) (line : Option String) : Html :=
  el "aside" [("class", "pane-left")] [
    el "div" [("class", "pane-head")] [profile ctx line],
    el "div" [("class", "pane-lists")] [siteNav ctx here, searchWidget, linksWidget]
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
table of contents, if any) in a third column, then the site's script, with
`scriptData` (and the owner's time zone) as its settings. `line` is what Domo
says on this page, if anything. -/
def layout (ctx : Ctx) (here : String) (pageTitle : Option String)
    (body : List Html) (line : Option String := none) (side : List Html := [])
    (trail : List (String × Option String) := [])
    (scriptData : List (String × String) := []) : String :=
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
    ] ++ side ++ [.script "/site.js" (scriptData ++ [("data-owner-tz", ownerTimeZone)])])
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

/-! ## Home: a board of widgets

The home page is a board of widgets, like an old forum's front page. Each
is a light well headed by a silver bar, in the forum's look. They sit in two
columns, the main one and a narrow side one, and on narrow screens in one,
interleaved (see `.widgets` in style.css). -/

/-- A widget: a bar above a body. `key` names it (class `w-<key>`), which
places it on the board. A hidden one is for `site.js` to fill in and show. -/
def widget (key : String) (bar body : List Html) (hidden := false)
    (label : Option String := none) (extra : String := "") : Html :=
  let cls := s!"widget w-{key}" ++ (if extra.isEmpty then "" else s!" {extra}")
  el "section" ([("class", cls)] ++ label.toList.map (("aria-label", ·)) ++
      (if hidden then [("hidden", "")] else [])) [
    el "div" [("class", "widget-bar")] bar,
    el "div" [("class", "widget-body")] body
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

/-- The welcome: the intro and the "now" line, as the board's sticky post. -/
def welcomeWidget (intro : List Markdown.Block) (recent : List GitHub.Repo) : Html :=
  widget "welcome" [el "span" [("class", "sticky")] [txt "Sticky:"], txt " Welcome"] [
    el "section" [("class", "intro")] (Markdown.toHtml intro ++ nowLine recent)
  ]

/-- One row of the board index: a board, what it holds, how many entries it
has (if known) and its latest entry. -/
structure BoardRow where
  title : String
  href : String
  blurb : String
  count : Option Nat := none
  /-- The latest entry, linked, and its date; nothing when there's none. -/
  latest : Option (Html × Option Date) := none

def Collection.boardRow (c : Collection) : BoardRow :=
  { title := c.title, href := s!"/{c.key}/", blurb := c.blurb, count := some c.entries.length,
    latest := c.entries.head?.map fun p => (link (postUrl p) [txt p.title], some p.date) }

/-- The board index, as on an old forum's front page: each board with its
count and its latest entry. -/
def boardIndex (rows : List BoardRow) : Html :=
  widget "board" [el "span" [] [txt "Board"], el "span" [] [txt "Entries"], el "span" [] [txt "Latest"]] [
    el "ul" [("class", "rows board")] <| rows.map fun r =>
      el "li" [] [
        el "div" [] [link r.href [txt r.title], el "br", el "span" [("class", "note")] [txt r.blurb]],
        el "span" [("class", "count")] [txt ((r.count.map toString).getD "–")],
        el "div" [("class", "latest")] <| match r.latest with
          | some (l, d) => [l] ++ (d.toList.map fun d => [el "br", el "span" [("class", "note")] [dateTag d]]).flatten
          | none => [el "span" [("class", "note")] [txt "Nothing yet."]]
      ]
  ] (label := some "Boards")

/-- The board's rows: the collections. (Projects have their own widget.) -/
def boardRows (ctx : Ctx) : List BoardRow :=
  ctx.collections.map (·.boardRow)

/-- What a widget says when the build couldn't fetch its part from GitHub. -/
def githubFailed : Html :=
  status "failed" "Couldn't reach GitHub when this page was built. It'll try again next time."

/-- GitHub activity: the contribution calendar as a pegboard, a column per
week and a hole per day, lit by how much happened that day. -/
def activityWidget (cal : GitHub.Calendar) : Html :=
  let pegs (w : List GitHub.Day) : List Html :=
    w.map fun d =>
      let s := if d.count == 1 then "" else "s"
      el "span" [("class", s!"peg l{d.level}"), ("title", s!"{d.count} contribution{s} on {d.date.long}")] []
  -- the first week can start mid-week: empty holes before it
  let pad := match cal.weeks.head?.bind (·.head?) with
    | some d => List.replicate d.date.weekday (el "span" [("class", "peg pad")] [])
    | none => []
  let weeks := cal.weeks.zipIdx.map fun (w, i) =>
    el "span" [("class", "week")] ((if i == 0 then pad else []) ++ pegs w)
  let s := if cal.total == 1 then "" else "s"
  widget "activity" [link GitHub.profileUrl [txt "Activity"],
      el "span" [("class", "bar-note")] [txt s!"{cal.total} contribution{s} in the last year"]] [
    el "div" [("class", "pegboard"), ("aria-hidden", "true")] weeks
  ] (label := some "GitHub activity")

/-- One news item: its date and its Markdown body. -/
structure NewsItem where
  date : Date
  body : List Markdown.Block

/-- The news: newest first, each item's date above its text. -/
def newsWidget (items : List NewsItem) : List Html :=
  if items.isEmpty then [] else [
    widget "news" [txt "News"] [
      el "ul" [("class", "rows news")] <| items.map fun n =>
        el "li" [] [
          el "span" [("class", "note")] [dateTag n.date],
          el "div" [] (Markdown.toHtml n.body)
        ]
    ]
  ]

/-- Pinned repositories, as a thread list: each name with its language (and
stars, if any) on the right, its description beneath. Nothing when there's
no token to fetch them with, a status when fetching them failed, and a note
when nothing is pinned. -/
def projectsWidget (pinned : Fetched (List GitHub.Repo)) : List Html :=
  match pinned with
  | .skipped => []
  | .failed => [widget "projects" [link GitHub.profileUrl [txt "Projects"]] [githubFailed]
      (label := some "Projects")]
  | .ok repos => [
    widget "projects" [link GitHub.profileUrl [txt "Projects"]] [
      if repos.isEmpty then el "p" [("class", "note")] [txt "Nothing pinned yet."]
      else el "ul" [("class", "rows")] <| repos.map fun r =>
        let stars := if r.stars == 0 then [] else
          [s!"{r.stars} star" ++ (if r.stars == 1 then "" else "s")]
        let details := r.language.toList ++ stars
        el "li" [] <|
          [el "div" [("class", "proj")] ([link r.url [txt r.name]] ++
            (if details.isEmpty then [] else
              [el "span" [("class", "note")] ((details.map txt).intersperse (el "br"))]))] ++
          (match r.description with | some d => [el "p" [("class", "note")] [txt d]] | none => [])
    ] (label := some "Projects")
  ]

/-- A placeholder line of text for the skeleton: an empty groove, `width`
(a class: `w1` longest to `w3` shortest) as long. -/
def bone (width : String) : Html := el "span" [("class", s!"bone {width}")] []

/-- Recently played, with nothing at all when there's no API key (see
`Site/LastFm.lean`). The build writes it as a skeleton, the size of the real
thing: the latest track's empty cover and grooves for its lines, and four
rows like it. So the page doesn't jump when `site.js` has fetched the tracks
and fills it in (the latest large, the ones before it as rows, the play
count in the bar). Without JavaScript there's nothing to fill it with, so
it says so instead. -/
def listeningWidget (lastfmKey : Option String) : List Html :=
  if lastfmKey.isNone then [] else
  let skeleton (cover : String) (lines : List String) :=
    [el "span" [("class", cover)] [], el "span" [("class", "track-text")] (lines.map bone)]
  [ widget "listening" [link LastFm.profileUrl [txt "Recently played"],
        el "span" [("class", "bar-note")] []] [
      el "div" [("class", "np skeleton script-only"), ("aria-hidden", "true")]
        (skeleton "cover large" ["w1", "w2", "w3"]),
      el "ul" [("class", "rows skeleton script-only"), ("aria-hidden", "true")] <|
        ["w2", "w1", "w2", "w1"].map fun w => el "li" [] (skeleton "cover" [w, "w3", "w3"]),
      noscriptStatus "Turn on JavaScript to see what I've been listening to."
    ] (label := some "Recently played") ]

/-- Who's online, like the bottom of an old forum's front page: you and
Domo. `site.js` adds what's playing, how often you've been here, and, as the
last line, the owner's time ("All times are Stockholm time…"). -/
def onlineWidget : Html :=
  widget "online" [txt "Who's online"] [
    el "ul" [("class", "rows online")] [
      el "li" [] [txt "In here: you and ", el "span" [("class", "online-domo")] [txt "Domo"], txt "."],
      el "li" [("class", "online-listening"), ("hidden", "")] [],
      el "li" [("class", "online-visits"), ("hidden", "")] [],
      el "li" [("class", "online-time note"), ("hidden", "")] []
    ],
    noscriptStatus "Turn on JavaScript to see what's playing, your visits and the time here."
  ] (label := some "Who's online")

/-- Everything the home page shows besides the left pane. -/
structure Home where
  /-- The intro, from `content/intro.md`. -/
  intro : List Markdown.Block := []
  /-- The most recently updated repositories, for the "now" line. -/
  recent : List GitHub.Repo := []
  /-- Pinned repositories (skipped without a GitHub token). -/
  pinned : Fetched (List GitHub.Repo) := .skipped
  /-- The contribution calendar (skipped without a GitHub token). -/
  contributions : Fetched GitHub.Calendar := .skipped
  news : List NewsItem := []
  /-- The Last.fm API key, for `site.js` to fetch recently played in the
  browser. It's read-only, so it's fine in the page. -/
  lastfmKey : Option String := none

/-- The home page: a board of widgets. The main column has the welcome, the
board index, the activity, and the news and projects side by side; the side
column recently played and who's online. -/
def home (ctx : Ctx) (h : Home := {}) : String :=
  let main :=
    [welcomeWidget h.intro h.recent, boardIndex (boardRows ctx)] ++
    (match h.contributions with
      | .skipped => []
      | .failed => [widget "activity" [link GitHub.profileUrl [txt "Activity"]] [githubFailed]
          (label := some "GitHub activity")]
      | .ok cal => [activityWidget cal]) ++
    [el "div" [("class", "widgets-pair")] (newsWidget h.news ++ projectsWidget h.pinned)]
  let side := listeningWidget h.lastfmKey ++ [onlineWidget]
  layout ctx "/" none [
    el "div" [("class", "widgets")] [
      el "div" [("class", "widgets-main")] main,
      el "div" [("class", "widgets-side")] side
    ]
  ] (scriptData := h.lastfmKey.toList.map fun k => ("data-lastfm-key", k))

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
one). Giscus's script is one of the two the site allows (see
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
      -- until giscus's frame has loaded (site.js takes it away, or says it failed)
      el "div" [("class", "giscus-wait script-only")] [status "loading" "Loading the comments…"],
      noscriptStatus "Turn on JavaScript to read and write comments.",
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

/-- The guestbook: a page anyone can leave a note on, as personal sites of
the 2000s had. Its notes are giscus comments, like a post's, in the
discussion for `/guestbook/`. `intro` is from `content/guestbook.md`, if
there is one. -/
def guestbookPage (ctx : Ctx) (intro : List Markdown.Block := []) : String :=
  let body := if intro.isEmpty
    then [el "p" [] [txt "Leave a note: sign in with GitHub to write one."]]
    else Markdown.toHtml intro
  layout ctx "/guestbook/" (some "Guestbook")
    [el "article" [] ([el "h1" [] [txt "Guestbook"]] ++ body ++ [comments])]
    (line := some "sign it. i'll read it.") (trail := [("Guestbook", none)])

/-- The page for an address with nothing at it (`404.html`, which GitHub
Pages serves for any missing page, at that address: so every link on it is
absolute, as everywhere on the site). -/
def notFoundPage (ctx : Ctx) : String :=
  layout ctx "/404.html" (some "Not found") [
    el "article" [] [
      el "h1" [] [txt "Nothing here"],
      status "failed" "There's no page at this address. It may have moved, or the link may be wrong.",
      el "p" [] [link "/" [txt "Go to the home page"], txt ", or use the search or the navigation."]
    ]
  ] (line := some "nothing here. i looked.") (trail := [("Not found", none)])

/-- What the search in the left pane searches (`/search.json`): every entry
of every collection, newest first, with its title, address, summary, date,
collection, and everything it says as plain text (`Markdown.searchText`). -/
def searchIndex (ctx : Ctx) : String :=
  Lean.Json.compress <| Lean.Json.arr <| (ctx.collections.flatMap fun c =>
    c.entries.map fun p => Lean.Json.mkObj [
      ("title", .str p.title), ("url", .str (postUrl p)),
      ("summary", .str (p.summary.getD "")), ("date", .str p.date.long),
      ("section", .str c.title), ("body", .str (Markdown.searchText p.body))]).toArray

end Site.Pages
