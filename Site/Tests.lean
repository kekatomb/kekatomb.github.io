import Site.Html
import Site.Post
import Site.Markdown.ToHtml
import Site.Pages
import Site.Domo
import Site.LastFm
import Site.Env
import Site.GitHub

/-!
# Tests

Each `#guard` is evaluated when this module compiles, so a failing check
makes `lake build` fail.
-/

namespace Site.Tests
open Site Markdown

/-! ## Blocks -/
#guard render "# Hello *world* #" == "<h1>Hello <em>world</em></h1>\n"
#guard render "###### six" == "<h6>six</h6>\n"
-- margin notes: `^[…]`, parsed like any inline text, links included
#guard render "a^[see *this* and [that](/x)] b" ==
  "<p>a<span class=\"note-anchor\"><span class=\"note-ref\" aria-hidden=\"true\"></span><small class=\"sidenote\"><span class=\"note-cover\" aria-hidden=\"true\"></span><span class=\"sidenote-text\">see <em>this</em> and <a href=\"/x\">that</a></span></small><small class=\"note-room\" aria-hidden=\"true\">see this and that</small></span> b</p>\n"
#guard render "2^[unclosed" == "<p>2^[unclosed</p>\n"
#guard render "x^2 and [y](/z)" == "<p>x^2 and <a href=\"/z\">y</a></p>\n"
-- footnotes: bracketed links, numbered by first reference, listed at the end
-- with links back; the first reference to each carries the back link's target
#guard render "b[^z] a[^y] c[^z]\n\n[^y]: Why.\n[^z]: Zed\n  continued." ==
  "<p>b<a href=\"#fn-z\" class=\"footnote-ref\" aria-label=\"Footnote 1\" id=\"fnref-z\">[1]</a> " ++
  "a<a href=\"#fn-y\" class=\"footnote-ref\" aria-label=\"Footnote 2\" id=\"fnref-y\">[2]</a> " ++
  "c<a href=\"#fn-z\" class=\"footnote-ref\" aria-label=\"Footnote 1\">[1]</a></p>\n" ++
  "<section class=\"footnotes\" aria-label=\"Footnotes\"><ol><li id=\"fn-z\">Zed\ncontinued. " ++
  "<a href=\"#fnref-z\" class=\"footnote-back\" aria-label=\"Back to the text of footnote 1\">↩</a></li>\n" ++
  "<li id=\"fn-y\">Why. " ++
  "<a href=\"#fnref-y\" class=\"footnote-back\" aria-label=\"Back to the text of footnote 2\">↩</a></li>\n" ++
  "</ol>\n</section>\n"
-- undefined: left as written; unreferenced: left out; no footnotes, no list
#guard render "a[^nope]\n\n[^unused]: x" == "<p>a[^nope]</p>\n"
#guard render "[^ x] and [^]" == "<p>[^ x] and [^]</p>\n"
-- an image in a note: shown in the note, and taking up room in its copy
#guard render "a^[![A cat](/c.png)]" ==
  "<p>a<span class=\"note-anchor\"><span class=\"note-ref\" aria-hidden=\"true\"></span><small class=\"sidenote\"><span class=\"note-cover\" aria-hidden=\"true\"></span><span class=\"sidenote-text\"><img src=\"/c.png\" alt=\"A cat\"></span></small><small class=\"note-room\" aria-hidden=\"true\"><img src=\"/c.png\" alt=\"\"></small></span></p>\n"
-- a note in a heading stays out of its id and the table of contents
#guard (toHtmlWithToc (parse "## One^[n]")).2.map (·.id) == ["one"]
#guard render "####### seven" == "<p>####### seven</p>\n"
#guard render "one\ntwo\n\nthree" == "<p>one\ntwo</p>\n<p>three</p>\n"
#guard render "```lean\ndef x := 1 < 2\n```" ==
  "<div class=\"codeblock\"><div class=\"codeblock-bar\">lean</div>\n<pre><code class=\"language-lean\">def x := 1 &lt; 2</code></pre>\n</div>\n"
#guard render "```\n# not a heading\n\n*raw*\n```" ==
  "<div class=\"codeblock\"><pre><code># not a heading\n\n*raw*</code></pre>\n</div>\n"
#guard render "> quote\n> # head" == "<blockquote><p>quote</p>\n<h1>head</h1>\n</blockquote>\n"
#guard render "- a\n- b\n  more\n\n- c" == "<ul><li>a</li>\n<li>b\nmore</li>\n<li>c</li>\n</ul>\n"
#guard render "3. x\n4. y" == "<ol start=\"3\"><li>x</li>\n<li>y</li>\n</ol>\n"
#guard render "1. x" == "<ol><li>x</li>\n</ol>\n"
#guard render "---" == "<hr>\n"
#guard render "* * *\n* item" == "<hr>\n<ul><li>item</li>\n</ul>\n"
#guard render "para\n# heading" == "<p>para</p>\n<h1>heading</h1>\n"

/-! ## Inlines -/
#guard render "*a **b** c*" == "<p><em>a <strong>b</strong> c</em></p>\n"
#guard render "_x_ and __y__" == "<p><em>x</em> and <strong>y</strong></p>\n"
#guard render "a ***b*** c" == "<p>a <strong><em>b</em></strong> c</p>\n"
#guard render "***b*** then **c *d* e**" ==
  "<p><strong><em>b</em></strong> then <strong>c <em>d</em> e</strong></p>\n"
#guard render "snake_case_name" == "<p>snake_case_name</p>\n"
#guard render "a * b * c" == "<p>a * b * c</p>\n"
#guard render "unclosed *star and [bracket" == "<p>unclosed *star and [bracket</p>\n"
#guard render "\\*not em\\*" == "<p>*not em*</p>\n"
#guard render "`a < b` and `` x`y ``" == "<p><code>a &lt; b</code> and <code>x`y</code></p>\n"
#guard render "`*no em*`" == "<p><code>*no em*</code></p>\n"
#guard render "a  \nb" == "<p>a<br>b</p>\n"
#guard render "a\\\nb" == "<p>a<br>b</p>\n"
#guard render "[x *y*](https://e.org \"title\")" == "<p><a href=\"https://e.org\">x <em>y</em></a></p>\n"
#guard render "[w](https://en.wikipedia.org/wiki/Lean_(proof_assistant))" ==
  "<p><a href=\"https://en.wikipedia.org/wiki/Lean_(proof_assistant)\">w</a></p>\n"
#guard render "![alt text](/img.png)" == "<p><img src=\"/img.png\" alt=\"alt text\"></p>\n"

/-! ## No JavaScript, ever -/
#guard render "<script>alert(1)</script>" == "<p>&lt;script&gt;alert(1)&lt;/script&gt;</p>\n"
#guard render "<img src=x onerror=alert(1)>" == "<p>&lt;img src=x onerror=alert(1)&gt;</p>\n"
#guard render "[a](javascript:alert(1))" == "<p><a href=\"#\">a</a></p>\n"
#guard render "[a](JaVaScRiPt:alert(1))" == "<p><a href=\"#\">a</a></p>\n"
#guard render "[a](data:text/html,hi)" == "<p><a href=\"#\">a</a></p>\n"
#guard render "![a](vbscript:x)" == "<p><img src=\"#\" alt=\"a\"></p>\n"
#guard render "[a](\"onmouseover=\"x)" == "<p><a href=\"&quot;onmouseover=&quot;x\">a</a></p>\n"
#guard render "[m](mailto:me@example.org) [r](../up)" ==
  "<p><a href=\"mailto:me@example.org\">m</a> <a href=\"../up\">r</a></p>\n"
#guard Html.render (.elem "script" [] [.text "alert(1)"]) == "alert(1)"
-- the one exception: giscus's script, and only with `data-` settings
#guard Html.render (.script "https://evil.example/x.js" [("data-a", "b")]) == ""
#guard Html.render (.script "https://giscus.app/client.js" [("data-a", "\"x"), ("onload", "y")]) ==
  "<script src=\"https://giscus.app/client.js\" data-a=\"&quot;x\" crossorigin=\"anonymous\" async></script>\n"
#guard Html.render (.elem "p" [("onclick", "x"), ("style", "y"), ("id", "z")] []) ==
  "<p id=\"z\"></p>\n"
#guard Html.sanitizeUrl " java\tscript:x" == "#"

/-! ## Posts -/
#guard (Date.parse "2024-02-29").toOption == some ⟨2024, 2, 29⟩
#guard (Date.parse "2023-02-29").toOption.isNone
#guard (Date.parse "2023-13-01").toOption.isNone
#guard (Date.parse "23-1-1").toOption.isNone
#guard (Date.parse "2026-11-14").toOption.map Date.iso == some "2026-11-14"

def samplePost := "---\ntitle: \"Hi: there\"\ndate: 2026-11-14\n---\n# Body"
#guard (Post.parse "hello" samplePost).toOption.map (·.title) == some "Hi: there"
#guard (Post.parse "hello" samplePost).toOption.map (·.draft) == some false
#guard (Post.parse "Bad Slug" samplePost).toOption.isNone
#guard (Post.parse "x" "no front matter").toOption.isNone
#guard (Post.parse "x" "---\ntitle: t\n---\n").toOption.isNone  -- missing date
#guard (Post.parse "x" "---\ntitle: t\ndate: 2026-01-01\ntitel: typo\n---\n").toOption.isNone

#guard Date.long ⟨2026, 8, 23⟩ == "August 23rd, 2026"
#guard [1, 2, 3, 4, 11, 12, 13, 21, 22, 31].map (fun d => Date.long ⟨2026, 1, d⟩) ==
  [ "January 1st, 2026", "January 2nd, 2026", "January 3rd, 2026", "January 4th, 2026"
  , "January 11th, 2026", "January 12th, 2026", "January 13th, 2026"
  , "January 21st, 2026", "January 22nd, 2026", "January 31st, 2026" ]

/-! ## Layout -/
def samplePostValue : Post :=
  { slug := "x", title := "X", date := ⟨2026, 1, 1⟩, summary := none, draft := false, body := [] }
def sampleArticle : Post := { samplePostValue with slug := "y", title := "Y", kind := "articles" }
def sampleCtx : Pages.Ctx :=
  { blog := { Pages.blog with entries := [samplePostValue] },
    articles := { Pages.articles with entries := [sampleArticle] } }
def blogPage := Pages.collectionPage sampleCtx sampleCtx.blog
def homePage := Pages.home sampleCtx
def postPage := Pages.post sampleCtx samplePostValue
#guard (blogPage.splitOn "<link rel=\"stylesheet\" href=\"/style.css\">").length == 2
-- no RSS anywhere
#guard [homePage, blogPage, postPage].all fun pg => (pg.splitOn "feed.xml").length == 1
#guard (homePage.splitOn "<span class=\"note\">Don&#39;t bet your life on it.</span>").length == 2
#guard Html.render (Pages.borderedImage "/a.png" "alt" (some (4, 3))) ==
  "<p><span class=\"bordered\"><img src=\"/a.png\" alt=\"alt\" width=\"4\" height=\"3\"></span></p>\n"
-- the sitemap lists the top-level pages; each collection has its own container,
-- headed by a link to its index, listing its entries with dates
def navSection (pg start : String) : String :=
  (((pg.splitOn start).getD 1 "").splitOn "</nav>").headD ""
def sitemapOf (pg : String) := navSection pg "<nav class=\"sitemap\">"
def blogIndexOf (pg : String) := navSection pg "<nav class=\"post-index\" aria-label=\"Blog\">"
def articleIndexOf (pg : String) := navSection pg "<nav class=\"post-index\" aria-label=\"Articles\">"
#guard [homePage, blogPage, postPage].all fun pg =>
  ((sitemapOf pg).splitOn "<a href=\"/\"").length == 2 &&
  ((sitemapOf pg).splitOn "/about/").length == 1 &&
  ((sitemapOf pg).splitOn "/blog/").length == 1 &&
  ((blogIndexOf pg).splitOn "<div class=\"post-index-bar\"><a href=\"/blog/\"").length == 2 &&
  ((blogIndexOf pg).splitOn "<a href=\"/blog/x/\"").length == 2 &&
  ((blogIndexOf pg).splitOn "<time datetime=\"2026-01-01\">January 1st, 2026</time>").length == 2 &&
  ((articleIndexOf pg).splitOn "<div class=\"post-index-bar\"><a href=\"/articles/\"").length == 2 &&
  ((articleIndexOf pg).splitOn "<a href=\"/articles/y/\"").length == 2
#guard ((blogIndexOf (Pages.home {})).splitOn "No posts yet.").length == 2
#guard ((articleIndexOf (Pages.home {})).splitOn "No articles yet.").length == 2
#guard ((Pages.post sampleCtx sampleArticle).splitOn
  "<a href=\"/articles/y/\" class=\"current\" aria-current=\"page\">Y</a>").length == 2
#guard (homePage.splitOn "<a href=\"/\" class=\"current\" aria-current=\"page\">Home</a>").length == 2
#guard (blogPage.splitOn "<a href=\"/blog/\" class=\"current\" aria-current=\"page\">Blog</a>").length == 2
#guard (postPage.splitOn "<a href=\"/blog/x/\" class=\"current\" aria-current=\"page\">X</a>").length == 2
#guard [homePage, blogPage, postPage].all fun pg => (pg.splitOn "aria-current").length == 2
-- breadcrumbs on every page but home, ending with the page itself (not a link)
def crumbsOf (pg : String) := navSection pg "<nav class=\"breadcrumbs\" aria-label=\"Breadcrumbs\">"
#guard (homePage.splitOn "class=\"breadcrumbs\"").length == 1
#guard ((crumbsOf blogPage).splitOn "<li><a href=\"/\">lindg.re</a></li>").length == 2
#guard ((crumbsOf blogPage).splitOn "<li><span>Blog</span></li>").length == 2
#guard ((crumbsOf postPage).splitOn "<li><a href=\"/blog/\">Blog</a></li>").length == 2
#guard ((crumbsOf postPage).splitOn "<li><span>X</span></li>").length == 2
#guard ((crumbsOf (Pages.post sampleCtx sampleArticle)).splitOn
  "<li><a href=\"/articles/\">Articles</a></li>").length == 2
-- posts have giscus comments; other pages have no script at all
#guard (postPage.splitOn "<script src=\"https://giscus.app/client.js\" data-repo=\"kekatomb/kekatomb.github.io\"").length == 2
#guard (postPage.splitOn "data-theme=\"https://lindg.re/giscus.css\"").length == 2
#guard [homePage, blogPage].all fun pg => (pg.splitOn "<script").length == 1
-- a post is laid out as a forum post, without a signature
#guard (postPage.splitOn "class=\"signature\"").length == 1
#guard (postPage.splitOn "class=\"domo\"").length == 2
-- the blog's index lists its entries as threads
#guard (blogPage.splitOn "<div class=\"threads\">").length == 2
#guard ((Pages.collectionPage {} Pages.blog).splitOn "No posts yet.").length == 3

/-! ## Pixel Domo -/
#guard Domo.runs "..##.###.".toList == [(2, 2), (5, 3)]
#guard Domo.runs "##".toList == [(0, 2)]
#guard Domo.runs "...".toList == []
-- every frame has the same size, so they stack exactly
#guard Domo.frames.all fun (_, rows) =>
  rows.length == 14 && rows.all (·.length == 12)
#guard (homePage.splitOn "class=\"domo\"").length == 2
-- the left pane is on every page, with the social links
#guard [homePage, blogPage, postPage].all fun pg =>
  (pg.splitOn "<aside class=\"pane-left\">").length == 2 &&
  (pg.splitOn "<a href=\"https://github.com/kekatomb\" title=\"GitHub\" class=\"tile\">").length == 2 &&
  (pg.splitOn "<main class=\"pane-right\">").length == 2
#guard (homePage.splitOn "class=\"open\" alt=\"lindgren\"").length == 2

/-! ## Domo's comments -/
def commentedPost : Post := { samplePostValue with domo := some "nom." }
-- a post's `domo:` comment goes in his bubble, hidden from screen readers
#guard ((Pages.post sampleCtx commentedPost).splitOn
  "<span class=\"speech\" aria-hidden=\"true\">nom.</span>").length == 2
-- no comment, no bubble
#guard [homePage, blogPage, postPage].all fun pg => (pg.splitOn "class=\"speech\"").length == 1
#guard (Post.parse "x" "---\ntitle: t\ndate: 2026-01-01\ndomo: hi there\n---\n").toOption.bind (·.domo)
  == some "hi there"
#guard (homePage.splitOn "<span class=\"zzz\" aria-hidden=\"true\">zzz</span>").length == 2

/-! ## Table of contents -/
#guard Markdown.slugify "Links, code & images" == "links-code-images"
#guard Markdown.slugify "  --Hello--World--  " == "hello-world"
#guard Markdown.slugify "!!!" == "section"
#guard Markdown.uniqueId "a" [] == "a"
#guard Markdown.uniqueId "a" ["a", "a-2"] == "a-3"
def tocDoc := Markdown.parse "# Title\n\n## One\n\n### One *a*\n\n## Two\n\n## One\n\n> ## Quoted\n"
#guard (Markdown.toHtmlWithToc tocDoc).2.map (fun e => (e.level, e.title, e.id)) ==
  [(2, "One", "one"), (3, "One a", "one-a"), (2, "Two", "two"), (2, "One", "one-2")]
-- a flat list, each entry tagged with its level
def tocHtml : String := String.join ((Pages.toc (Markdown.toHtmlWithToc tocDoc).2).map Html.render)
#guard (tocHtml.splitOn "<li class=\"d3\"><a href=\"#one-a\">One a</a></li>").length == 2
-- it sits after the main pane, as the page's third column
def tocPost : Post := { samplePostValue with body := tocDoc }
#guard ((Pages.post sampleCtx tocPost).splitOn "</main>\n<nav class=\"toc\"").length == 2
-- a page with fewer than two sections gets no table of contents
#guard (Pages.toc [{ level := 2, title := "x", id := "x" }]).isEmpty
#guard (postPage.splitOn "class=\"toc\"").length == 1

/-! ## Home: news and Last.fm -/
def sampleJson : String :=
  "{\"recenttracks\":{\"track\":[" ++
  "{\"artist\":{\"#text\":\"Boards of Canada\"},\"name\":\"Roygbiv\",\"url\":\"https://www.last.fm/music/x\",\"@attr\":{\"nowplaying\":\"true\"}}," ++
  "{\"artist\":{\"#text\":\"Aphex Twin\"},\"name\":\"Xtal\",\"url\":\"https://www.last.fm/music/y\",\"date\":{\"uts\":\"1000\",\"#text\":\"x\"}}" ++
  "]}}"
#guard (LastFm.parseRecent sampleJson).toOption.map (·.map (·.name)) == some ["Roygbiv", "Xtal"]
#guard (LastFm.parseRecent sampleJson).toOption.map (·.map (·.nowPlaying)) == some [true, false]
-- a single track arrives as an object, not a list
#guard (LastFm.parseRecent "{\"recenttracks\":{\"track\":{\"artist\":{\"#text\":\"A\"},\"name\":\"B\",\"url\":\"u\"}}}").toOption.map (·.length) == some 1
#guard (LastFm.parseRecent "not json").toOption.isNone
-- the cover: "large" if given, else the biggest other size; the placeholder star counts as none
#guard (LastFm.parseRecent ("{\"recenttracks\":{\"track\":[{\"artist\":{\"#text\":\"A\"},\"name\":\"B\",\"url\":\"u\"," ++
  "\"image\":[{\"size\":\"small\",\"#text\":\"s.jpg\"},{\"size\":\"extralarge\",\"#text\":\"xl.jpg\"}]}," ++
  "{\"artist\":{\"#text\":\"C\"},\"name\":\"D\",\"url\":\"u\"," ++
  "\"image\":[{\"size\":\"large\",\"#text\":\"https://x/2a96cbd8b46e442fc41c2b86b821562f.png\"}]}]}}")).toOption.map (·.map (·.image))
  == some [some "xl.jpg", none]
def playedAt (s : Nat) : LastFm.Track := { artist := "", name := "", url := "", playedAt := some s, nowPlaying := false }
#guard LastFm.ago 1030 (playedAt 1000) == "just now"
#guard LastFm.ago 1060 (playedAt 1000) == "1 minute ago"
#guard LastFm.ago (1000 + 3 * 3600) (playedAt 1000) == "3 hours ago"
#guard LastFm.ago (1000 + 86400) (playedAt 1000) == "1 day ago"
def homeFull : String :=
  Pages.home sampleCtx {
    news := [{ date := ⟨2026, 11, 14⟩, body := Markdown.parse "Hello *news*." }],
    listening := (LastFm.parseRecent sampleJson).toOption.map (·, 1000) }
#guard (homeFull.splitOn "<h2>News</h2>").length == 2
#guard (homeFull.splitOn "<p>Hello <em>news</em>.</p>").length == 2
#guard (homeFull.splitOn "<a href=\"https://www.last.fm/user/kekatomb\">Recently played</a>").length == 2
#guard (homeFull.splitOn "now playing").length == 2
-- tracks without a cover still get an (empty) cover box, so the rows line up
#guard (homeFull.splitOn "<span class=\"cover\"></span>").length == 3
-- Last.fm answered with no tracks: the container says so
#guard ((Pages.home sampleCtx { listening := some ([], 0) }).splitOn "Nothing played recently.").length == 2
-- no news, and no Last.fm data at all: neither section appears
#guard (homePage.splitOn "News").length == 1 && (homePage.splitOn "Recently played").length == 1

/-! ## .env files -/
#guard Env.parse "# comment\n\nLASTFM_API_KEY=abc123\n" == [("LASTFM_API_KEY", "abc123")]
#guard Env.parse "export A = \"x y\"\r\nB='z'\nnot a pair\n=nothing\n" == [("A", "x y"), ("B", "z")]
#guard Env.parse "URL=https://x?a=b" == [("URL", "https://x?a=b")]

/-! ## Home: intro, GitHub and buttons -/
def restJson : String :=
  "[{\"name\":\"lean\",\"html_url\":\"https://github.com/kekatomb/lean\",\"owner\":{\"login\":\"kekatomb\"}," ++
  "\"fork\":false,\"archived\":false,\"pushed_at\":\"2026-09-06T14:29:13Z\",\"description\":\"d\",\"language\":\"Lean\",\"stargazers_count\":2}," ++
  "{\"name\":\"forked\",\"html_url\":\"u\",\"owner\":{\"login\":\"kekatomb\"},\"fork\":true,\"pushed_at\":\"2026-12-01T00:00:00Z\"}," ++
  "{\"name\":\"kekatomb.github.io\",\"html_url\":\"https://github.com/kekatomb/kekatomb.github.io\",\"owner\":{\"login\":\"kekatomb\"}," ++
  "\"fork\":false,\"archived\":false,\"pushed_at\":\"2026-10-31T06:22:10Z\",\"description\":null,\"language\":null}]"
def orgJson : String :=
  "[{\"name\":\"fpil\",\"html_url\":\"https://github.com/kekalutions/fpil\",\"owner\":{\"login\":\"kekalutions\"}," ++
  "\"fork\":false,\"archived\":false,\"pushed_at\":\"2026-10-31T08:06:59Z\"}," ++
  "{\"name\":\"old\",\"html_url\":\"u\",\"owner\":{\"login\":\"kekalutions\"},\"fork\":false,\"archived\":false,\"pushed_at\":\"2020-01-01T00:00:00Z\"}]"
-- forks are left out; the most recent three across both accounts, newest first
def recentRepos : List GitHub.Repo :=
  GitHub.byRecency [(GitHub.parseRestRepos restJson).toOption.getD [], (GitHub.parseRestRepos orgJson).toOption.getD []]
#guard recentRepos.map (·.name) == ["fpil", "kekatomb.github.io", "lean", "old"]
-- split into projects and solutions, newest first
#guard (GitHub.nowParts recentRepos).1.map GitHub.nowLabel == ["this site", "lean"]
#guard (GitHub.nowParts recentRepos).2.map (·.name) == ["fpil", "old"]
-- at most three of each
def manyRepos (owner : String) : List GitHub.Repo :=
  (List.range 5).map fun i => { owner, name := s!"r{i}", url := "u", pushedAt := s!"2026-0{9 - i}" }
#guard (GitHub.nowParts (manyRepos "kekatomb" ++ manyRepos "kekalutions")).1.length == 3
#guard (GitHub.nowParts (manyRepos "kekatomb" ++ manyRepos "kekalutions")).2.length == 3
-- the Oxford comma
#guard GitHub.joinAnd ["a"] ", " " and " ", and " == ["a"]
#guard GitHub.joinAnd ["a", "b"] ", " " and " ", and " == ["a", " and ", "b"]
#guard GitHub.joinAnd ["a", "b", "c"] ", " " and " ", and " == ["a", ", ", "b", ", and ", "c"]
#guard GitHub.joinAnd ["a", "b", "c", "d"] ", " " and " ", and " == ["a", ", ", "b", ", ", "c", ", and ", "d"]
def pinnedJson : String :=
  "{\"data\":{\"user\":{\"pinnedItems\":{\"nodes\":[{\"name\":\"lean\",\"owner\":{\"login\":\"kekatomb\"}," ++
  "\"description\":\"d\",\"url\":\"https://github.com/kekatomb/lean\",\"stargazerCount\":3,\"primaryLanguage\":{\"name\":\"Lean\"}}]}}}}"
#guard (GitHub.parsePinned pinnedJson).toOption.map (·.map (·.language)) == some [some "Lean"]
#guard (GitHub.parsePinned "{\"errors\":[{\"message\":\"Bad credentials\"}]}").toOption.isNone
def homeData : Pages.Home where
  intro := Markdown.parse "# Hi\n\nAbout me."
  recent := recentRepos
  pinned := (GitHub.parsePinned pinnedJson).toOption
def homeGitHub : String := Pages.home sampleCtx homeData
#guard (homeGitHub.splitOn "<h1>Hi</h1>").length == 2
#guard (homeGitHub.splitOn ("<p class=\"now\">Lately I&#39;ve been working on " ++
  "<a href=\"https://github.com/kekatomb/kekatomb.github.io\">this site</a> and " ++
  "<a href=\"https://github.com/kekatomb/lean\">lean</a>, and writing up solutions to " ++
  "<a href=\"https://github.com/kekalutions/fpil\">fpil</a> and <a href=\"u\">old</a>.</p>")).length == 2
-- only one kind: the other half of the sentence is left out
def nowText (rs : List GitHub.Repo) : String := String.join ((Pages.nowLine rs).map Html.render)
#guard ((nowText ((manyRepos "kekalutions").take 1)).splitOn "Lately I&#39;ve been writing up solutions to ").length == 2
#guard ((nowText ((manyRepos "kekatomb").take 3)).splitOn ", and <a href=\"u\">r2</a>.</p>").length == 2
#guard (Pages.nowLine []).isEmpty
#guard (homeGitHub.splitOn "<span class=\"note\">Lean · ★ 3</span>").length == 2
#guard ((Pages.home sampleCtx { pinned := some [] }).splitOn "Nothing pinned yet.").length == 2
#guard (homePage.splitOn "Projects").length == 1

end Site.Tests
