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

/-! ## No JavaScript from content: only the scripts the templates name -/
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
-- scripts: only the allowed ones, and only with `data-` settings; the site's
-- own deferred, others without cookies
#guard Html.render (.script "https://evil.example/x.js" [("data-a", "b")]) == ""
#guard Html.render (.script "/evil.js" []) == ""
#guard Html.render (.script "/site.js" [("data-lastfm-key", "k"), ("onload", "y")]) ==
  "<script src=\"/site.js\" data-lastfm-key=\"k\" defer></script>\n"
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
-- the profile: the owner's name, their post count, and buttons for their
-- other profiles; nothing about where they are
def profileOf (pg : String) : String :=
  (((pg.splitOn "<section class=\"profile\" aria-label=\"About the owner\">").getD 1 "").splitOn "</section>").headD ""
#guard ((profileOf homePage).splitOn ("<p class=\"profile-name\"><strong>lindgren</strong><br>" ++
  "<span class=\"note\">Don&#39;t bet your life on it.</span></p>")).length == 2
#guard ((profileOf homePage).splitOn "<p class=\"note\">Posts: 2</p>").length == 2
#guard ((profileOf (Pages.home {})).splitOn "<p class=\"note\">Posts: 0</p>").length == 2
#guard ((profileOf homePage).splitOn ("<a href=\"https://github.com/kekatomb\" title=\"GitHub\" class=\"button icon-button\">" ++
  "<img src=\"/icons/github-dark.svg\" alt=\"GitHub\" width=\"14\" height=\"14\"></a>")).length == 2
-- (the time zone is only for site.js, in the script tag)
#guard [homePage, blogPage, postPage].all fun pg =>
  (pg.splitOn "Stockholm").length == 2 && (pg.splitOn "Location").length == 1
#guard Html.render (Pages.borderedImage "/a.png" "alt" (some (4, 3))) ==
  "<p><span class=\"bordered\"><img src=\"/a.png\" alt=\"alt\" width=\"4\" height=\"3\"></span></p>\n"
-- the navigation: Home and each collection, on every page.
-- The collection you're in unfolds to list its entries with their dates.
def navSection (pg start : String) : String :=
  (((pg.splitOn start).getD 1 "").splitOn "</nav>").headD ""
def siteNavOf (pg : String) := navSection pg "<nav class=\"site-nav\" aria-label=\"Site\">"
#guard [homePage, blogPage, postPage].all fun pg =>
  ((siteNavOf pg).splitOn "<a href=\"/\"").length == 2 &&
  ((siteNavOf pg).splitOn "/blog/\"").length == 2 && ((siteNavOf pg).splitOn "/articles/\"").length == 2
-- on home, everything is folded (the board lists the entries)
#guard ((siteNavOf homePage).splitOn "site-nav-entries").length == 1
-- in the blog, the blog's entries unfold, and not the articles'
#guard [blogPage, postPage].all fun pg =>
  ((siteNavOf pg).splitOn "<ul class=\"site-nav-entries\">").length == 2 &&
  ((siteNavOf pg).splitOn "/blog/x/\"").length == 2 &&
  ((siteNavOf pg).splitOn "<time datetime=\"2026-01-01\">January 1st, 2026</time>").length == 2 &&
  ((siteNavOf pg).splitOn "/articles/y/").length == 1
#guard ((siteNavOf (Pages.collectionPage {} Pages.articles)).splitOn
  "<p class=\"note site-nav-entries\">No articles yet.</p>").length == 2
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
-- posts have giscus comments
#guard (postPage.splitOn "<script src=\"https://giscus.app/client.js\" data-repo=\"kekatomb/kekatomb.github.io\"").length == 2
#guard (postPage.splitOn "data-theme=\"https://lindg.re/giscus.css\"").length == 2
-- every page has the site's own script (for Domo), with the owner's time
-- zone, last in the body; only posts have another (giscus)
def siteScript := "<script src=\"/site.js\" data-owner-tz=\"Europe/Stockholm\" defer></script>\n</body>"
#guard [homePage, blogPage, postPage].all fun pg => (pg.splitOn siteScript).length == 2
#guard [homePage, blogPage].all fun pg => (pg.splitOn "<script").length == 2
#guard (postPage.splitOn "<script").length == 3
-- the home page's also has the Last.fm key, when there is one
#guard ((Pages.home sampleCtx { lastfmKey := some "k" }).splitOn
  "<script src=\"/site.js\" data-lastfm-key=\"k\" data-owner-tz=\"Europe/Stockholm\" defer></script>").length == 2
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
-- the left pane is on every page, with the profile
#guard [homePage, blogPage, postPage].all fun pg =>
  (pg.splitOn "<aside class=\"pane-left\">").length == 2 &&
  (pg.splitOn "<section class=\"profile\"").length == 2 &&
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
def homeFull : String :=
  Pages.home sampleCtx {
    news := [{ date := ⟨2026, 11, 14⟩, body := Markdown.parse "Hello *news*." }],
    lastfmKey := some "k" }
#guard (homeFull.splitOn "<section class=\"widget w-news\"><div class=\"widget-bar\">News</div>").length == 2
#guard (homeFull.splitOn "<p>Hello <em>news</em>.</p>").length == 2
-- recently played: written as a skeleton the size of the real thing (the
-- latest track and four rows), for site.js to fill in, with room in the bar
-- for the play count
#guard (homeFull.splitOn ("<section class=\"widget w-listening\" aria-label=\"Recently played\">" ++
  "<div class=\"widget-bar\"><a href=\"https://www.last.fm/user/kekatomb\">Recently played</a><span class=\"bar-note\"></span></div>\n" ++
  "<div class=\"widget-body\"><div class=\"np skeleton script-only\" aria-hidden=\"true\"><span class=\"cover large\"></span>")).length == 2
#guard (homeFull.splitOn "<li><span class=\"cover\"></span><span class=\"track-text\"><span class=\"bone").length == 5
-- the skeleton is only for when JavaScript can fill it; without, a status says so
#guard (homeFull.splitOn "<div class=\"np skeleton script-only\"").length == 2
#guard (homeFull.splitOn ("<noscript><div class=\"status status-noscript\"><img src=\"/img/status-noscript.svg\" " ++
  "alt=\"\" width=\"24\" height=\"28\"><p>Turn on JavaScript to see what I&#39;ve been listening to.</p>")).length == 2
-- comments say so too (as does the search in the left pane)
#guard (postPage.splitOn "<noscript><div class=\"status status-noscript\">").length == 3
#guard (postPage.splitOn "Turn on JavaScript to read and write comments.").length == 2
-- while giscus loads, a status says so (only with JavaScript)
#guard (postPage.splitOn ("<div class=\"giscus-wait script-only\"><div class=\"status status-loading\">" ++
  "<img src=\"/img/status-loading.svg\"")).length == 2
-- who's online says what it would show with JavaScript
#guard (homePage.splitOn "Turn on JavaScript to see what&#39;s playing, your visits and the time here.").length == 2
-- the status icons: Domo knocked out and asleep, at the same size as each other
#guard Domo.statusIcons.all fun (_, rows) => rows.length == 14 && rows.all (·.length == 12)
#guard (Domo.svg Domo.koFrame Domo.statusScale).startsWith
  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 28\" width=\"24\" height=\"28\""
-- the owner's time is a line in who's online, hidden until site.js sets it
#guard (homePage.splitOn "<li class=\"online-time note\" hidden=\"\"></li>").length == 2
#guard (homePage.splitOn "w-clock").length == 1
-- no news, and no Last.fm key: neither section appears
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
  pinned := Pages.Fetched.ofExcept (GitHub.parsePinned pinnedJson)
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
#guard (homeGitHub.splitOn ("<div class=\"proj\"><a href=\"https://github.com/kekatomb/lean\">lean</a>" ++
  "<span class=\"note\">Lean<br>3 stars</span></div>\n<p class=\"note\">d</p>")).length == 2
#guard ((Pages.home sampleCtx { pinned := .ok [] }).splitOn "Nothing pinned yet.").length == 2
-- when fetching them failed, Projects and Activity say so; without a token
-- they're left out
def homeFailed : String := Pages.home sampleCtx { pinned := .failed, contributions := .failed }
#guard (homeFailed.splitOn "<section class=\"widget w-projects\"").length == 2
#guard (homeFailed.splitOn "<section class=\"widget w-activity\"").length == 2
#guard (homeFailed.splitOn "<div class=\"status status-failed\">").length == 3
#guard (homeFailed.splitOn "Couldn&#39;t reach GitHub when this page was built.").length == 3
#guard (homePage.splitOn "w-projects").length == 1 && (homePage.splitOn "status-failed").length == 1
#guard (homePage.splitOn "Projects").length == 1

/-! ## Home: the board -/
-- the widgets, in two columns: main, then side
#guard (homePage.splitOn "<div class=\"widgets\"><div class=\"widgets-main\"><section class=\"widget w-welcome\">").length == 2
#guard (homePage.splitOn "<div class=\"widgets-side\"><section class=\"widget w-online\"").length == 2
-- the welcome is the sticky post, with the intro
#guard (homeGitHub.splitOn "<div class=\"widget-bar\"><span class=\"sticky\">Sticky:</span> Welcome</div>").length == 2
-- the board index: each collection with its count and its latest entry
def boardOf (pg : String) : String :=
  (((pg.splitOn "<ul class=\"rows board\">").getD 1 "").splitOn "</ul>").headD ""
#guard ((boardOf homePage).splitOn ("<li><div><a href=\"/blog/\">Blog</a><br><span class=\"note\">Personal thoughts.</span></div>\n" ++
  "<span class=\"count\">1</span><div class=\"latest\"><a href=\"/blog/x/\">X</a><br><span class=\"note\">" ++
  "<time datetime=\"2026-01-01\">January 1st, 2026</time></span></div>\n</li>")).length == 2
#guard ((boardOf (Pages.home {})).splitOn "<span class=\"count\">0</span><div class=\"latest\"><span class=\"note\">Nothing yet.</span>").length == 3
-- only the collections: projects have their own widget
#guard ((boardOf homeGitHub).splitOn "Projects").length == 1

/-! ## Home: activity -/
#guard Date.weekday ⟨2026, 11, 16⟩ == 1
#guard Date.weekday ⟨2000, 1, 1⟩ == 6
#guard Date.weekday ⟨2024, 2, 29⟩ == 4
def contribJson : String :=
  "{\"data\":{\"user\":{\"contributionsCollection\":{\"contributionCalendar\":{\"totalContributions\":4,\"weeks\":[" ++
  "{\"contributionDays\":[{\"date\":\"2026-11-12\",\"contributionCount\":0,\"contributionLevel\":\"NONE\"}," ++
  "{\"date\":\"2026-11-13\",\"contributionCount\":1,\"contributionLevel\":\"FIRST_QUARTILE\"}," ++
  "{\"date\":\"2026-11-14\",\"contributionCount\":3,\"contributionLevel\":\"FOURTH_QUARTILE\"}]}," ++
  "{\"contributionDays\":[{\"date\":\"2026-11-15\",\"contributionCount\":0,\"contributionLevel\":\"NONE\"}]}]}}}}}"
def contrib : Option GitHub.Calendar := (GitHub.parseContributions contribJson).toOption
#guard contrib.map (·.total) == some 4
#guard contrib.map (·.weeks.map (·.map (·.level))) == some [[0, 1, 4], [0]]
#guard (GitHub.parseContributions "{\"errors\":[{\"message\":\"Bad credentials\"}]}").toOption.isNone
def homeActivity : String :=
  Pages.home sampleCtx { contributions := Pages.Fetched.ofExcept (GitHub.parseContributions contribJson) }
-- the first week starts on a Thursday: four empty holes before it
#guard (homeActivity.splitOn ("<span class=\"week\">" ++ String.join (List.replicate 4 "<span class=\"peg pad\"></span>") ++
  "<span class=\"peg l0\" title=\"0 contributions on November 12th, 2026\"></span>" ++
  "<span class=\"peg l1\" title=\"1 contribution on November 13th, 2026\"></span>")).length == 2
#guard (homeActivity.splitOn "4 contributions in the last year").length == 2
#guard (homePage.splitOn "w-activity").length == 1

/-! ## Domo's eyes -/
#guard ["right", "down", "downright"].all fun n => (Domo.frames.map (·.1)).contains n
#guard (homePage.splitOn "<img src=\"/img/domo-downright.svg\" class=\"downright\" alt=\"\"").length == 2
#guard Domo.rightFrame != Domo.openFrame

/-! ## The left pane: guestbook, search and links -/
#guard (homePage.splitOn "last-visit").length == 1
-- the guestbook: in the navigation on every page, current on its own
def guestbook : String := Pages.guestbookPage sampleCtx
#guard [homePage, blogPage, postPage, guestbook].all fun pg =>
  ((siteNavOf pg).splitOn "/guestbook/\"").length == 2
#guard ((siteNavOf guestbook).splitOn
  "<a href=\"/guestbook/\" class=\"current\" aria-current=\"page\">Guestbook</a>").length == 2
#guard (guestbook.splitOn "<h1>Guestbook</h1>").length == 2
#guard ((crumbsOf guestbook).splitOn "<li><span>Guestbook</span></li>").length == 2
#guard (guestbook.splitOn "<script src=\"https://giscus.app/client.js\"").length == 2
#guard (guestbook.splitOn "Leave a note").length == 2
#guard ((Pages.guestbookPage sampleCtx (Markdown.parse "Say *hi*.")).splitOn "<p>Say <em>hi</em>.</p>").length == 2
-- search: hidden until site.js sets it up, over an index of every entry
#guard (homePage.splitOn ("<section class=\"side-well search\" aria-label=\"Search\"><div class=\"side-well-bar\">Search</div>\n" ++
  "<div class=\"side-well-body\"><div class=\"search-box script-only\" aria-hidden=\"true\">")).length == 2
#guard (homePage.splitOn "<p>Turn on JavaScript to search the posts.</p>").length == 2
#guard ((Lean.Json.parse (Pages.searchIndex sampleCtx)).toOption.bind (·.getArr?.toOption)).map
  (·.toList.map fun e => (e.getObjValAs? String "url").toOption) == some [some "/blog/x/", some "/articles/y/"]
#guard (Pages.searchIndex {}) == "[]"
-- the index has each entry's text too, plain, margin notes and all
#guard Markdown.searchText (Markdown.parse "# Hi\n\nSome *very*   plain^[a note] text.\n\n- one\n- two\n") ==
  "Hi Some very plain a note text. one two"
#guard ((Pages.searchIndex sampleCtx).splitOn "\"body\":").length == 3
-- links: this site's own 88×31 button first
#guard (homePage.splitOn ("<section class=\"side-well links\" aria-label=\"Links\">" ++
  "<div class=\"side-well-bar\">Links</div>\n<div class=\"side-well-body\"><ul><li><a href=\"/\" " ++
  "title=\"lindg.re: link here with this button\"><img src=\"/img/button-lindgre.svg\"")).length == 2
-- the page for a missing address: absolute links only, since GitHub Pages
-- serves it at whatever address was asked for
def notFound : String := Pages.notFoundPage sampleCtx
#guard (notFound.splitOn "<h1>Nothing here</h1>").length == 2
#guard (notFound.splitOn "<div class=\"status status-failed\">").length == 2
#guard (notFound.splitOn "href=\"/\"").length ≥ 2
#guard ((notFound.splitOn "href=\"").drop 1).all fun s => s.startsWith "/" || s.startsWith "#" || s.startsWith "http"
#guard ((notFound.splitOn "src=\"").drop 1).all fun s => s.startsWith "/" || s.startsWith "http"
#guard Domo.button.startsWith "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 88 31\" width=\"88\" height=\"31\""

end Site.Tests
