import Site

/-!
# Site builder

`lake exe site` reads `content/` and writes the finished site to `_site/`:

* `content/intro.md`, `content/news/<date>.md`, GitHub, Last.fm → `_site/index.html`
* `content/posts/<slug>.md` → `_site/blog/<slug>/index.html`
* `content/articles/<slug>.md` → `_site/articles/<slug>/index.html`
* (each collection)         → `_site/blog/index.html`, `_site/articles/index.html`
* `static/**`, `CNAME`      → copied as-is
-/

open Site System

def contentDir : FilePath := "content"
def staticDir : FilePath := "static"
def outDir : FilePath := "_site"

def writePage (path : FilePath) (html : String) : IO Unit := do
  if let some parent := path.parent then IO.FS.createDirAll parent
  IO.FS.writeFile path html

def copyFile (src dst : FilePath) : IO Unit := do
  if let some parent := dst.parent then IO.FS.createDirAll parent
  IO.FS.writeBinFile dst (← IO.FS.readBinFile src)

/-- Load a collection's entries from its folder, newest first. -/
def loadCollection (c : Pages.Collection) : IO Pages.Collection := do
  return { c with entries := ← loadPosts c }
where
  loadPosts (c : Pages.Collection) : IO (List Post) := do
  let dir := contentDir / c.dir
  unless ← dir.pathExists do return []
  let mut posts := #[]
  for entry in ← dir.readDir do
    if entry.path.extension != some "md" then continue
    let some slug := entry.path.fileStem | continue
    match Post.parse slug (← IO.FS.readFile entry.path) with
    | .ok p => if !p.draft then posts := posts.push { p with kind := c.key }
    | .error e => throw <| IO.userError s!"{entry.path}: {e}"
  -- newest first; ties broken by slug so the output is deterministic
  return posts.toList.mergeSort fun a b =>
    a.date.key > b.date.key || (a.date.key == b.date.key && a.slug ≤ b.slug)

/-- News items from `content/news/`, newest first. Each file is named by its
date (`2026-11-14.md`, or `2026-11-14-anything.md` for more than one a day)
and holds plain Markdown. -/
def loadNews : IO (List Pages.NewsItem) := do
  let dir := contentDir / "news"
  unless ← dir.pathExists do return []
  let mut items : Array (String × Pages.NewsItem) := #[]
  for entry in ← dir.readDir do
    if entry.path.extension != some "md" then continue
    let some stem := entry.path.fileStem | continue
    match Date.parse (String.ofList (stem.toList.take 10)) with
    | .ok date => items := items.push (stem, { date, body := Markdown.parse (← IO.FS.readFile entry.path) })
    | .error e => throw <| IO.userError s!"{entry.path}: file name must start with YYYY-MM-DD ({e})"
  return (items.toList.mergeSort fun a b => a.1 ≥ b.1).map (·.2)

/-- Run `curl` on `args`; its output, or `none` (with a warning naming `what`)
when it fails. -/
def curl (what : String) (args : Array String) : IO (Option String) := do
  let out ← IO.Process.output { cmd := "curl", args := #["-sf", "--max-time", "20"] ++ args }
  if out.exitCode == 0 then return some out.stdout
  IO.eprintln s!"warning: couldn't fetch {what} (curl exit {out.exitCode}); leaving it out"
  return none

/-- The GitHub repositories, most recently updated first (for the "now" line), and the
pinned ones (for Projects). Recent repos come from the public REST API, which
works without a token. Pins need GraphQL, which needs `GITHUB_TOKEN`; without
it they're `none`. Any failure just leaves that part out. -/
def fetchGitHub (envFile : List (String × String)) :
    IO (List GitHub.Repo × Option (List GitHub.Repo)) := do
  let token ← Env.lookup envFile "GITHUB_TOKEN"
  let auth := match token with
    | some t => #["-H", s!"Authorization: Bearer {t}"]
    | none => #[]
  let headers := #["-H", "Accept: application/vnd.github+json", "-H", "User-Agent: lindg.re-site-builder"] ++ auth
  let mut lists := []
  for url in GitHub.recentUrls do
    if let some body ← curl "recent GitHub repositories" (headers ++ #[url]) then
      match GitHub.parseRestRepos body with
      | .ok repos => lists := lists ++ [repos]
      | .error e => IO.eprintln s!"warning: couldn't read GitHub's repositories ({e}); leaving them out"
  let pinned ← match token with
    | none => pure none
    | some _ => do
      let some body ← curl "pinned GitHub repositories"
        (headers ++ #["-X", "POST", "-d", GitHub.pinnedQuery, GitHub.graphqlUrl]) | pure none
      match GitHub.parsePinned body with
      | .ok repos => pure (some repos)
      | .error e =>
        IO.eprintln s!"warning: couldn't read GitHub's pinned repositories ({e}); leaving them out"
        pure none
  return (GitHub.byRecency lists, pinned)

/-- Variables from the local `.env` file, if there is one. -/
def loadEnvFile : IO (List (String × String)) := do
  let path : FilePath := ".env"
  if ← path.pathExists then return Env.parse (← IO.FS.readFile path) else return []

def build : IO Unit := do
  let ctx : Pages.Ctx := {
    blog := ← loadCollection Pages.blog
    articles := ← loadCollection Pages.articles }

  if ← outDir.pathExists then IO.FS.removeDirAll outDir
  IO.FS.createDirAll outDir

  let envFile ← loadEnvFile
  let introPath := contentDir / "intro.md"
  let intro ← if ← introPath.pathExists
    then pure (Markdown.parse (← IO.FS.readFile introPath)) else pure []
  let (recent, pinned) ← fetchGitHub envFile
  writePage (outDir / "index.html") <| Pages.home ctx {
    intro, recent, pinned, news := ← loadNews
    lastfmKey := (← Env.lookup envFile "LASTFM_API_KEY").filter (!·.isEmpty) }
  for c in ctx.collections do
    writePage (outDir / c.key / "index.html") (Pages.collectionPage ctx c)
    for p in c.entries do
      writePage (outDir / c.key / p.slug / "index.html") (Pages.post ctx p)
  for (name, rows) in Domo.frames do
    writePage (outDir / "img" / s!"domo-{name}.svg") (Domo.svg rows)


  if ← staticDir.pathExists then
    for src in ← staticDir.walkDir do
      if ← src.isDir then continue
      if (src.fileName.map (·.startsWith ".")).getD false then continue
      let rel := String.ofList (src.toString.toList.drop (staticDir.toString.length + 1))
      copyFile src (outDir / rel)
  if ← FilePath.pathExists "CNAME" then copyFile "CNAME" (outDir / "CNAME")

  IO.println s!"built {ctx.blog.entries.length} post(s) and {ctx.articles.entries.length} article(s) into {outDir}/"

def main : IO UInt32 := do
  try
    build
    return 0
  catch e =>
    IO.eprintln s!"error: {e}"
    return 1
