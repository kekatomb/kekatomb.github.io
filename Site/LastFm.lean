import Lean.Data.Json

/-!
# Last.fm: recently played

The home page shows the most recent tracks scrobbled to Last.fm. There's no
JavaScript, so they're fetched while the site is built (see `fetchRecent` in
`Main.lean`) and written into the page. The deploy workflow rebuilds the site
every hour to keep them fresh.

This module only parses the API's JSON and formats the times, so it can be
tested without the network.
-/

namespace Site.LastFm
open Lean (Json)

/-- The Last.fm user whose scrobbles are shown. -/
def user : String := "kekatomb"

def profileUrl : String := s!"https://www.last.fm/user/{user}"

/-- How many tracks the home page shows. -/
def count : Nat := 5

/-- The API call for the recent tracks (`key` is the API key). -/
def apiUrl (key : String) : String :=
  s!"https://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user={user}" ++
  s!"&api_key={key}&format=json&limit={count}"

structure Track where
  artist : String
  name : String
  url : String
  /-- When it was played, in seconds since 1970 (absent while it's playing). -/
  playedAt : Option Nat
  nowPlaying : Bool
  /-- The album cover: Last.fm's image URL when parsed, then the local copy's
  path once `downloadCovers` has fetched it. -/
  image : Option String := none
deriving BEq, Repr

/-- Last.fm sends this grey star instead of a cover when a track has none. -/
def placeholderImage : String := "2a96cbd8b46e442fc41c2b86b821562f"

/-- The cover URL from a track's `image` list, unless it's empty or Last.fm's
placeholder star. "large" (174px) comes first: sharp at the 48px it's shown at,
even on high-resolution screens, without downloading the 300px version. -/
def pickImage (t : Json) : Option String :=
  match (t.getObjValD "image").getArr? with
  | .error _ => none
  | .ok imgs =>
    let bySize (size : String) : Option String :=
      imgs.toList.findSome? fun i =>
        if (i.getObjValAs? String "size").toOption == some size then
          (i.getObjValAs? String "#text").toOption.filter fun u =>
            !u.isEmpty && (u.splitOn placeholderImage).length == 1
        else none
    ["large", "extralarge", "medium", "small"].findSome? bySize

/-- Read one track object from the API's JSON. -/
def parseTrack (t : Json) : Except String Track := do
  let artist ← (t.getObjValD "artist").getObjValAs? String "#text"
  let name ← t.getObjValAs? String "name"
  let url ← t.getObjValAs? String "url"
  let playedAt := ((t.getObjValD "date").getObjValAs? String "uts").toOption.bind String.toNat?
  let nowPlaying := ((t.getObjValD "@attr").getObjValAs? String "nowplaying").toOption == some "true"
  pure { artist, name, url, playedAt, nowPlaying, image := pickImage t }

/-- Read the response to `user.getrecenttracks`, at most `count` tracks.
(The API sends a single object instead of a list when there's one track.) -/
def parseRecent (src : String) : Except String (List Track) := do
  let j ← Json.parse src
  let tracks := (j.getObjValD "recenttracks").getObjValD "track"
  let items := match tracks.getArr? with
    | .ok arr => arr.toList
    | .error _ => [tracks]
  let parsed ← items.mapM parseTrack
  pure (parsed.take count)

/-- When a track was played, relative to `now` (seconds since 1970). -/
def ago (now : Nat) (t : Track) : String :=
  if t.nowPlaying then "now playing" else
  match t.playedAt with
  | none => ""
  | some at_ =>
    let d := now - at_
    let unit (n : Nat) (one many : String) := s!"{n} {if n == 1 then one else many} ago"
    if d < 60 then "just now"
    else if d < 3600 then unit (d / 60) "minute" "minutes"
    else if d < 86400 then unit (d / 3600) "hour" "hours"
    else unit (d / 86400) "day" "days"

/-- Download each track's cover into `dir` (served at `/img/covers/`), so the
page loads covers from this site instead of from Last.fm. Each track's
`image` becomes the local path, or `none` when there's no cover or the
download fails. -/
def downloadCovers (dir : System.FilePath) (tracks : List Track) : IO (List Track) := do
  IO.FS.createDirAll dir
  tracks.mapM fun t => do
    let some src := t.image | return t
    let ext := if src.endsWith ".png" then "png" else if src.endsWith ".gif" then "gif" else "jpg"
    let name := s!"{hash src}.{ext}"
    let out ← IO.Process.output
      { cmd := "curl", args := #["-sf", "--max-time", "20", "-o", (dir / name).toString, src] }
    if out.exitCode == 0 then return { t with image := some s!"/img/covers/{name}" }
    else
      IO.eprintln s!"warning: couldn't download the cover for {t.artist} – {t.name}; leaving it out"
      return { t with image := none }

end Site.LastFm
