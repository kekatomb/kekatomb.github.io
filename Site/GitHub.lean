import Lean.Data.Json
import Site.Post

/-!
# GitHub: pinned repositories and recent work

Two things on the home page come from GitHub, fetched while the site is built
(see `Main.lean`):

* **Projects**: the repositories pinned on the profile. GitHub only exposes
  pins through its GraphQL API, which needs a token (`GITHUB_TOKEN`; GitHub
  Actions provides one, locally it can go in `.env`).
* **Activity**: the contribution calendar of the last year, also from
  GraphQL, so also only with a token.
* **The "now" line**: the most recently pushed repositories of the profile and
  of the `kekalutions` organisation (solutions to textbooks, CTFs and the
  like), from the public REST API, which works without a token.

This module only builds the requests and parses the responses, so it can be
tested without the network.
-/

namespace Site.GitHub
open Lean (Json)

def user : String := "kekatomb"
def profileUrl : String := s!"https://github.com/{user}"

/-- The organisation with solutions to textbooks, CTFs and so on. Its repos
get their own part of the "now" line: "…, and writing up solutions to …". -/
def solutionsOrg : String := "kekalutions"

/-- The most each part of the "now" line names: projects, and solutions. -/
def recentCount : Nat := 3

/-- The REST calls for each account's most recently pushed repositories. -/
def recentUrls : List String :=
  [ s!"https://api.github.com/users/{user}/repos?sort=pushed&per_page=10"
  , s!"https://api.github.com/orgs/{solutionsOrg}/repos?sort=pushed&per_page=10" ]

def graphqlUrl : String := "https://api.github.com/graphql"

/-- The GraphQL request body for the pinned repositories. -/
def pinnedQuery : String :=
  Json.compress <| Json.mkObj [("query", Json.str <|
    s!"\{ user(login: \"{user}\") \{ pinnedItems(first: 6, types: REPOSITORY) \{ nodes \{ " ++
    "... on Repository { name owner { login } description url stargazerCount " ++
    "primaryLanguage { name } } } } } }")]

structure Repo where
  owner : String
  name : String
  url : String
  description : Option String := none
  language : Option String := none
  stars : Nat := 0
  /-- When it was last pushed to, as an ISO 8601 timestamp (sorts as text). -/
  pushedAt : String := ""
deriving BEq, Repr

private def optStr (j : Json) (k : String) : Option String :=
  (j.getObjValAs? String k).toOption.filter (!·.isEmpty)

/-- Repositories from a REST list (`/users/…/repos`), leaving out forks and
archived ones. -/
def parseRestRepos (src : String) : Except String (List Repo) := do
  let arr ← (← Json.parse src).getArr?
  let repos ← arr.toList.filterMapM fun r => do
    let fork := (r.getObjValAs? Bool "fork").toOption.getD false
    let archived := (r.getObjValAs? Bool "archived").toOption.getD false
    if fork || archived then return none
    return some {
      owner := ← (r.getObjValD "owner").getObjValAs? String "login"
      name := ← r.getObjValAs? String "name"
      url := ← r.getObjValAs? String "html_url"
      description := optStr r "description"
      language := optStr r "language"
      stars := (r.getObjValAs? Nat "stargazers_count").toOption.getD 0
      pushedAt := (r.getObjValAs? String "pushed_at").toOption.getD "" }
  pure repos

/-- Fail with GitHub's own message when a GraphQL response has errors. -/
def checkErrors (j : Json) : Except String Unit := do
  if let .ok errs := (j.getObjValD "errors").getArr? then
    if !errs.isEmpty then throw s!"GitHub said: {(errs[0]!.getObjValAs? String "message").toOption.getD "error"}"

/-- Pinned repositories from the GraphQL response. -/
def parsePinned (src : String) : Except String (List Repo) := do
  let j ← Json.parse src
  checkErrors j
  let nodes ← ((((j.getObjValD "data").getObjValD "user").getObjValD "pinnedItems").getObjValD "nodes").getArr?
  nodes.toList.mapM fun r => do
    pure {
      owner := ← (r.getObjValD "owner").getObjValAs? String "login"
      name := ← r.getObjValAs? String "name"
      url := ← r.getObjValAs? String "url"
      description := optStr r "description"
      language := optStr (r.getObjValD "primaryLanguage") "name"
      stars := (r.getObjValAs? Nat "stargazerCount").toOption.getD 0 }

/-- The GraphQL request body for the contribution calendar of the last year. -/
def contributionsQuery : String :=
  Json.compress <| Json.mkObj [("query", Json.str <|
    s!"\{ user(login: \"{user}\") \{ contributionsCollection \{ contributionCalendar \{ " ++
    "totalContributions weeks { contributionDays { date contributionCount contributionLevel } } } } } }")]

/-- One day of the calendar. `level` is GitHub's own shade for it, from 0
(nothing) to 4 (the busiest quarter of days). -/
structure Day where
  date : Date
  count : Nat
  level : Nat
deriving BEq, Repr

/-- The contribution calendar: weeks (Sunday first), oldest first. The first
and last weeks can be partial. -/
structure Calendar where
  total : Nat
  weeks : List (List Day)
deriving BEq, Repr

def levelOf : String → Nat
  | "FIRST_QUARTILE" => 1
  | "SECOND_QUARTILE" => 2
  | "THIRD_QUARTILE" => 3
  | "FOURTH_QUARTILE" => 4
  | _ => 0

/-- The contribution calendar from the GraphQL response. -/
def parseContributions (src : String) : Except String Calendar := do
  let j ← Json.parse src
  checkErrors j
  let cal := (((j.getObjValD "data").getObjValD "user").getObjValD "contributionsCollection").getObjValD
    "contributionCalendar"
  let weeks ← (cal.getObjValD "weeks").getArr?
  let weeks ← weeks.toList.mapM fun w => do
    let days ← (w.getObjValD "contributionDays").getArr?
    days.toList.mapM fun d => do
      pure {
        date := ← Date.parse (← d.getObjValAs? String "date")
        count := (d.getObjValAs? Nat "contributionCount").toOption.getD 0
        level := levelOf ((d.getObjValAs? String "contributionLevel").toOption.getD "") }
  pure { total := (cal.getObjValAs? Nat "totalContributions").toOption.getD 0, weeks }

/-- When a repository was last pushed to, as a date. -/
def Repo.pushedDate (r : Repo) : Option Date :=
  (Date.parse (String.ofList (r.pushedAt.toList.take 10))).toOption

/-- All repositories from the lists, most recently pushed first. -/
def byRecency (lists : List (List Repo)) : List Repo :=
  lists.flatten.mergeSort fun a b => a.pushedAt ≥ b.pushedAt

/-- The "now" line's two parts: the most recent projects (the profile's own
repositories) and solutions (the solutions organisation's), at most
`recentCount` of each, newest first. -/
def nowParts (recent : List Repo) : List Repo × List Repo :=
  ((recent.filter (·.owner != solutionsOrg)).take recentCount,
   (recent.filter (·.owner == solutionsOrg)).take recentCount)

/-- How a repository is named in the "now" line: its name, except this site's
own repository, which is "this site". -/
def nowLabel (r : Repo) : String :=
  if r.name == s!"{user}.github.io" then "this site" else r.name

/-- Join items as an English list with the Oxford comma: "a", "a and b",
"a, b, and c". `comma` is ", ", `and_` " and " and `commaAnd` ", and ". -/
def joinAnd {α} (xs : List α) (comma and_ commaAnd : α) : List α :=
  match xs with
  | [] => []
  | [x] => [x]
  | [x, y] => [x, and_, y]
  | _ =>
    let rec go : List α → List α
      | [] => []
      | [x] => [commaAnd, x]
      | x :: rest => comma :: x :: go rest
    match xs with
    | x :: rest => x :: go rest
    | [] => []

end Site.GitHub
