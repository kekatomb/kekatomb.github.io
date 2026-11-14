/-!
# Pixel Domo

The mark in the top-left of the home page. Each frame is a grid of `#` (ink)
and `.` (empty), turned into an SVG at build time and written to
`_site/img/domo-<name>.svg`. The page's stylesheet decides which frame shows
(see `.domo` in `static/style.css`), so he can react to hover and press
without JavaScript.

The pixels are dark metal lit from above: a top-to-bottom gradient, a thin
highlight on every top edge, and a tight shadow underneath, like the rest of
the site's light skeuomorphism.
-/

namespace Site.Domo

/-- Screen pixels per sprite pixel. -/
def scale : Nat := 3

/-- Idle: mouth open. -/
def openFrame : List String :=
  [ ".##########."
  , "############"
  , "############"
  , "###.####.###"
  , "###.####.###"
  , "############"
  , "##.#.#.#.###"
  , "##........##"
  , "##........##"
  , "###.#.#.#.##"
  , "############"
  , "############"
  , ".##########."
  , "..##....##.." ]

/-- Chomp: the teeth lock together. -/
def shutFrame : List String :=
  [ ".##########."
  , "############"
  , "############"
  , "###.####.###"
  , "###.####.###"
  , "############"
  , "##.#.#.#.###"
  , "###.#.#.#.##"
  , "############"
  , "############"
  , "############"
  , "############"
  , ".##########."
  , "..##....##.." ]

/-- Hover: eyes up, mouth stretched wide open. -/
def wideFrame : List String :=
  [ ".##########."
  , "############"
  , "###.####.###"
  , "###.####.###"
  , "############"
  , "##.#.#.#.###"
  , "##........##"
  , "##........##"
  , "##........##"
  , "##........##"
  , "###.#.#.#.##"
  , "############"
  , ".##########."
  , "..##....##.." ]

/-- Asleep: eyes closed, mouth shut. -/
def sleepFrame : List String :=
  [ ".##########."
  , "############"
  , "############"
  , "############"
  , "##..####..##"
  , "############"
  , "##.#.#.#.###"
  , "###.#.#.#.##"
  , "############"
  , "############"
  , "############"
  , "############"
  , ".##########."
  , "..##....##.." ]

/-- All frames, by the name used in their file name. -/
def frames : List (String × List String) :=
  [("open", openFrame), ("shut", shutFrame), ("wide", wideFrame), ("sleep", sleepFrame)]

/-- Runs of `#` in a row, as `(start, length)`. -/
def runs (row : List Char) : List (Nat × Nat) :=
  go row 0 none []
where
  go : List Char → Nat → Option Nat → List (Nat × Nat) → List (Nat × Nat)
    | [], i, some s, acc => acc ++ [(s, i - s)]
    | [], _, none, acc => acc
    | c :: cs, i, cur, acc =>
      if c == '#' then go cs (i + 1) (cur.getD i |> some) acc
      else match cur with
        | some s => go cs (i + 1) none (acc ++ [(s, i - s)])
        | none => go cs (i + 1) none acc

def width (rows : List String) : Nat := (rows.head?.map String.length).getD 0 * scale
def height (rows : List String) : Nat := rows.length * scale

/-- One `<rect>` per run of ink, row by row. -/
def rects (rows : List String) : String :=
  let rec go : List String → Nat → String
    | [], _ => ""
    | r :: rs, y =>
      String.join ((runs r.toList).map fun (x, n) =>
        s!"<rect x=\"{x * scale}\" y=\"{y * scale}\" width=\"{n * scale}\" height=\"{scale}\"/>")
      ++ go rs (y + 1)
  go rows 0

def svg (rows : List String) : String :=
  let w := width rows
  let h := height rows
  s!"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 {w} {h}\" width=\"{w}\" height=\"{h}\" shape-rendering=\"crispEdges\">
<defs>
<linearGradient id=\"metal\" x1=\"0\" y1=\"0\" x2=\"0\" y2=\"{h}\" gradientUnits=\"userSpaceOnUse\">
<stop offset=\"0\" stop-color=\"#4a4a4a\"/><stop offset=\"1\" stop-color=\"#1a1a1a\"/>
</linearGradient>
<filter id=\"bevel\" x=\"-10%\" y=\"-10%\" width=\"120%\" height=\"125%\" color-interpolation-filters=\"sRGB\">
<feOffset in=\"SourceAlpha\" dy=\"1\" result=\"down\"/>
<feComposite in=\"SourceAlpha\" in2=\"down\" operator=\"out\" result=\"top-edges\"/>
<feFlood flood-color=\"#fff\" flood-opacity=\".4\"/>
<feComposite in2=\"top-edges\" operator=\"in\" result=\"highlight\"/>
<feDropShadow in=\"SourceAlpha\" dx=\"0\" dy=\"1\" stdDeviation=\".6\" flood-color=\"#000\" flood-opacity=\".35\" result=\"shadow\"/>
<feMerge><feMergeNode in=\"shadow\"/><feMergeNode in=\"SourceGraphic\"/><feMergeNode in=\"highlight\"/></feMerge>
</filter>
</defs>
<g filter=\"url(#bevel)\" fill=\"url(#metal)\">{rects rows}</g>
</svg>
"

end Site.Domo
