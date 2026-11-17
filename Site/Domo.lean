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

/-- Knocked out: crossed eyes, mouth shut. Not one of his frames in the
corner, but the icon for something that failed to load (see
`Pages.status`). -/
def koFrame : List String :=
  [ ".##########."
  , "############"
  , "##.#.##.#.##"
  , "###.####.###"
  , "##.#.##.#.##"
  , "############"
  , "##.#.#.#.###"
  , "###.#.#.#.##"
  , "############"
  , "############"
  , "############"
  , "############"
  , ".##########."
  , "..##....##.." ]

/-- The icons for `Pages.status`, by the name in their file name
(`/img/status-<name>.svg`): Domo awake while something loads, knocked out
when it failed to, and asleep when JavaScript is off. -/
def statusIcons : List (String × List String) :=
  [("loading", openFrame), ("failed", koFrame), ("noscript", sleepFrame)]

/-- Screen pixels per sprite pixel in a status icon. -/
def statusScale : Nat := 2

/-- The open frame with its eyes (rows 3 and 4) replaced. -/
def withEyes (row3 row4 : String) : List String :=
  openFrame.set 3 row3 |>.set 4 row4

/-- Watching: his eyes follow the pointer (`site.js` picks the frame).
To the right: eyes moved one pixel over. -/
def rightFrame : List String := withEyes "####.####.##" "####.####.##"

/-- Down: the lids come down to half. (The eyes can't move lower: they would
run into the gaps between his teeth.) -/
def downFrame : List String := withEyes "############" "###.####.###"

/-- Down and to the right. -/
def downRightFrame : List String := withEyes "############" "####.####.##"

/-- All frames, by the name used in their file name. -/
def frames : List (String × List String) :=
  [ ("open", openFrame), ("shut", shutFrame), ("wide", wideFrame), ("sleep", sleepFrame)
  , ("right", rightFrame), ("down", downFrame), ("downright", downRightFrame) ]

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

def width (rows : List String) (px : Nat := scale) : Nat := (rows.head?.map String.length).getD 0 * px
def height (rows : List String) (px : Nat := scale) : Nat := rows.length * px

/-- One `<rect>` per run of ink, row by row, `px` screen pixels per sprite
pixel. -/
def rects (rows : List String) (px : Nat := scale) : String :=
  let rec go : List String → Nat → String
    | [], _ => ""
    | r :: rs, y =>
      String.join ((runs r.toList).map fun (x, n) =>
        s!"<rect x=\"{x * px}\" y=\"{y * px}\" width=\"{n * px}\" height=\"{px}\"/>")
      ++ go rs (y + 1)
  go rows 0

/-- A frame as an SVG, `px` screen pixels per sprite pixel. -/
def svg (rows : List String) (px : Nat := scale) : String :=
  let w := width rows px
  let h := height rows px
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
<g filter=\"url(#bevel)\" fill=\"url(#metal)\">{rects rows px}</g>
</svg>
"

/-- The site's 88×31 button, for other sites to link here with, as the
personal sites of the 2000s did: a silver plate with a dark edge and a light
one along its lit top and left, and the site's name across it.
Flat colours, no filters, so it stays crisp when scaled. -/
def button : String :=
  s!"<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 88 31\" width=\"88\" height=\"31\" shape-rendering=\"crispEdges\">
<defs>
<linearGradient id=\"plate\" x1=\"0\" y1=\"0\" x2=\"0\" y2=\"1\">
<stop offset=\"0\" stop-color=\"#fafafa\"/><stop offset=\"1\" stop-color=\"#d2d2d2\"/>
</linearGradient>
</defs>
<rect width=\"88\" height=\"31\" fill=\"#7a7a7a\"/>
<rect x=\"1\" y=\"1\" width=\"86\" height=\"29\" fill=\"url(#plate)\"/>
<rect x=\"1\" y=\"1\" width=\"86\" height=\"1\" fill=\"#fff\"/>
<rect x=\"1\" y=\"1\" width=\"1\" height=\"29\" fill=\"#fff\"/>
<text x=\"44\" y=\"20\" text-anchor=\"middle\" font-family=\"Verdana, Tahoma, &quot;DejaVu Sans&quot;, sans-serif\" font-size=\"13\" font-weight=\"bold\" fill=\"#222\" shape-rendering=\"auto\">lindg.re</text>
</svg>
"

end Site.Domo
