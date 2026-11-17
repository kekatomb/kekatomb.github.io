/-!
# Last.fm: recently played

The home page shows the most recent tracks scrobbled to Last.fm, fetched in
the browser by `static/site.js` (which has the rules: the cover it picks,
"3 hours ago", and so on). The build only writes the deck, hidden, with the
API key for the script (see `deckWidget` in `Site/Pages.lean`); the
script fills it in and shows it. Without JavaScript, or when Last.fm can't be
reached, it stays hidden.
-/

namespace Site.LastFm

/-- The Last.fm user whose scrobbles are shown (`site.js` has it too). -/
def user : String := "kekatomb"

def profileUrl : String := s!"https://www.last.fm/user/{user}"

end Site.LastFm
