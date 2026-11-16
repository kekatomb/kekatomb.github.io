// The site's own JavaScript, kept to a minimum. Two parts of the home page are
// live, fetched when the page is read rather than when the site was built:
//
// - Recently played, from Last.fm (with the API key the build passes in
//   `data-lastfm-key`; it's a read-only key, so it's fine in the page). The
//   build writes its panel empty and hidden; this fills it in and shows it,
//   then checks again every 10 seconds while the page is in view, so "now
//   playing" stays true. Without JavaScript, or if Last.fm can't be reached,
//   it stays hidden (and once shown, a failed check leaves it as it was).
// - The "Lately I've been working on…" line, from GitHub's public API. The
//   build writes a copy, which this refreshes; if GitHub can't be reached,
//   the copy stays. It mirrors `nowLine` in Site/Pages.lean and
//   Site/GitHub.lean, so both look alike.

(() => {
  "use strict";

  const script = document.currentScript;

  /** An element with attributes and children (strings become text). */
  const el = (tag, attrs = {}, ...children) => {
    const e = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v);
    e.append(...children);
    return e;
  };

  const getJson = async (url) => {
    const res = await fetch(url, { headers: { Accept: "application/json" } });
    if (!res.ok) throw new Error(`${url}: ${res.status}`);
    return res.json();
  };

  // ------------------------------------------------------------- Last.fm

  const LASTFM_USER = "kekatomb";
  const TRACKS = 5;
  // how often to check for new tracks, while the page is in view
  const POLL_MS = 10_000;
  // Last.fm sends this grey star instead of a cover when a track has none.
  const PLACEHOLDER = "2a96cbd8b46e442fc41c2b86b821562f";

  /** The cover URL: "large" (174px) first, sharp at 48px on any screen. */
  const pickImage = (t) => {
    for (const size of ["large", "extralarge", "medium", "small"]) {
      const url = (t.image || []).find((i) => i.size === size)?.["#text"];
      if (url && !url.includes(PLACEHOLDER)) return url;
    }
    return null;
  };

  /** When a track was played, relative to now. */
  const ago = (now, t) => {
    if (t["@attr"]?.nowplaying === "true") return "now playing";
    const at = Number(t.date?.uts);
    if (!at) return "";
    const d = now - at;
    const unit = (n, one, many) => `${n} ${n === 1 ? one : many} ago`;
    if (d < 60) return "just now";
    if (d < 3600) return unit(Math.floor(d / 60), "minute", "minutes");
    if (d < 86400) return unit(Math.floor(d / 3600), "hour", "hours");
    return unit(Math.floor(d / 86400), "day", "days");
  };

  const track = (now, t) => {
    const cover = pickImage(t);
    return el("li", {},
      el("span", { class: "cover" },
        ...(cover ? [el("img", { src: cover, alt: "", width: "48", height: "48" })] : [])),
      el("span", {},
        el("a", { href: t.url }, t.name), el("br"),
        t.artist?.["#text"] ?? "", el("br"),
        el("span", { class: "note" }, ago(now, t))));
  };

  const fillListening = async () => {
    const key = script?.dataset.lastfmKey;
    const panel = document.querySelector(".listening");
    const list = panel?.querySelector(".post-index-list");
    if (!key || !list) return;
    const data = await getJson(
      "https://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks" +
      `&user=${LASTFM_USER}&api_key=${encodeURIComponent(key)}&format=json&limit=${TRACKS}`);
    let tracks = data.recenttracks?.track;
    if (!tracks) return;
    // a single track comes as an object, not a list
    if (!Array.isArray(tracks)) tracks = [tracks];
    const now = Math.floor(Date.now() / 1000);
    const fresh = tracks.length
      ? el("ul", {}, ...tracks.slice(0, TRACKS).map((t) => track(now, t)))
      : el("p", { class: "note" }, "Nothing played recently.");
    // only when something changed, so the covers don't flicker every check
    if (list.innerHTML !== fresh.outerHTML) list.replaceChildren(fresh);
    panel.hidden = false;
  };

  /** Fill in recently played now, then again every `POLL_MS` while the page
   * is in view, and straight away when it comes back into view. One check at
   * a time; a failed one leaves the panel as it was. */
  const pollListening = () => {
    if (!script?.dataset.lastfmKey || !document.querySelector(".listening")) return;
    let busy = false;
    const check = async () => {
      if (busy || document.hidden) return;
      busy = true;
      try { await fillListening(); }
      catch (err) { console.warn("couldn't update recently played:", err); }
      finally { busy = false; }
    };
    check();
    setInterval(check, POLL_MS);
    document.addEventListener("visibilitychange", check);
  };

  // ---------------------------------------------------------- the now line

  const GITHUB_USER = "kekatomb";
  // the organisation with solutions to textbooks, CTFs and so on
  const SOLUTIONS_ORG = "kekalutions";
  const RECENT = 3;

  /** "a", "a and b", "a, b, and c" (the Oxford comma). */
  const joinAnd = (xs) => xs.flatMap((x, i) =>
    i === 0 ? [x]
    : xs.length === 2 ? [" and ", x]
    : i === xs.length - 1 ? [", and ", x]
    : [", ", x]);

  const names = (repos) => joinAnd(repos.map((r) =>
    el("a", { href: r.html_url },
      r.name === `${GITHUB_USER}.github.io` ? "this site" : r.name)));

  const refreshNow = async () => {
    const intro = document.querySelector("main > section.intro");
    if (!intro) return;
    const lists = await Promise.all([
      getJson(`https://api.github.com/users/${GITHUB_USER}/repos?sort=pushed&per_page=10`),
      getJson(`https://api.github.com/orgs/${SOLUTIONS_ORG}/repos?sort=pushed&per_page=10`),
    ]);
    const recent = lists.flat()
      .filter((r) => !r.fork && !r.archived)
      .sort((a, b) => (b.pushed_at ?? "").localeCompare(a.pushed_at ?? ""));
    const projects = recent.filter((r) => r.owner.login !== SOLUTIONS_ORG).slice(0, RECENT);
    const solutions = recent.filter((r) => r.owner.login === SOLUTIONS_ORG).slice(0, RECENT);

    const sentence =
      projects.length && solutions.length
        ? ["Lately I've been working on ", ...names(projects),
           ", and writing up solutions to ", ...names(solutions)]
      : projects.length ? ["Lately I've been working on ", ...names(projects)]
      : solutions.length ? ["Lately I've been writing up solutions to ", ...names(solutions)]
      : [];

    const old = intro.querySelector("p.now");
    if (!sentence.length) { old?.remove(); return; }
    const line = el("p", { class: "now" }, ...sentence, ".");
    if (old) old.replaceWith(line); else intro.append(line);
  };

  pollListening();
  // a failed fetch leaves the line as the build wrote it, and says why in
  // the console
  refreshNow().catch((err) => console.warn("couldn't update the now line:", err));
})();
