// The site's own JavaScript, kept small. Every page loads it, last. Without
// it every page still works: Domo runs on his stylesheet timers, and the
// live widgets stay hidden or show what the build wrote.
//
// - Domo, on every page: he remembers you (in this browser's localStorage,
//   nowhere else), says something about it, watches the pointer, and sleeps
//   when it's night where the owner is (`data-owner-tz` on the script tag).
// - Recently played on the home page, from Last.fm (with the read-only key
//   the build passes in `data-lastfm-key`). Checked every 10 seconds while
//   the page is in view; a failed check leaves it as it was.
// - The "Lately I've been working on…" line, from GitHub's public API. The
//   build writes a copy, which this refreshes; it mirrors `nowLine` in
//   Site/Pages.lean and Site/GitHub.lean, so both look alike.
// - The search in the left pane, over `/search.json`, which the build writes:
//   titles, summaries and the posts' text, best matches first, with where in
//   the text a match is.
// - Comments, by giscus: "Loading the comments…" until its frame has loaded,
//   and a failed status if it doesn't.
// - Who's online, on the home page: what's playing, how often you've been
//   here, whether Domo is awake, and the owner's time, as a forum's footer
//   gives it ("All times are UTC+1. The time now is 14:32.").

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

  /** A status: Domo as a small pixel icon beside a message, in place of
   * something that can't be shown. The same markup as `status` in
   * Site/Pages.lean; `kind` is "failed" here (the build writes "noscript"). */
  const status = (kind, message) => el("div", { class: `status status-${kind}` },
    el("img", { src: `/img/status-${kind}.svg`, alt: "", width: "24", height: "28" }),
    el("p", {}, message));

  const getJson = async (url) => {
    const res = await fetch(url, { headers: { Accept: "application/json" } });
    if (!res.ok) throw new Error(`${url}: ${res.status}`);
    return res.json();
  };

  /** localStorage as JSON. It can be missing or refuse (private windows,
   * blocked storage), so every read and write may quietly do nothing. */
  const store = {
    get(key, fallback) {
      try {
        const v = localStorage.getItem(key);
        return v ? JSON.parse(v) : fallback;
      } catch { return fallback; }
    },
    set(key, value) {
      try { localStorage.setItem(key, JSON.stringify(value)); } catch { /* fine */ }
    },
  };

  /** A line of the Who's online widget (`.online-<name>`): set its contents,
   * or hide it with none. */
  const online = (name, parts) => {
    const li = document.querySelector(`.online-${name}`);
    if (!li) return;
    if (parts) li.replaceChildren(...parts);
    li.hidden = !parts;
  };

  // ------------------------------------------------------ the owner's time

  const OWNER_TZ = script?.dataset.ownerTz || "Europe/Stockholm";

  /** The owner's offset from UTC, e.g. "UTC+1" (or "UTC"). It changes with
   * daylight saving, so it's worked out each time. */
  const ownerOffset = () => {
    try {
      const name = new Intl.DateTimeFormat("en-GB", { timeZone: OWNER_TZ, timeZoneName: "shortOffset" })
        .formatToParts(new Date()).find((p) => p.type === "timeZoneName")?.value;
      return name ? name.replace("GMT", "UTC") : "UTC";
    } catch {
      return null;
    }
  };

  /** The hour and minute where the owner is (or here, if the zone is bad). */
  const ownerTime = () => {
    const now = new Date();
    try {
      const parts = new Intl.DateTimeFormat("en-GB", {
        timeZone: OWNER_TZ, hour: "2-digit", minute: "2-digit", hourCycle: "h23",
      }).formatToParts(now);
      const get = (type) => Number(parts.find((p) => p.type === type)?.value);
      return { hour: get("hour"), minute: get("minute") };
    } catch {
      return { hour: now.getHours(), minute: now.getMinutes() };
    }
  };

  const pad2 = (n) => String(n).padStart(2, "0");

  /** Night where the owner is, when Domo sleeps: midnight to seven. */
  const ownerNight = () => ownerTime().hour < 7;

  const startTime = () => {
    let shown = "";
    const tick = () => {
      const { hour, minute } = ownerTime();
      const hhmm = `${pad2(hour)}:${pad2(minute)}`;
      if (hhmm === shown) return;
      shown = hhmm;
      const offset = ownerOffset();
      online("time", [offset ? `All times are ${offset}. The time now is ` : "The time now is ",
        el("b", {}, hhmm), "."]);
    };
    tick();
    setInterval(tick, 1000);
  };

  // ------------------------------------------------------------------ Domo

  // He takes over from his stylesheet timers (see `.domo.scripted` in
  // style.css): `data-state` is talk, awake or asleep, and `data-look` where
  // his eyes point. Hovering still wakes him and pressing still chomps, in
  // CSS alone.

  const SLEEP_AFTER = 30_000;        // ignored this long, he falls asleep
  const NIGHT_SLEEP_AFTER = 4_000;   // ...or this long, at the owner's night
  const IDLE_AFTER = 120_000;        // no pointer, keys or scrolling
  const AWAY_AFTER = 30_000;         // tab hidden this long counts as leaving
  const VISIT_GAP = 30 * 60_000;     // a new visit after this long away
  const HEARTBEAT = 15_000;          // how often time on the site is counted

  const startDomo = () => {
    const domo = document.querySelector(".domo");
    if (!domo) return;
    domo.classList.add("scripted");

    // the page's own line (from the build), if it has one
    let speech = domo.querySelector(".speech");
    const pageLine = speech?.textContent.trim() || null;
    if (!speech) {
      speech = el("span", { class: "speech", "aria-hidden": "true" });
      domo.append(speech);
    }

    // ------------------------------------------------- what he remembers

    const now = Date.now();
    const prev = store.get("domo", {});
    const mem = { visits: 0, views: 0, seconds: 0, chomps: 0, ...prev };
    const newVisit = !prev.last || now - prev.last > VISIT_GAP;
    if (newVisit) mem.visits += 1;
    mem.views += 1;
    mem.first ??= now;
    mem.last = now;
    mem.lastPath = location.pathname;
    mem.lastTitle = document.title.replace(/\s+—\s+lindg\.re$/, "");
    store.set("domo", mem);

    // ------------------------------------------------- state and speech

    let state = "awake";
    let hovered = false;
    let talking = false;
    let next = null;          // one line waiting while he talks
    let sleepTimer = 0;

    const setState = (s) => {
      state = s;
      domo.dataset.state = s;
      const status = document.querySelector(".online-domo");
      if (status) status.textContent = s === "asleep" ? "Domo (asleep)" : "Domo (watching)";
    };

    const sleepSoon = () => {
      clearTimeout(sleepTimer);
      sleepTimer = setTimeout(() => {
        if (!talking && !hovered) setState("asleep");
      }, ownerNight() ? NIGHT_SLEEP_AFTER : SLEEP_AFTER);
    };

    /** Say a line: the bubble shows, his mouth moves, then he's quiet. A
     * line said while he's talking waits for him to finish. */
    const say = (text) => {
      if (!text) return;
      if (talking) { next = text; return; }
      talking = true;
      clearTimeout(sleepTimer);
      speech.textContent = text;
      speech.classList.add("on");
      setState("talk");
      setTimeout(() => {
        speech.classList.remove("on");
        talking = false;
        setState("awake");
        sleepSoon();
        if (next) {
          const t = next;
          next = null;
          setTimeout(() => say(t), 700);
        }
      }, Math.min(6000, 1600 + text.length * 70));
    };

    /** Once per page view, keyed by name. */
    const said = new Set();
    const sayOnce = (key, text) => {
      if (said.has(key)) return;
      said.add(key);
      say(text);
    };

    // ------------------------------------------- what he says on arrival

    const rotate = (lines) => lines[(mem.visits + mem.views) % lines.length];
    const daysAway = prev.last ? Math.floor((now - prev.last) / 86_400_000) : 0;
    const sameDay = prev.last && new Date(prev.last).toDateString() === new Date(now).toDateString();
    const yourHour = new Date(now).getHours();

    let arrival = null;
    if (newVisit) {
      if (mem.visits === 1) arrival = pageLine;              // a stranger: just the page's line
      else if (yourHour >= 1 && yourHour < 5) arrival = "you should be asleep.";
      else if (daysAway >= 7) arrival = `it's been ${daysAway} days. i counted.`;
      else if (mem.visits === 2) arrival = "oh. you came back.";
      else if (sameDay) arrival = rotate(["back already?", "again today.", "you can't stay away, can you."]);
      else if (mem.visits % 5 === 0) arrival = `visit ${mem.visits}. i keep a list.`;
      else arrival = rotate(["you again.", "i remember you.", "there you are.", "i waited."]);
    } else if (pageLine) {
      arrival = pageLine;
    } else if (prev.lastPath === location.pathname) {
      if (mem.views % 3 === 0) arrival = "you were just here.";
    } else if (prev.lastTitle && mem.views % 3 === 0) {
      arrival = `you were reading “${prev.lastTitle}”.`;
    }

    // at the owner's night he's asleep when you arrive, and says nothing
    // until you wake him
    if (ownerNight()) setState("asleep");
    else {
      setState("awake");
      if (arrival) setTimeout(() => say(arrival), 500);
      else sleepSoon();
    }

    const visits = mem.visits;
    online("visits", [`You've been here ${visits} time${visits === 1 ? "" : "s"}. Domo keeps count.`]);

    // ---------------------------------------------------- what he notices

    let wakes = 0;
    domo.addEventListener("pointerenter", () => {
      hovered = true;
      clearTimeout(sleepTimer);
      if (state === "asleep") {
        setState("awake");
        if (ownerNight()) {
          wakes += 1;
          const { hour, minute } = ownerTime();
          if (wakes === 1) say("…you woke me.");
          else if (wakes === 2) say(`it's ${pad2(hour)}:${pad2(minute)} here. let me sleep.`);
        }
      }
    });
    domo.addEventListener("pointerleave", () => {
      hovered = false;
      sleepSoon();
    });

    // chomping him: counted, forever
    domo.addEventListener("pointerdown", () => {
      mem.chomps += 1;
      store.set("domo", { ...store.get("domo", mem), chomps: mem.chomps });
      const lines = { 5: "stop that.", 10: "i remember every one.", 25: `${mem.chomps}.`, 50: "you're not going to stop, are you." };
      if (lines[mem.chomps]) say(lines[mem.chomps]);
    });

    let copies = 0;
    document.addEventListener("copy", () => {
      copies += 1;
      if (copies === 1) say("i saw that.");
      else if (copies === 3) say("that isn't yours.");
    });

    // his eyes follow the pointer, while he's awake (one check a frame)
    let pointer = null;
    let looking = false;
    const look = () => {
      looking = false;
      if (!pointer) return;
      const r = domo.getBoundingClientRect();
      const dx = pointer.x - (r.left + r.width / 2);
      const dy = pointer.y - (r.top + r.height / 2);
      const right = dx > 60;
      const down = dy > 60 && dy > dx / 3;
      domo.dataset.look = right && down ? "downright" : right ? "right" : down ? "down" : "";
    };

    // idle: no pointer, keys or scrolling for a while
    let idleTimer = 0;
    const active = () => {
      clearTimeout(idleTimer);
      idleTimer = setTimeout(() => {
        if (!document.hidden) sayOnce("idle", "…are you still there?");
      }, IDLE_AFTER);
      // while you move he stays awake to watch (but doesn't wake up for it)
      if (state === "awake" && !talking) sleepSoon();
    };
    document.addEventListener("pointermove", (e) => {
      pointer = { x: e.clientX, y: e.clientY };
      if (!looking) { looking = true; requestAnimationFrame(look); }
      active();
    }, { passive: true });
    document.addEventListener("keydown", active);
    document.addEventListener("scroll", active, { passive: true });
    active();

    // leaving the tab: after a moment its title asks you back
    const title = document.title;
    let hiddenAt = 0;
    let titleTimer = 0;
    let returns = 0;
    document.addEventListener("visibilitychange", () => {
      if (document.hidden) {
        hiddenAt = Date.now();
        titleTimer = setTimeout(() => { document.title = "come back."; }, 3000);
        return;
      }
      clearTimeout(titleTimer);
      document.title = title;
      if (Date.now() - hiddenAt > AWAY_AFTER) {
        returns += 1;
        if (returns === 1) say("where did you go?");
        else if (returns === 2) say("i waited.");
      }
    });

    // time on the site, counted while the page is in view
    setInterval(() => {
      if (document.hidden) return;
      const m = store.get("domo", mem);
      m.seconds = (m.seconds || 0) + HEARTBEAT / 1000;
      m.last = Date.now();
      store.set("domo", m);
      if (m.seconds >= 3600) sayOnce("hour", "you spend a lot of time here.");
    }, HEARTBEAT);
  };

  // ------------------------------------------------- Last.fm: recently played

  const LASTFM_USER = "kekatomb";
  const TRACKS = 5;
  // how often to check for new tracks, while the page is in view
  const POLL_MS = 10_000;
  // the play count, every this many checks (and whenever a track arrives)
  const COUNT_EVERY = 6;
  // Last.fm sends this grey star instead of a cover when a track has none.
  const PLACEHOLDER = "2a96cbd8b46e442fc41c2b86b821562f";

  /** The cover URL: "large" (174px) first, sharp at 64px on any screen. */
  const pickImage = (t) => {
    for (const size of ["large", "extralarge", "medium", "small"]) {
      const url = (t.image || []).find((i) => i.size === size)?.["#text"];
      if (url && !url.includes(PLACEHOLDER)) return url;
    }
    return null;
  };

  const playing = (t) => t["@attr"]?.nowplaying === "true";
  const artistOf = (t) => t.artist?.["#text"] ?? "";

  /** When a track was played, relative to now. */
  const ago = (now, t) => {
    if (playing(t)) return "Listening now";
    const at = Number(t.date?.uts);
    if (!at) return "";
    const d = now - at;
    const unit = (n, one, many) => `${n} ${n === 1 ? one : many} ago`;
    if (d < 60) return "Played just now";
    if (d < 3600) return `Played ${unit(Math.floor(d / 60), "minute", "minutes")}`;
    if (d < 86400) return `Played ${unit(Math.floor(d / 3600), "hour", "hours")}`;
    return `Played ${unit(Math.floor(d / 86400), "day", "days")}`;
  };

  const startListening = () => {
    const key = script?.dataset.lastfmKey;
    const widget = document.querySelector(".w-listening");
    const body = widget?.querySelector(".widget-body");
    const plays = widget?.querySelector(".bar-note");
    if (!key || !body) return;

    const lastfm = (method, params = "") => getJson(
      `https://ws.audioscrobbler.com/2.0/?method=${method}&user=${LASTFM_USER}` +
      `&api_key=${encodeURIComponent(key)}&format=json${params}`);

    /** A cover, as a small print on the well; an empty square without one. */
    const cover = (t, size) => {
      const src = pickImage(t);
      return el("span", { class: size === 64 ? "cover large" : "cover" },
        ...(src ? [el("img", { src, alt: "", width: String(size), height: String(size) })] : []));
    };

    /** A track's name (linked), artist and when, stacked. */
    const text = (now, t) => el("span", { class: "track-text" },
      el("a", { class: "track-name", href: t.url }, t.name),
      el("span", {}, artistOf(t)),
      el("span", { class: playing(t) ? "track-when track-now" : "note track-when" }, ago(now, t)));

    let tracks = [];
    let shownKey = "";

    const render = (now) => {
      if (!tracks.length) {
        body.replaceChildren(el("p", { class: "note" }, "Nothing played recently."));
        return;
      }
      const [latest, ...earlier] = tracks;
      body.replaceChildren(
        el("div", { class: "np" }, cover(latest, 64), text(now, latest)),
        ...(earlier.length ? [el("ul", { class: "rows" },
          ...earlier.map((t) => el("li", {}, cover(t, 32), text(now, t))))] : []));
    };

    const updateCount = async () => {
      const info = await lastfm("user.getinfo");
      const n = Number(info.user?.playcount);
      if (plays && Number.isFinite(n)) plays.textContent = `${n.toLocaleString("en")} plays`;
    };

    const update = async (withCount) => {
      const data = await lastfm("user.getrecenttracks", `&limit=${TRACKS}`);
      let fresh = data.recenttracks?.track;
      if (!fresh) return;
      // a single track comes as an object, not a list
      if (!Array.isArray(fresh)) fresh = [fresh];
      fresh = fresh.slice(0, TRACKS);
      const now = Math.floor(Date.now() / 1000);
      const freshKey = fresh.map((t) => `${t.url}|${t.date?.uts ?? "now"}`).join(" ");
      const arrived = freshKey !== shownKey;
      if (arrived) {
        // only rebuild when something changed, so the covers don't flicker
        shownKey = freshKey;
        tracks = fresh;
        render(now);
      } else {
        body.querySelectorAll(".track-when").forEach((n, i) => { n.textContent = ago(now, tracks[i]); });
      }
      const latest = tracks[0];
      online("listening", latest && playing(latest)
        ? [`${LASTFM_USER} is listening to `, el("a", { href: latest.url }, latest.name), ` by ${artistOf(latest)}.`]
        : null);
      if (arrived || withCount) await updateCount();
    };

    /** Check now, then every `POLL_MS` while the page is in view, and
     * straight away when it comes back. One check at a time; a failed one
     * leaves the widget as it was. */
    let busy = false;
    let checks = 0;
    const check = async () => {
      if (busy || document.hidden) return;
      busy = true;
      try { await update(checks++ % COUNT_EVERY === 0); }
      catch (err) {
        console.warn("couldn't update recently played:", err);
        // the first check failed: say so in place of the skeleton (the
        // next check that works fills it in); a later failure leaves the
        // tracks as they were
        if (!shownKey) body.replaceChildren(status("failed", "Couldn't reach Last.fm. Trying again shortly."));
      }
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
    const intro = document.querySelector("main section.intro");
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

  // ---------------------------------------------------------------- search

  /** A query's words, lower case. */
  const queryWords = (query) => query.toLowerCase().split(/\s+/).filter(Boolean);

  /** How well an entry matches a query's words: nothing unless every word
   * is in it somewhere (title, summary or text); otherwise more for each
   * word in the title (most), the summary, or only the text (least). */
  const score = (entry, words) => {
    const title = entry.title.toLowerCase();
    const summary = (entry.summary || "").toLowerCase();
    const body = (entry.body || "").toLowerCase();
    let total = 0;
    for (const w of words) {
      if (title.includes(w)) total += 4;
      else if (summary.includes(w)) total += 2;
      else if (body.includes(w)) total += 1;
      else return 0;
    }
    return total;
  };

  /** Where in an entry's text a word the title and summary don't have is:
   * about a sentence around its first one, cut at word boundaries, with
   * "…" where it was cut. None when the title and summary have them all. */
  const SNIPPET = 70;   // characters either side
  const snippet = (entry, words) => {
    const head = `${entry.title} ${entry.summary || ""}`.toLowerCase();
    const body = entry.body || "";
    const lower = body.toLowerCase();
    const word = words.find((w) => !head.includes(w));
    const at = word ? lower.indexOf(word) : -1;
    if (at < 0) return null;
    let from = Math.max(0, at - SNIPPET);
    let to = Math.min(body.length, at + word.length + SNIPPET);
    if (from > 0) from = body.indexOf(" ", from) + 1 || from;
    if (to < body.length) to = body.lastIndexOf(" ", to) > at ? body.lastIndexOf(" ", to) : to;
    return (from > 0 ? "…" : "") + body.slice(from, to).trim() + (to < body.length ? "…" : "");
  };

  /** Text with the query's words marked (`<mark>`), as nodes. */
  const highlight = (text, words) => {
    const pattern = words.map((w) => w.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")).join("|");
    if (!pattern) return [text];
    return text.split(new RegExp(`(${pattern})`, "gi"))
      .map((part, i) => (i % 2 ? el("mark", {}, part) : part));
  };

  const SEARCH_RESULTS = 6;

  const startSearch = () => {
    const well = document.querySelector(".side-well.search");
    const body = well?.querySelector(".side-well-body");
    if (!body) return;
    const input = el("input", {
      type: "search", "aria-label": "Search posts and articles", placeholder: "Search posts…",
      autocomplete: "off", spellcheck: "false" });
    const results = el("ul", { class: "rows", "aria-live": "polite" });
    results.hidden = true;
    body.replaceChildren(el("div", { class: "search-box" }, input), results);

    // the index, fetched once, the first time you reach for the box
    let index = null;
    const load = () => {
      index ??= getJson("/search.json").catch((err) => {
        console.warn("couldn't load the search index:", err);
        index = null;
        return null;
      });
      return index;
    };
    input.addEventListener("focus", load, { once: true });

    input.addEventListener("input", async () => {
      const query = input.value.trim();
      if (!query) { results.replaceChildren(); results.hidden = true; return; }
      const all = await load();
      if (input.value.trim() !== query) return;   // you've typed on since
      results.hidden = false;
      if (!all) {
        results.replaceChildren(el("li", {}, status("failed", "Couldn't load the search. Try again in a moment.")));
        return;
      }
      const words = queryWords(query);
      // best first; equally good ones stay newest first (the index's order)
      const found = all.map((e, i) => ({ e, i, s: score(e, words) }))
        .filter((r) => r.s > 0)
        .sort((a, b) => b.s - a.s || a.i - b.i)
        .map((r) => r.e);
      results.replaceChildren(...(found.length
        ? found.slice(0, SEARCH_RESULTS).map((e) => {
            const excerpt = snippet(e, words);
            return el("li", {},
              el("a", { href: e.url }, ...highlight(e.title, words)),
              ...(excerpt ? [el("span", { class: "search-snippet" }, ...highlight(excerpt, words))] : []),
              el("span", { class: "note search-where" }, `${e.section}, ${e.date}`));
          })
        : [el("li", { class: "note" }, "No posts match.")]));
    });
  };

  // -------------------------------------------------------------- comments

  // giscus draws the comments in a frame; until it has loaded, the build's
  // "Loading the comments…" stands in. If the frame never comes (giscus is
  // blocked or down), or never finishes once you've scrolled to it (it
  // loads lazily), that says so instead.
  const FRAME_WAIT = 15_000;

  const startComments = () => {
    const box = document.querySelector(".giscus");
    const wait = box?.querySelector(".giscus-wait");
    if (!wait) return;
    let done = false;
    const fail = () => {
      if (done) return;
      done = true;
      wait.replaceChildren(status("failed", "Couldn't load the comments from giscus. Reload to try again."));
    };
    const loaded = () => { if (!done) { done = true; wait.remove(); } };

    // the frame: when it appears, wait for it to load once it's in view
    let frameTimer = setTimeout(fail, FRAME_WAIT);
    const watch = (frame) => {
      clearTimeout(frameTimer);
      frame.addEventListener("load", loaded, { once: true });
      new IntersectionObserver((entries, seen) => {
        if (!entries.some((e) => e.isIntersecting)) return;
        seen.disconnect();
        frameTimer = setTimeout(fail, FRAME_WAIT);
        frame.addEventListener("load", () => clearTimeout(frameTimer), { once: true });
      }).observe(box);
    };
    const found = box.querySelector("iframe");
    if (found) { watch(found); return; }
    new MutationObserver((_, obs) => {
      const frame = box.querySelector("iframe");
      if (frame) { obs.disconnect(); watch(frame); }
    }).observe(box, { childList: true, subtree: true });
  };

  startDomo();
  startSearch();
  startComments();
  startTime();
  startListening();
  // a failed fetch leaves the line as the build wrote it, and says why in
  // the console
  if (document.querySelector("main section.intro")) {
    refreshNow().catch((err) => console.warn("couldn't update the now line:", err));
  }
})();
