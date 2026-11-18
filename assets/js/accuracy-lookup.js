/* Accuracy lookup: search an airport or an item and read its figures.
 *
 * Reads the payload that tools/gen_accuracy_tables.py writes to
 * assets/pages/accuracy_lookup_{europe,world,overall}.json and renders it
 * into each div.acc-lookup[data-scope][data-src]. Every figure comes from the
 * payload; this file holds wording and layout only.
 *
 * Row        = [own_pct, own_label, eligible, ref_idx, ref_movements, ref_pct,
 *               ref_label, compared, accuracy_text, median_s, accuracy_label]
 * OverallRow = [ref_idx, values_per_day, eligible, own_pct, own_label,
 *               compared_with, ref_movements, ref_pct, ref_label, compared,
 *               accuracy_text, median_s, p90_s, accuracy_label]
 */
(function () {
  "use strict";

  var DASH = "—";
  var R = { ownPct: 0, ownLabel: 1, eligible: 2, ref: 3, refMov: 4, refPct: 5, refLabel: 6,
            compared: 7, accText: 8, median: 9, accLabel: 10 };
  var O = { ref: 0, perDay: 1, eligible: 2, ownPct: 3, ownLabel: 4, comparedWith: 5, refMov: 6,
            refPct: 7, refLabel: 8, compared: 9, accText: 10, median: 11, p90: 12, accLabel: 13 };

  // The five rows of the airport card, by MAIN anchor.
  var CARD_ROWS = [
    ["Runway times", ["take-off", "landing"]],
    ["Block times", ["off-block", "in-block"]],
    ["Runways", ["runway-dep", "runway-arr"]],
    ["Stands", ["stand-dep", "stand-arr"]],
    ["Airports", ["adep", "ades"]]
  ];

  // ---------------------------------------------------------------- helpers

  function esc(s) {
    return String(s == null ? "" : s)
      .replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;").replace(/'/g, "&#39;");
  }

  // Escape, then show `backtick` spans as <code>.
  function code(s) {
    return esc(s).replace(/`([^`]*)`/g, "<code>$1</code>");
  }

  function plain(s) {
    return String(s == null ? "" : s).replace(/`/g, "");
  }

  function fold(s) {
    return String(s == null ? "" : s).toLowerCase().replace(/ı/g, "i")
      .normalize("NFD").replace(/\p{Diacritic}/gu, "");
  }

  function isNum(v) {
    return typeof v === "number" && isFinite(v);
  }

  function fmtPct(v) {
    return isNum(v) ? v.toFixed(1) + " %" : DASH;
  }

  function fmtCount(v) {
    if (!isNum(v)) return DASH;
    return String(Math.round(v)).replace(/\B(?=(\d{3})+(?!\d))/g, ",");
  }

  function fmtSecs(v) {
    return isNum(v) ? Math.round(v) + " s" : DASH;
  }

  function labelClass(label) {
    switch (label) {
      case "good": case "high": return "acc-good";
      case "fair": case "partial": return "acc-fair";
      case "limited": case "low": return "acc-poor";
      default: return "acc-none";
    }
  }

  function badge(label, empty) {
    var word = label || empty;
    if (!word) return "";
    return '<span class="acc-badge ' + labelClass(label) + '">' + esc(word) + "</span>";
  }

  function bar(pct, label) {
    var w = isNum(pct) ? Math.max(0, Math.min(100, pct)) : 0;
    return '<span class="acc-bar" aria-hidden="true"><span class="acc-bar-fill ' + labelClass(label) +
      '" style="width:' + w + '%"></span></span>';
  }

  function pageHref(page) {
    return esc(String(page || "").replace(/\.qmd$/, ".html"));
  }

  function plural(n, one, many) {
    return fmtCount(n) + " " + (n === 1 ? one : many);
  }

  function mainBy(payload, x) {
    var main = payload.main || [];
    for (var i = 0; i < main.length; i++) {
      if (main[i].anchor === x || main[i].item === x) return main[i];
    }
    return null;
  }

  function itemName(payload, key) {
    var m = mainBy(payload, key);
    if (m) return m.name;
    var it = (payload.items || {})[key];
    return it ? it.name : key;
  }

  function referenceName(payload, idx) {
    return (payload.references || [])[idx] || "";
  }

  function itemHash(payload, key) {
    var m = mainBy(payload, key);
    return "#" + (m ? m.anchor : "item-" + encodeURIComponent(key));
  }

  // ----------------------------------------------------------------- search

  function rankFields(fields, q) {
    var best = 3;
    for (var i = 0; i < fields.length; i++) {
      var f = fields[i];
      if (!f) continue;
      if (f === q) return 0;
      if (f.indexOf(q) === 0 || f.indexOf(" " + q) >= 0 || f.indexOf("(" + q) >= 0) best = Math.min(best, 1);
      else if (f.indexOf(q) >= 0) best = Math.min(best, 2);
    }
    return best;
  }

  function search(payload, query, limit) {
    if (limit === undefined) limit = 8;
    var q = fold(query).trim();
    if (!q || !payload) return [];
    var hits = [];
    var order = 0;
    if (payload.scope !== "overall") {
      (payload.airports || []).forEach(function (a) {
        var icao = fold(a.icao);
        var rank = icao === q ? 0 : rankFields([icao, fold(a.name), fold(a.country)], q);
        if (rank === 0 && icao !== q) rank = 0.5;  // only an exact ICAO comes first; a whole name or country next
        if (rank < 3) {
          hits.push({ rank: rank, side: 0, order: order++, r: {
            kind: "airport", key: a.icao, label: a.name,
            sub: a.icao + " · " + a.country + " · rank " + a.rank } });
        }
      });
    }
    var items = payload.items || {};
    var keys = [];
    (payload.main || []).forEach(function (m) { if (keys.indexOf(m.item) < 0) keys.push(m.item); });
    Object.keys(items).forEach(function (k) { if (keys.indexOf(k) < 0) keys.push(k); });
    keys.forEach(function (k) {
      var m = mainBy(payload, k);
      var it = items[k];
      if (!it && !(m && payload.scope !== "overall")) return;
      var fields = [fold(k), fold(plain(it ? it.name : ""))];
      if (m) fields.push(fold(m.title), fold(m.anchor), fold(plain(m.name)));
      var rank = rankFields(fields, q) || 0.5;
      if (rank < 3) {
        hits.push({ rank: rank, side: 1, order: order++, r: {
          kind: "item", key: k, label: m ? m.name : it.name,
          sub: it ? it.group : "" } });
      }
    });
    hits.sort(function (x, y) { return x.rank - y.rank || x.side - y.side || x.order - y.order; });
    return hits.slice(0, limit).map(function (h) { return h.r; });
  }

  // ----------------------------------------------------------- airport card

  function referenceNote(a) {
    if (a.where === "airport records") return "Compared with airport records";
    if (a.where === "outside Europe") return "No reference: availability only";
    return "Compared with Network Manager only";
  }

  function warnings(payload, a) {
    var linked = payload.scope === "europe";
    function callout(anchor, title, text) {
      var head = linked ? '<a href="#' + anchor + '">' + esc(title) + "</a>" : esc(title);
      var stop = /\.$/.test(title) ? "" : ".";
      return '<div class="acc-warn" role="note"><strong>' + head + stop + "</strong> " + esc(text) + "</div>";
    }
    var out = [];
    var flags = a.flags || [];
    if (flags.indexOf("ground") >= 0) {
      out.push(callout("ground-reception", "Ground events are nearly absent here",
        "The receivers near the airport hear almost no aircraft on the ground. Block times, stands " +
        "and the airport surface events are then missing, not wrong."));
    }
    if (flags.indexOf("no_times") >= 0) {
      out.push(callout("no-times", "No take-off or landing time exists here",
        "OPDI has no take-off time and no landing time on any of the days, although its flight list " +
        "has flights here. Do not use this airport for runway or arrival studies."));
    }
    var limited = (a.limited || []).map(function (an) {
      var m = mainBy(payload, an);
      return m ? m.title : an;
    });
    if (limited.length) {
      out.push(callout("unreliable", "Unreliable here: " + limited.join(", "),
        "Fewer than half of OPDI's values agree with the records. Do not use the item here without " +
        "checking it."));
    }
    return out.join("");
  }

  function availabilityLine(row) {
    if (!row || (!isNum(row[R.ownPct]) && !isNum(row[R.eligible]))) {
      return '<p class="acc-muted">No flights of this kind at this airport.</p>';
    }
    return '<div class="acc-avail">' + bar(row[R.ownPct], row[R.ownLabel]) +
      '<span class="acc-figure">' + fmtPct(row[R.ownPct]) + "</span> " + badge(row[R.ownLabel]) +
      ' <span class="acc-muted">of ' + plural(row[R.eligible], "flight", "flights") + "</span></div>";
  }

  function accuracyLines(payload, row) {
    if (!row) return "";
    var label = row[R.accLabel];
    var ref = referenceName(payload, row[R.ref]);
    if (label === "no reference" || ref === "no reference") {
      return '<p class="acc-muted">No reference: availability only.</p>';
    }
    var parts = [];
    if (row[R.accText]) parts.push('<span class="acc-figure">' + esc(row[R.accText]) + "</span>");
    if (isNum(row[R.median])) parts.push("median " + fmtSecs(row[R.median]));
    parts.push(plural(row[R.compared] == null ? null : row[R.compared], "compared", "compared"));
    var out = '<p class="acc-acc">' + badge(label, "not rated") + " " + parts.join(" · ") + "</p>";
    out += '<p class="acc-muted acc-small">Against ' + esc(ref) + ": found for " + fmtPct(row[R.refPct]) +
      " of " + plural(row[R.refMov], "recorded movement", "recorded movements") + ".</p>";
    return out;
  }

  function tile(payload, m, row) {
    var it = (payload.items || {})[m.item];
    var name = it ? '<a href="' + pageHref(it.page) + '">' + code(m.name) + "</a>" : code(m.name);
    return '<div class="acc-tile"><h5 class="acc-tile-title">' + name + "</h5>" +
      '<div class="acc-tile-part"><span class="acc-part-label">Availability</span>' + availabilityLine(row) + "</div>" +
      '<div class="acc-tile-part"><span class="acc-part-label">Accuracy</span>' + accuracyLines(payload, row) + "</div>" +
      "</div>";
  }

  function compactRow(payload, key, row) {
    var it = payload.items[key] || {};
    var name = it.page ? '<a href="' + pageHref(it.page) + '">' + code(it.name) + "</a>" : code(it.name || key);
    var avail = (!isNum(row[R.ownPct]) && !isNum(row[R.eligible]))
      ? '<span class="acc-muted">no flights</span>'
      : fmtPct(row[R.ownPct]) + " " + badge(row[R.ownLabel]) +
        ' <span class="acc-muted">of ' + fmtCount(row[R.eligible]) + "</span>";
    var acc;
    if (row[R.accLabel] === "no reference" || referenceName(payload, row[R.ref]) === "no reference") {
      acc = '<span class="acc-muted">no reference</span>';
    } else {
      acc = badge(row[R.accLabel], "not rated") + (row[R.accText] ? " " + esc(row[R.accText]) : "") +
        ' <span class="acc-muted">' + fmtCount(row[R.compared]) + " compared</span>";
    }
    return '<li class="acc-compact-row"><span class="acc-compact-name">' + name + "</span>" +
      '<span class="acc-compact-avail">' + avail + "</span>" +
      '<span class="acc-compact-acc">' + acc + "</span></li>";
  }

  function renderAirport(payload, icao) {
    var a = null;
    (payload.airports || []).forEach(function (x) { if (x.icao === icao) a = x; });
    if (!a) return '<p class="acc-muted">No airport ' + esc(icao) + " on this page.</p>";
    var rows = (payload.rows || {})[icao] || {};
    var mov = payload.scope === "europe" ? "movements in the Network Manager records"
                                         : "movements in OPDI's flight list";
    var out = '<article class="acc-card acc-airport">';
    out += '<header class="acc-card-head"><h3 class="acc-card-title">' + esc(a.name) + "</h3>" +
      '<p class="acc-meta"><code>' + esc(a.icao) + "</code> · " + esc(a.country) +
      " · rank " + esc(a.rank) + " · " + fmtCount(a.movements) + " " + mov + "</p>" +
      '<p class="acc-refnote">' + esc(referenceNote(a)) + "</p></header>";
    out += warnings(payload, a);

    CARD_ROWS.forEach(function (cr) {
      var tiles = cr[1].map(function (an) {
        var m = mainBy(payload, an);
        return m ? tile(payload, m, rows[m.item]) : "";
      }).join("");
      if (tiles) {
        out += '<section class="acc-group"><h4 class="acc-group-title">' + esc(cr[0]) + "</h4>" +
          '<div class="acc-tiles">' + tiles + "</div></section>";
      }
    });

    var mainItems = (payload.main || []).map(function (m) { return m.item; });
    var groups = [];
    var byGroup = {};
    var n = 0;
    Object.keys(payload.items || {}).forEach(function (k) {
      if (mainItems.indexOf(k) >= 0 || !rows[k]) return;
      var g = payload.items[k].group || "Other";
      if (!byGroup[g]) { byGroup[g] = []; groups.push(g); }
      byGroup[g].push(compactRow(payload, k, rows[k]));
      n++;
    });
    if (n) {
      out += '<details class="acc-others"><summary>Every other item at this airport (' + n + ")</summary>";
      groups.forEach(function (g) {
        out += '<h5 class="acc-others-group">' + esc(g) + '</h5><ul class="acc-compact">' +
          byGroup[g].join("") + "</ul>";
      });
      out += "</details>";
    }
    return out + "</article>";
  }

  // -------------------------------------------------------------- item view

  function renderItem(payload, itemOrAnchor) {
    var m = mainBy(payload, itemOrAnchor);
    var key = m ? m.item : itemOrAnchor;
    var it = (payload.items || {})[key];
    if (!m && !it) return '<p class="acc-muted">No item ' + esc(itemOrAnchor) + " on this page.</p>";
    var byAvail = payload.scope === "world";
    var col = byAvail ? R.ownLabel : R.accLabel;
    var labels = ((payload.labels || {})[byAvail ? "availability" : "accuracy"] || []).slice();
    var emptyWord = byAvail ? "no flights" : "not rated";
    var members = {};
    (payload.airports || []).forEach(function (a) {
      var row = ((payload.rows || {})[a.icao] || {})[key];
      if (!row) return;
      var label = row[col] || emptyWord;
      if (labels.indexOf(label) < 0) labels.push(label);
      (members[label] = members[label] || []).push(a);
    });
    var total = 0;
    labels.forEach(function (l) { total += (members[l] || []).length; });

    var out = '<article class="acc-card acc-item"><header class="acc-card-head">' +
      '<h3 class="acc-card-title">' + code(m ? m.name : it.name) + "</h3>";
    if (it && it.page) {
      out += '<p class="acc-meta"><a href="' + pageHref(it.page) + '">How this item is computed</a>' +
        (it.group ? " · " + esc(it.group) : "") + "</p>";
    }
    out += '<p class="acc-refnote">' + (byAvail
      ? "Airports by availability: the share of OPDI's flights that carry the item."
      : "Airports by accuracy: how often OPDI's value agrees with the reference records.") +
      "</p></header>";

    if (!total) return out + '<p class="acc-muted">No airport has this item.</p></article>';

    out += '<div class="acc-stack" role="img" aria-label="' + esc(labels.filter(function (l) {
      return members[l];
    }).map(function (l) { return l + ": " + members[l].length; }).join(", ")) + '">';
    labels.forEach(function (l) {
      var c = (members[l] || []).length;
      if (c) out += '<span class="acc-stack-seg ' + labelClass(l) + '" style="flex-grow:' + c + '"></span>';
    });
    out += "</div>";

    labels.forEach(function (l) {
      var list = members[l] || [];
      if (!list.length) return;
      out += '<details class="acc-label-group" open><summary>' + badge(l) + " " +
        plural(list.length, "airport", "airports") + '</summary><div class="acc-chips">' +
        list.map(function (a) {
          return '<a class="acc-chip" href="#airport-' + esc(a.icao) + '" title="' + esc(a.name) + '">' +
            esc(a.icao) + "</a>";
        }).join("") + "</div></details>";
    });
    return out + "</article>";
  }

  // ----------------------------------------------------------- overall view

  function renderOverallItem(payload, item) {
    var m = mainBy(payload, item);
    var key = m ? m.item : item;
    var it = (payload.items || {})[key];
    var rows = (payload.overall || {})[key] || [];
    if (!it && !rows.length) return '<p class="acc-muted">No item ' + esc(item) + " on this page.</p>";
    var out = '<article class="acc-card acc-overall"><header class="acc-card-head">' +
      '<h3 class="acc-card-title">' + code(m ? m.name : it.name) + "</h3>";
    var links = [];
    if (it && it.page) links.push('<a href="' + pageHref(it.page) + '">How this item is computed</a>');
    if (m) {
      links.push("Per airport: <a href=\"europe.html#" + esc(m.anchor) + "\">European airports</a> / " +
        "<a href=\"world.html#" + esc(m.anchor) + "\">worldwide</a>");
    }
    if (links.length) out += '<p class="acc-meta">' + links.join(" · ") + "</p>";
    out += "</header>";

    rows.forEach(function (r) {
      var ref = referenceName(payload, r[O.ref]);
      var noRef = ref === "no reference" || r[O.accLabel] === "no reference";
      function dd(term, value) {
        return '<div class="acc-field"><dt>' + esc(term) + "</dt><dd>" + value + "</dd></div>";
      }
      out += '<section class="acc-refblock"><h4 class="acc-group-title">' +
        (noRef ? "No reference: availability only" : "Against " + esc(ref)) + '</h4><dl class="acc-fields">';
      out += dd("Values per day", fmtCount(r[O.perDay]));
      out += dd("Flights that could carry it", fmtCount(r[O.eligible]));
      out += dd("Availability among these flights", fmtPct(r[O.ownPct]) + " " + badge(r[O.ownLabel]));
      if (!noRef) {
        out += dd("Compared with", r[O.comparedWith] ? esc(r[O.comparedWith]) : DASH);
        out += dd("Movements in the reference", fmtCount(r[O.refMov]));
        out += dd("Found for these movements", fmtPct(r[O.refPct]) + " " + badge(r[O.refLabel]));
        out += dd("Values compared", fmtCount(r[O.compared]));
        out += dd("Accuracy", badge(r[O.accLabel], "not rated") + (r[O.accText] ? " " + esc(r[O.accText]) : ""));
        out += dd("Median difference", fmtSecs(r[O.median]));
        out += dd("90th percentile of the difference", fmtSecs(r[O.p90]));
      }
      out += "</dl></section>";
    });
    return out + "</article>";
  }

  function renderError(src) {
    return '<div class="acc-error" role="alert"><p>The figures could not be loaded from <code>' + esc(src) +
      "</code>. Every figure is also in the CSV files, such as <code>airport_accuracy_europe.csv</code>, " +
      'listed under <a href="#all-items">Every other item</a>.</p></div>';
  }

  // ------------------------------------------------------------------ mount

  var uid = 0;

  function mount(el) {
    var src = el.getAttribute("data-src");
    el.innerHTML = '<p class="acc-muted">Loading the figures…</p>';
    fetch(src)
      .then(function (r) { if (!r.ok) throw new Error(r.status); return r.json(); })
      .then(function (payload) { build(el, payload); })
      .catch(function () { el.innerHTML = renderError(src); });
  }

  function build(el, payload) {
    var id = "acc-lookup-" + (++uid);
    var overall = payload.scope === "overall";
    var placeholder = overall
      ? "Search an item (take-off, stand, AOBT…)"
      : "Search an airport (ICAO, name, country) or an item (take-off, stand, AOBT…)";
    el.innerHTML =
      '<div class="acc-search">' +
      '<label class="visually-hidden" for="' + id + '-q">Search</label>' +
      '<input type="search" id="' + id + '-q" class="form-control acc-input" autocomplete="off" ' +
      'role="combobox" aria-autocomplete="list" aria-expanded="false" aria-controls="' + id + '-list" ' +
      'placeholder="' + esc(placeholder) + '">' +
      '<ul class="acc-suggest" id="' + id + '-list" role="listbox" hidden></ul>' +
      '<p class="acc-nomatch acc-muted" hidden></p></div>' +
      '<div class="acc-quick">' + (payload.main || []).map(function (m) {
        return '<a class="acc-chip acc-quick-chip" href="#' + esc(m.anchor) + '">' + esc(m.title) + "</a>";
      }).join("") + "</div>" +
      '<div class="acc-view" aria-live="polite"></div>';

    var input = el.querySelector(".acc-input");
    var list = el.querySelector(".acc-suggest");
    var nomatch = el.querySelector(".acc-nomatch");
    var view = el.querySelector(".acc-view");
    var results = [];
    var active = -1;

    function close() {
      list.hidden = true;
      list.innerHTML = "";
      input.setAttribute("aria-expanded", "false");
      input.removeAttribute("aria-activedescendant");
      active = -1;
    }

    function setActive(i) {
      var opts = list.querySelectorAll("[role=option]");
      if (!opts.length) return;
      active = (i + opts.length) % opts.length;
      opts.forEach(function (o, j) { o.setAttribute("aria-selected", j === active ? "true" : "false"); });
      input.setAttribute("aria-activedescendant", opts[active].id);
      opts[active].scrollIntoView({ block: "nearest" });
    }

    function show(hash) {
      var h = decodeURIComponent((hash || "").replace(/^#/, ""));
      var html = null;
      if (!overall && h.indexOf("airport-") === 0) html = renderAirport(payload, h.slice(8));
      else if (h.indexOf("item-") === 0) {
        var k = h.slice(5);
        html = overall ? renderOverallItem(payload, k) : renderItem(payload, k);
      } else if (mainBy(payload, h) && mainBy(payload, h).anchor === h) {
        html = overall ? renderOverallItem(payload, h) : renderItem(payload, h);
      }
      if (html === null) return false;
      view.innerHTML = html;
      return true;
    }

    function go(hash) {
      if (show(hash) && window.history && history.replaceState) {
        history.replaceState(null, "", hash);
      }
    }

    function choose(r) {
      input.value = plain(r.label);
      close();
      nomatch.hidden = true;
      go(r.kind === "airport" ? "#airport-" + r.key : itemHash(payload, r.key));
    }

    input.addEventListener("input", function () {
      results = search(payload, input.value);
      if (!input.value.trim()) { close(); nomatch.hidden = true; return; }
      if (!results.length) {
        close();
        nomatch.textContent = "No airport or item matches “" + input.value.trim() + "”.";
        nomatch.hidden = false;
        return;
      }
      nomatch.hidden = true;
      list.innerHTML = results.map(function (r, i) {
        return '<li role="option" id="' + id + "-o" + i + '" aria-selected="false" data-i="' + i + '">' +
          '<span class="acc-opt-label">' + code(r.label) + "</span>" +
          (r.sub ? ' <span class="acc-opt-sub">' + esc(r.sub) + "</span>" : "") + "</li>";
      }).join("");
      list.hidden = false;
      input.setAttribute("aria-expanded", "true");
      active = -1;
    });

    input.addEventListener("keydown", function (e) {
      if (e.key === "ArrowDown") { e.preventDefault(); if (!list.hidden) setActive(active + 1); }
      else if (e.key === "ArrowUp") { e.preventDefault(); if (!list.hidden) setActive(active - 1); }
      else if (e.key === "Enter") {
        if (results.length && !list.hidden) { e.preventDefault(); choose(results[active < 0 ? 0 : active]); }
      } else if (e.key === "Escape") { close(); }
    });

    // mousedown, so the choice lands before the input loses focus.
    list.addEventListener("mousedown", function (e) {
      var li = e.target.closest("[role=option]");
      if (!li) return;
      e.preventDefault();
      choose(results[Number(li.getAttribute("data-i"))]);
    });

    input.addEventListener("blur", function () { setTimeout(close, 150); });

    // Chips and in-card links that open another view stay on the widget.
    el.addEventListener("click", function (e) {
      var a = e.target.closest("a[href^='#']");
      if (!a) return;
      var hash = a.getAttribute("href");
      if (show(hash)) {
        e.preventDefault();
        if (window.history && history.replaceState) history.replaceState(null, "", hash);
        view.scrollIntoView({ block: "start", behavior: "smooth" });
      }
    });

    window.addEventListener("hashchange", function () { show(location.hash); });
    show(location.hash);
  }

  var AccLookup = {
    esc: esc, fold: fold, search: search,
    fmtPct: fmtPct, fmtCount: fmtCount, fmtSecs: fmtSecs, labelClass: labelClass,
    renderAirport: renderAirport, renderItem: renderItem,
    renderOverallItem: renderOverallItem, renderError: renderError, mount: mount
  };

  if (typeof module !== "undefined") module.exports = AccLookup;
  else {
    window.AccLookup = AccLookup;
    var start = function () { document.querySelectorAll(".acc-lookup").forEach(AccLookup.mount); };
    if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
    else start();
  }
})();
