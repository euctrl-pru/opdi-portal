import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import { createRequire } from "node:module";

const L = createRequire(import.meta.url)("../../assets/js/accuracy-lookup.js");
const load = (s) => JSON.parse(fs.readFileSync(`assets/pages/accuracy_lookup_${s}.json`, "utf8"));
const p = load("airports");
const overall = load("overall");
const clean = (html) => {
  for (const bad of ["NaN", "undefined", "null"]) assert.ok(!html.includes(bad), `contains ${bad}`);
};

test("fold removes case and accents", () => {
  assert.equal(L.fold("İstanbul"), "istanbul");
  assert.equal(L.fold("Málaga"), "malaga");
});

test("search finds an exact ICAO first", () => {
  const r = L.search(p, "egll")[0];
  assert.equal(r.kind, "airport");
  assert.equal(r.key, "EGLL");
});

test("search ignores accents", () => {
  const ist = L.search(p, "istanbul").map((r) => r.key);
  assert.ok(ist.includes("LTFM") && ist.includes("LTFJ"), ist.join());
  assert.ok(L.search(p, "malaga").map((r) => r.key).includes("LEMG"));
});

test("search finds items by MAIN title", () => {
  const r = L.search(p, "take-off")[0];
  assert.equal(r.kind, "item");
  assert.equal(r.key, "ATOT");
});

test("search limits and empties", () => {
  assert.equal(L.search(p, "").length, 0);
  assert.equal(L.search(p, "zzzz").length, 0);
  assert.ok(L.search(p, "a").length <= 8);
});

test("overall search covers items only", () => {
  const r = L.search(overall, "a");
  assert.ok(r.length > 0 && r.every((x) => x.kind === "item"));
});

test("formatters", () => {
  assert.equal(L.fmtPct(null), "—");
  assert.equal(L.fmtPct(95.6), "95.6 %");
  assert.equal(L.fmtCount(2154), "2,154");
  assert.equal(L.fmtCount(null), "—");
  assert.equal(L.fmtSecs(34), "34 s");
  assert.equal(L.fmtSecs(null), "—");
});

test("labelClass", () => {
  assert.equal(L.labelClass("too few compared"), "acc-none");
  assert.equal(L.labelClass("high"), "acc-good");
  assert.equal(L.labelClass("partial"), "acc-fair");
  assert.equal(L.labelClass("limited"), "acc-poor");
  assert.equal(L.labelClass(""), "acc-none");
});

test("renderAirport LFPG", () => {
  const h = L.renderAirport(p, "LFPG");
  assert.ok(h.includes("Charles de Gaulle"));
  assert.ok(h.includes("94.5 % within 60 s"));
  assert.ok(h.includes("high"));
  clean(h);
});

test("renderAirport LGIR with empty values", () => {
  clean(L.renderAirport(p, "LGIR"));
});

test("renderAirport KATL, an airport outside Europe", () => {
  const h = L.renderAirport(p, "KATL");
  assert.ok(h.includes("availability only"));
  clean(h);
});

test("renderAirport escapes payload strings", () => {
  const q = structuredClone(p);
  q.airports[0].name = "<img src=x>";
  assert.ok(!L.renderAirport(q, q.airports[0].icao).includes("<img"));
});

test("renderItem off-block", () => {
  const h = L.renderItem(p, "off-block");
  assert.ok(h.includes("limited"));
  assert.ok(h.includes("LFPG"));
  clean(h);
  assert.equal(L.renderItem(p, "AOBT"), h);
});

test("renderOverallItem ATOT", () => {
  const h = L.renderOverallItem(overall, "ATOT");
  assert.ok(h.includes("93.2 % within 60 s"));
  assert.ok(h.includes("airports.html#take-off"));
  clean(h);
});

test("renderError links to the downloads", () => {
  const h = L.renderError("x.json");
  assert.ok(h.includes("airport_accuracy_europe.csv") || h.includes('href="#all-items"'));
});

// ------------------------------------------------- merged airports payload
const ap = load("airports");

test("renderAirport LTFM shows both ranks and limitation links", () => {
  const h = L.renderAirport(ap, "LTFM");
  assert.ok(h.includes("Europe") && h.includes("rank 1") && h.includes("16"), h.slice(0, 600));
  assert.ok(h.includes("rank 1 in Europe · rank 16 worldwide"));
  assert.ok(h.includes('href="../limitations.html#lim-take-off-landing"'));
  assert.ok(h.includes('href="#ground-reception"'));
  clean(h);
});

test("renderAirport KATL: availability only, with limitation links", () => {
  const h = L.renderAirport(ap, "KATL");
  assert.ok(h.includes("availability only"));
  assert.ok(h.includes("limitations.html#lim-"));
  assert.ok(h.includes("rank 3 worldwide") && !h.includes("in Europe ·"));
  clean(h);
});

test("renderItem follows the list filter", () => {
  const world = L.renderItem(ap, "take-off", { list: "world" });
  assert.ok(!world.includes("LGIR") && world.includes("KATL"));
  const eu = L.renderItem(ap, "take-off", { list: "europe" });
  assert.ok(!eu.includes("KATL") && eu.includes("LGIR"));
  const all = L.renderItem(ap, "take-off");
  assert.ok(all.includes("KATL") && all.includes("LGIR"));
  clean(all);
});

test("search follows the list filter", () => {
  assert.equal(L.search(ap, "katl", 8, { list: "europe" }).filter((r) => r.kind === "airport").length, 0);
  assert.equal(L.search(ap, "katl")[0].key, "KATL");
  assert.equal(L.search(ap, "lgir", 8, { list: "world" }).filter((r) => r.kind === "airport").length, 0);
});

test("routeHash never throws and is case-insensitive", () => {
  assert.equal(L.routeHash(ap, "#%E0%A4"), null);
  assert.deepEqual(L.routeHash(ap, "#airport-egll"), { view: "airport", key: "EGLL" });
  assert.deepEqual(L.routeHash(ap, "#take-off"), { view: "item", key: "ATOT" });
  assert.deepEqual(L.routeHash(ap, "#item-AOBT"), { view: "item", key: "AOBT" });
  assert.equal(L.routeHash(ap, "#airport-zzzz"), null);
  assert.equal(L.routeHash(ap, "#nothing"), null);
  assert.equal(L.routeHash(ap, ""), null);
  assert.equal(L.routeHash(overall, "#airport-EGLL"), null);
});

test("nextIndex wraps and starts from either end", () => {
  assert.equal(L.nextIndex(-1, -1, 5), 4);
  assert.equal(L.nextIndex(-1, 1, 5), 0);
  assert.equal(L.nextIndex(4, 1, 5), 0);
  assert.equal(L.nextIndex(0, -1, 5), 4);
  assert.equal(L.nextIndex(2, 1, 5), 3);
  assert.equal(L.nextIndex(-1, 1, 0), -1);
});

test("inputLabel follows the view", () => {
  assert.equal(L.inputLabel(ap, "item", "ATOT"), "Take-off time (ATOT)");
  assert.equal(L.inputLabel(ap, "airport", "LTFM"), "İstanbul Airport");
  assert.equal(L.inputLabel(ap, "item", "flight list: TYPECODE"), "typecode (flight list)");
});

test("a null compared count is not written", () => {
  const q = structuredClone(ap);
  for (const row of Object.values(q.rows.LFPG)) { row[7] = null; row[10] = "too few compared"; row[3] = 0; }
  const h = L.renderAirport(q, "LFPG");
  assert.ok(!h.includes("— compared"));
  clean(h);
});

test("items link to their limitations", () => {
  const h = L.renderAirport(p, "LFPG");
  assert.ok(h.includes('href="../limitations.html#lim-block-times"'));
  assert.ok(L.renderOverallItem(overall, "ATOT").includes('href="../limitations.html#lim-take-off-landing"'));
});

test("under All, airports without a reference are grouped by availability apart", () => {
  const h = L.renderItem(ap, "take-off", { list: "all" });
  const cut = h.indexOf("Not compared");
  assert.ok(cut > 0, "no Not compared section");
  const compared = h.slice(0, cut);
  const notCompared = h.slice(cut);
  assert.ok(!compared.includes("#airport-KATL") && notCompared.includes("#airport-KATL"));
  assert.ok(compared.includes("#airport-LFPG"));
  assert.ok(!compared.includes(">no reference<"));
  assert.ok(/acc-badge acc-good">high</.test(notCompared));
  clean(h);
  // European top 100 keeps the accuracy groups only; Worldwide top 100 splits like All.
  assert.ok(!L.renderItem(ap, "take-off", { list: "europe" }).includes("Not compared"));
  const world = L.renderItem(ap, "take-off", { list: "world" });
  assert.ok(world.indexOf("#airport-LTFM") < world.indexOf("Not compared"));
  assert.ok(world.indexOf("#airport-KATL") > world.indexOf("Not compared"));
});

test("KATL card: airports compared with Network Manager, other items availability only", () => {
  const h = L.renderAirport(ap, "KATL");
  assert.ok(h.includes("departure and destination airports compared with Network Manager"), h.slice(0, 800));
  assert.ok(!h.includes("No reference: availability only"));
  assert.ok(h.includes("to or from Europe in the Network Manager records"));
  clean(h);
});

test("adep under All and Worldwide: KATL is in an accuracy group", () => {
  for (const list of ["all", "world"]) {
    const h = L.renderItem(ap, "adep", { list });
    const cut = h.indexOf("Not compared");
    const compared = cut > 0 ? h.slice(0, cut) : h;
    assert.ok(compared.includes("#airport-KATL"), list);
    assert.ok(cut < 0 || !h.slice(cut).includes("#airport-KATL"), list);
    clean(h);
  }
});

test("take-off under All: KATL stays in Not compared", () => {
  const h = L.renderItem(ap, "take-off", { list: "all" });
  const cut = h.indexOf("Not compared");
  assert.ok(cut > 0 && h.slice(cut).includes("#airport-KATL") && !h.slice(0, cut).includes("#airport-KATL"));
});

test("ground warning is linked only for an airport on the European list", () => {
  const kclt = ap.airports.find((a) => a.icao === "KCLT");
  assert.ok(kclt.flags.includes("ground") && kclt.ranks.europe == null);
  const h = L.renderAirport(ap, "KCLT");
  assert.ok(h.includes("Ground events are nearly absent here"));
  assert.ok(!h.includes('href="#ground-reception"'));
  assert.ok(L.renderAirport(ap, "LTFM").includes('href="#ground-reception"'));
});

test("renderError points to the right section for each scope", () => {
  assert.ok(L.renderError("x.json", "airports").includes('href="#all-items">Every other item</a>'));
  assert.ok(L.renderError("x.json", "overall").includes('href="#all-items">Downloads</a>'));
});
