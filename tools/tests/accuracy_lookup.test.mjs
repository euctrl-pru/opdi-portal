import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import { createRequire } from "node:module";

const L = createRequire(import.meta.url)("../../assets/js/accuracy-lookup.js");
const load = (s) => JSON.parse(fs.readFileSync(`assets/pages/accuracy_lookup_${s}.json`, "utf8"));
const p = load("europe");
const world = load("world");
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

test("renderAirport KATL on the world page", () => {
  const h = L.renderAirport(world, "KATL");
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
  assert.ok(h.includes("europe.html#take-off"));
  clean(h);
});

test("renderError links to the downloads", () => {
  const h = L.renderError("x.json");
  assert.ok(h.includes("airport_accuracy_europe.csv") || h.includes('href="#all-items"'));
});
