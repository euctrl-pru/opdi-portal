# Accuracy Lookup and Data Menu Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the wide per-airport and per-item tables on the three Accuracy pages with a search box that shows one airport or one item as a readable card, and move Limitations and Accuracy and availability into the Data menu of the sidebar.

**Architecture:** `tools/gen_accuracy_tables.py` stays the only source of the accuracy figures. It now also writes one compact JSON file per page (`assets/pages/accuracy_lookup_{europe,world,overall}.json`) and a small include per page (`content/accuracy/_generated/{europe,world,overall}_lookup.md`). The include holds the anchors, a fallback text and the widget's mount point, and it loads one dependency-free script (`assets/js/accuracy-lookup.js`) and stylesheet (`assets/css/accuracy-lookup.css`). The script fetches the JSON and renders the cards in the browser. Its pure functions (search, label classes, number formatting) are exported for `node --test`.

**Tech Stack:** Quarto 1.7 website (theme cosmo, Bootstrap 5), Python 3 standard library (generator), plain ES2020 JavaScript with no libraries, `uvx pytest`, `node --test` (Node 24).

**Spec:** The user's request in this session: *"Instead of showing massive tables, which bug out graphically, make it so that a user could search for an airport / event and get a structured response which is pleasant to read"*, plus *"put accuracy / availability / limitations as sub menus under data"*. The data contract is `assets/accuracy/README.md`. The label rules are on `content/accuracy/index.qmd` ("How to read the labels").

## Global Constraints

- Every figure shown comes from `assets/accuracy/*.csv` through `tools/gen_accuracy_tables.py`. No number is typed into JS, CSS or page prose. New prose on `content/accuracy/*.qmd` may only use numbers already registered in `assets/pages/accuracy_numbers.csv` (`tools/check_site.py` enforces this).
- Availability and accuracy are shown as two separate things and never merged into one score. Every label sits next to its number and to the count it was computed over.
- Labels are the words of the staged tables: availability `high` / `partial` / `low`; accuracy `good` / `fair` / `limited` / `too few compared` / `no reference`. They are shown as words, and colour is only a second cue.
- New page wording must pass the `FORBIDDEN_WORDS` check in `tools/check_site.py` (for example, no "robust", "comprehensive", "pin", "surface" as a verb).
- No new dependency: no CDN script, no npm package, no Python package. The generator stays "plain Python 3, no extra packages".
- The generator's `--check` mode must cover every new file it writes.
- Existing anchors stay valid: `europe.qmd#take-off`, `#landing`, `#off-block`, `#in-block`, `#runway-dep`, `#runway-arr`, `#stand-dep`, `#stand-arr`, `#adep`, `#ades`, `#per-item`, `#all-items`, `#patterns`, `#unreliable`, `#ground-reception`, `#no-times`, and the same item anchors plus `#per-item` and `#all-items` on `world.qmd`.
- Commit messages: a short subject plus a body only where needed. Per the user's global CLAUDE.md, **no `Co-Authored-By` trailer and no "Generated with" line**.

## Decisions taken here

- **Data menu shape.** `Data` gets two nested sections after "Data preview": `Limitations` (entries "Limitations of each item" → `content/limitations.qmd` and "Limits of the dataset" → `content/data/limitations.qmd`, moved out of the flat list) and `Accuracy and availability` (its current three entries). The top-level `Limitations` and `Accuracy and availability` entries are removed. Page paths stay the same, so no link breaks.
- **Kept tables.** These stay as they are, being short or narrow: `europe_patterns`, `europe_limited`, `europe_ground`, `europe_no_times`, `overall_no_reference`, `overall_stability`, `overall_unstable`, `algorithms_labels`. These are no longer written and are deleted: `europe_summary`, `world_summary`, the 20 `{europe,world}_<anchor>` detail tables and `overall_items`.
- **JSON outside the page.** The data is fetched, not inlined, because the inline JSON would be about 800 kB per page. It is written under `assets/pages/`, the folder that already holds generator output and is copied by `resources: assets/`.
- **Deep links.** `#take-off` (any MAIN anchor) opens the item view for that item. `#airport-EGLL` opens that airport's card. Choosing a result updates the hash with `history.replaceState`, so a card can be shared as a link.

## File Structure

| File | Responsibility |
|---|---|
| `_quarto.yml` (modify, sidebar ~lines 52–146) | Menu move |
| `tools/gen_accuracy_tables.py` (modify) | Build the lookup payloads and includes; stop writing the replaced tables |
| `tools/tests/test_gen_tools.py` (modify) | Payload and include tests; replaces `test_every_airport_appears_once_in_each_summary` |
| `tools/tests/test_check_site.py` (modify) | Sidebar shape test |
| `assets/js/accuracy-lookup.js` (create) | Widget: fetch, search, render, hash routing |
| `tools/tests/accuracy_lookup.test.mjs` (create) | `node --test` tests of the pure functions |
| `assets/css/accuracy-lookup.css` (create) | Card, badge and bar styles, light and dark |
| `content/accuracy/europe.qmd`, `world.qmd`, `index.qmd` (modify) | Swap the table sections for the lookup; reword "How to read" |
| `tools/README.md`, `PUBLISHING_CHECKLIST.md` (modify) | Name the new outputs and the `node --test` command |

## Review Focus

1. **The page is opened without a server, or the fetch fails** (`file://`, offline, a 404). Expect a visible message that links to the CSV downloads, not an empty box. Owned by Task 3 (`renderError` test) and Task 4 (noscript and fallback text).
2. **The query matches nothing, or many things** (`"xx"`, `"airport"`, an empty string, upper/lower case, accents such as `"Istanbul"` vs `İstanbul`, `"Malaga"` vs `Málaga`). Expect a "no match" line, at most 8 suggestions, ICAO exact match first, and search that ignores case and accents. Owned by Task 3 (`search` tests).
3. **An airport has no rows, or empty values, for an item** (for example `LGIR`, which has 0 OPDI arrivals; the runway and stand rows have no availability of their own). Expect "no flights" or "—" in place of `NaN %` or `undefined`. Owned by Task 2 (payload uses `null`) and Task 3 (`fmtPct(null)` test).
4. **The world page** has no airport records outside Europe. Its cards must say "availability only, no reference" and must not show an empty accuracy column as if it were a bad score. Owned by Task 2 (`reference` field) and Task 3 (render test for a `no reference` row).
5. **Phone width (~400 px) and dark mode.** The card must stack with no horizontal page scroll, and the badges must stay readable in the Quarto dark theme if it is ever enabled. Owned by Task 4 (visual check at 400 px).

---

### Task 1: Move Limitations and Accuracy into the Data menu

**Files:**
- Modify: `_quarto.yml` (sidebar, currently lines 52–146)
- Test: `tools/tests/test_check_site.py`

**Interfaces:**
- Consumes: nothing (the test reads `_quarto.yml` as text).
- Produces: nothing later tasks use.

- [ ] **Step 1: Write the failing test** `test_quality_pages_sit_under_data` in `tools/tests/test_check_site.py`. It reads the real `_quarto.yml` (`Path(__file__).parents[2] / "_quarto.yml"`) and asserts:
  - the text between `- section: "Data"` and `- section: "Algorithms"` contains `content/limitations.qmd`, `content/data/limitations.qmd`, `content/accuracy/index.qmd`, `content/accuracy/europe.qmd` and `content/accuracy/world.qmd`;
  - it contains `section: "Limitations"` and `section: "Accuracy and availability"`;
  - the text after `- section: "Algorithms"` contains neither `content/limitations.qmd` nor `content/accuracy/`.
- [ ] **Step 2: Run it.** `uvx pytest tools/tests/test_check_site.py::test_quality_pages_sit_under_data -q` → FAIL.
- [ ] **Step 3: Edit the sidebar** as set out under *Decisions taken here*, with the indentation of the existing nested sections (see "Flight list" under Algorithms).
- [ ] **Step 4: Verify.** The test passes. `python3 tools/check_site.py --only sidebar` prints `OK`.
- [ ] **Step 5: Commit** `git commit -m "sidebar: limitations and accuracy under Data"`.

### Task 2: Generator writes lookup payloads and includes

**Files:**
- Modify: `tools/gen_accuracy_tables.py`
- Test: `tools/tests/test_gen_tools.py`
- Generated (commit them): `assets/pages/accuracy_lookup_{europe,world,overall}.json`, `content/accuracy/_generated/{europe,world,overall}_lookup.md`. Delete: `content/accuracy/_generated/{europe,world}_summary.md`, the 20 `{europe,world}_{take-off,landing,off-block,in-block,runway-dep,runway-arr,stand-dep,stand-arr,adep,ades}.md`, `overall_items.md`.

**Interfaces:**
- Consumes: existing `read`, `num`, `by_airport`, `accuracy_text`, `MAIN`, `ITEMS`, `ITEM_NAME`, `ITEM_PAGE`, `GROUND_NEARLY_ABSENT_PCT`, `HEADER`.
- Produces (Task 3 relies on this JSON shape exactly):

```text
LookupPayload = {
  "scope": "europe" | "world" | "overall",
  "labels": {"availability": ["high","partial","low"],
             "accuracy": ["good","fair","limited","too few compared","no reference"]},
  "references": ["airport records", "Network Manager", "no reference"],
  "main": [{"anchor": "take-off", "title": "Take-off", "item": "ATOT", "name": "Take-off time (`ATOT`)"}, ...],  # MAIN, in order
  "items": {"<item>": {"name": "`ATOT`", "page": "../algorithms/take-off-landing.qmd", "group": "Runway and stands"}},
  # scope europe/world only:
  "airports": [{"icao","name","country","rank","movements","where","flags","limited"}],
      # where: "airport records" | "Network Manager" (europe without records) | "outside Europe" | "Europe, no airport records"
      # flags: subset of ["ground", "no_times"], same rules as europe_ground / europe_no_times
      # limited: MAIN anchors whose accuracy label is "limited"
  "rows": {"<ICAO>": {"<item>": Row}},
  # scope overall only:
  "overall": {"<item>": [OverallRow, ...]}   # one per reference, order airport records, Network Manager, no reference
}
Row        = [own_pct, own_label, eligible, ref_idx, ref_movements, ref_pct, ref_label, compared, accuracy_text, median_s, accuracy_label]
OverallRow = [ref_idx, values_per_day, eligible, own_pct, own_label, compared_with, ref_movements, ref_pct, ref_label,
              compared, accuracy_text, median_s, p90_s, accuracy_label]
```

  Percentages are rounded to 1 decimal, counts and seconds to integers, an empty CSV cell becomes `null`, a label becomes `""` when empty, and `ref_idx` indexes `references`. `accuracy_text` is the existing `accuracy_text(r)` string (for example `"94.5 % within 60 s"`).
- New functions:
  - `item_group(page: str) -> str`: maps an algorithm page to its sidebar section title ("Flight list", "Runway and stands", "Vertical profile", "Airspace and terminal area", "Measurements"). `plausibility-checks` and `processing-steps` map to "Other". Copy the mapping from the Algorithms sidebar in `_quarto.yml`.
  - `airport_payload(rows: list[dict], airports_csv: list[dict], scope: str) -> dict`
  - `overall_payload(items: list[dict]) -> dict`
  - `lookup_include(scope: str) -> str`: the Markdown include (exact content in Step 5).
  - `build()` adds the three JSON files (written with `json.dumps(payload, ensure_ascii=False, separators=(",", ":")) + "\n"`) and the three includes to `wanted`, so `--check` covers them.
  - Stale outputs: in write mode, `main()` deletes the `OUT_DIR/*.md` files that are not in `wanted` and start with `HEADER`. In `--check` mode it reports them as `out of date`. Test: `test_stale_generated_tables_are_reported` writes `HEADER + "x"` to `tmp_path/content/accuracy/_generated/old.md` in a copy of the root, built with `shutil.copytree` of `assets/accuracy` and `content/accuracy/_generated` and `content/algorithms/_generated`, and asserts that `main(["--check", "--root", tmp])` returns 1.

- [ ] **Step 1: Write the failing tests** in `tools/tests/test_gen_tools.py`. Replace `test_every_airport_appears_once_in_each_summary` with:
  - `test_lookup_has_every_airport_and_main_item`: for `europe` and `world`, load the JSON; `len(airports) == 100`, the ICAOs equal the CSV's set, and every airport has a `rows` entry for each `main[*].item`.
  - `test_lookup_rows_match_staged_csv`: `rows["LFPG"]["ATOT"]` in europe equals `[95.6, "high", 2154, 0, 2046, 98.8, "high", 2022, "94.5 % within 60 s", 34, "good"]`, and `rows["LTFM"]["ATOT"][10] == "too few compared"`.
  - `test_lookup_flags_follow_the_tables`: europe `LTFM` has `"ground"` in flags, `LGIR` has `"no_times"`, `LMML` has `"take-off"` in `limited`.
  - `test_lookup_empty_cells_are_null`: no value in any `Row` is `""` at the number positions (0, 2, 4, 5, 7, 9) — only `None` or a number.
  - `test_lookup_overall_has_every_item`: `set(overall) == {r["item"] for r in pooled rows}`, and `overall["ATOT"]` has 2 rows, the first with `ref_idx` 0.
  - `test_lookup_files_stay_small`: each JSON file is under 450_000 bytes. If the first build is larger, record the size in the commit body and raise the bound to 1.2× the size measured.
  - `test_lookup_include_keeps_anchors`: `lookup_include("europe")` contains `[]{#take-off}` … `[]{#ades}` and `src="../../assets/js/accuracy-lookup.js"`.
- [ ] **Step 2: Run.** `uvx pytest tools/tests/test_gen_tools.py -q` → the new tests FAIL.
- [ ] **Step 3: Implement** the functions above.
- [ ] **Step 4: Regenerate.** Run `python3 tools/gen_accuracy_tables.py`, then `python3 tools/gen_accuracy_tables.py --check` (exit 0), then `uvx pytest tools/tests -q` (all pass).
- [ ] **Step 5: The include's exact content** (shown for `europe`; `world` and `overall` swap the scope, and `overall` carries no airport anchors):

```markdown
<!-- Written by tools/gen_accuracy_tables.py from assets/accuracy/; do not edit. -->

[]{#take-off}[]{#landing}[]{#off-block}[]{#in-block}[]{#runway-dep}[]{#runway-arr}[]{#stand-dep}[]{#stand-arr}[]{#adep}[]{#ades}

::: {#acc-lookup-europe .acc-lookup data-scope="europe" data-src="../../assets/pages/accuracy_lookup_europe.json"}
The search below needs JavaScript. Every figure is also in the files under [Every other item](#all-items).
:::

<link rel="stylesheet" href="../../assets/css/accuracy-lookup.css">
<script src="../../assets/js/accuracy-lookup.js" defer></script>
```

  In this task `build()` still writes the old tables too, so the pages and `check_site` keep working. Task 4 drops them from `build()` once no page includes them.
- [ ] **Step 6: Commit** `git add tools/gen_accuracy_tables.py tools/tests/test_gen_tools.py assets/pages/accuracy_lookup_*.json content/accuracy/_generated/*_lookup.md` then `git commit -m "accuracy: lookup payloads and includes"`.

### Task 3: The lookup widget

**Files:**
- Create: `assets/js/accuracy-lookup.js`, `assets/css/accuracy-lookup.css`
- Test: `tools/tests/accuracy_lookup.test.mjs`

**Interfaces:**
- Consumes: `LookupPayload` (Task 2), the mount `div.acc-lookup[data-scope][data-src]` (Task 2's include).
- Produces: `window.AccLookup` in the browser, and `module.exports` under Node, with:
  - `fold(s: string) -> string`: lower case with diacritics removed (`s.normalize("NFD").replace(/\p{Diacritic}/gu, "")`), plus `ı`→`i`.
  - `search(payload, query: string, limit = 8) -> Array<{kind: "airport"|"item", key: string, label: string, sub: string}>`. Airports match on ICAO, name and country; items match on the plain name, the item key and the MAIN title (for example "take-off", "stand", "AOBT"). Order: exact ICAO, then prefix matches, then substring matches, airports before items at equal rank. An empty query returns `[]`.
  - `fmtPct(v: number|null) -> string` (`"95.6 %"` or `"—"`), `fmtCount(v)` (`"2,154"` or `"—"`), `fmtSecs(v)` (`"34 s"` or `"—"`).
  - `labelClass(label: string) -> string`: `"acc-good"` for good/high, `"acc-fair"` for fair/partial, `"acc-poor"` for limited/low, `"acc-none"` for too few compared/no reference/"".
  - `renderAirport(payload, icao) -> string` (HTML), `renderItem(payload, itemOrAnchor) -> string`, `renderOverallItem(payload, item) -> string`, `renderError(src: string) -> string`.
  - `mount(el: HTMLElement)`: fetches `data-src`, builds the search box, and routes the hash.

**What each view shows** (the layout decisions; the markup is up to you):
- **Search box:** one `<input type="search">` with the placeholder "Search an airport (ICAO, name, country) or an item (take-off, stand, AOBT…)", and a suggestion list under it (`role="listbox"`, arrow keys plus Enter, click). Under it, a row of quick chips for the ten MAIN items.
- **Airport card:** header with name, `ICAO`, country, rank and movements, and a one-line reference note ("Compared with airport records" / "with Network Manager only" / "No reference: availability only"). Warning callouts for `flags` and `limited`, worded as on the page sections they link to (`#ground-reception`, `#no-times`, `#unreliable`). Then the MAIN items in five rows of two: Runway times (take-off, landing), Block times, Runways, Stands, Airports. Each item tile shows its name, an availability bar with the % and the badge, "of N flights", and the accuracy badge with `accuracy_text`, the median and "N compared". Finally `<details>` "Every other item at this airport (n)", grouped by `items[*].group`, as compact rows.
- **Item view (europe/world):** title and link to the algorithm page; a stacked bar of the airport count per accuracy label (availability label on the world page); then one collapsible group per label listing the airports as chips (`ICAO` with the name in a tooltip). A chip opens that airport's card.
- **Overall view (index):** the search covers items only. The card shows one block per reference row with every field of `OverallRow`, labelled in words, plus "Per airport: European airports / worldwide" links to `europe.qmd#<anchor>` when the item is in MAIN.
- Every badge shows the label word. Colour comes from `labelClass`, defined in the CSS with variables for light mode and for `.quarto-dark`.

- [ ] **Step 1: Write the failing tests** in `tools/tests/accuracy_lookup.test.mjs`, using `node:test` and `node:assert`. The fixture is `JSON.parse(fs.readFileSync("assets/pages/accuracy_lookup_europe.json"))`. Load the module with `createRequire(import.meta.url)("../../assets/js/accuracy-lookup.js")`.
  - `search(p, "egll")[0]` equals `{kind: "airport", key: "EGLL", …}`.
  - `search(p, "istanbul")` includes `LTFM` and `LTFJ`; `search(p, "malaga")` includes `LEMG`.
  - `search(p, "take-off")[0].kind === "item"` and its key is `"ATOT"`.
  - `search(p, "")` has length 0; `search(p, "zzzz")` has length 0; `search(p, "a").length <= 8`.
  - `fmtPct(null) === "—"`, `fmtPct(95.6) === "95.6 %"`, `fmtCount(2154) === "2,154"`.
  - `labelClass("too few compared") === "acc-none"`, `labelClass("high") === "acc-good"`.
  - `renderAirport(p, "LFPG")` contains `"Charles de Gaulle"`, `"94.5 % within 60 s"` and `"high"`, and does not contain `"NaN"`, `"undefined"` or `"null"`.
  - `renderAirport(p, "LGIR")` (empty values) contains none of `"NaN"`, `"undefined"`, `"null"`.
  - `renderAirport(world, "KATL")`, using the world JSON, contains `"availability only"`.
  - `renderItem(p, "off-block")` contains `"limited"` and `"LFPG"`.
  - `renderError("x.json")` contains `"airport_accuracy_europe.csv"` or a link to `#all-items`.
- [ ] **Step 2: Run.** `node --test tools/tests/` → FAIL (module missing).
- [ ] **Step 3: Implement** `accuracy-lookup.js` as an IIFE that assigns `AccLookup`. At the end: `if (typeof module !== "undefined") module.exports = AccLookup; else document.addEventListener("DOMContentLoaded", () => document.querySelectorAll(".acc-lookup").forEach(AccLookup.mount));`. Escape every string from the payload before it goes into HTML (`esc()`). Render backtick spans in names as `<code>`.
- [ ] **Step 4: Implement** `accuracy-lookup.css`: tiles in a CSS grid `repeat(auto-fit, minmax(16rem, 1fr))`, badges as pills, bars 6 px high. No `min-width` wider than 20rem anywhere.
- [ ] **Step 5: Verify.** `node --test tools/tests/` passes.
- [ ] **Step 6: Commit** `git commit -m "accuracy: airport and item lookup widget"`.

### Task 4: Pages use the lookup

**Files:**
- Modify: `content/accuracy/europe.qmd`, `content/accuracy/world.qmd`, `content/accuracy/index.qmd`, `tools/README.md`, `PUBLISHING_CHECKLIST.md`
- Delete (by running the generator): the outputs listed in Task 2.

**Interfaces:**
- Consumes: the `{scope}_lookup.md` includes (Task 2) and the widget (Task 3).

- [ ] **Step 1: europe.qmd.** Replace "## The 100 airports" (the `europe_summary` include) with `## Look up an airport or an item {#lookup}` plus `{{< include _generated/europe_lookup.md >}}`. Rewrite "How to read this page" so it describes the card (availability bar plus label, then accuracy label with the share, the median and the count compared), keeping the label definitions word for word. Replace the whole "## Per item {#per-item}" section (all ten callouts) with one short paragraph under `## Per item {#per-item}` that says to type an item's name in the search, or to click one of the item chips, to see every airport grouped by its label. Keep "Patterns", "Where an item is unreliable", "Where ground events are nearly absent", "Where no take-off or landing time exists" and "Every other item" unchanged.
- [ ] **Step 2: world.qmd.** The same, with `world_lookup.md`.
- [ ] **Step 3: index.qmd.** Under "## Every item", replace the `overall_items` include with `overall_lookup.md`. Keep the paragraph after it ("Counts are given to four significant figures …").
- [ ] **Step 4: Regenerate and check.** Remove the summary, detail and `overall_items` entries from `europe_tables`, `world_tables`, `overall_tables` and `build()` (keep the facts and the kept tables). Then run `python3 tools/gen_accuracy_tables.py`, which deletes the replaced outputs, then `python3 tools/check_site.py` → `OK`, then `uvx pytest tools/tests -q` and `node --test tools/tests/` → all pass. If `check_site` flags a number in the new prose, reword it to avoid the number rather than registering a new one.
- [ ] **Step 5: Docs.** In `tools/README.md`, list the three JSON files and three includes among the outputs of `gen_accuracy_tables.py`, and add `node --test tools/tests/` under "Tests". In `PUBLISHING_CHECKLIST.md`, add `- [ ] node --test tools/tests/ passes.` after the pytest line.
- [ ] **Step 6: Render and look.** Run `quarto render content/accuracy/` and then `quarto preview` (the fetch needs a server). In a browser:
  - europe: type "heath" → Enter → the EGLL card shows ten tiles with no `NaN` or `undefined`; the `#airport-EGLL` hash is set; reload keeps the card.
  - open `europe.html#off-block` → the item view shows the off-block groups; click `LFPG` → its card.
  - `LTFM` card shows the ground and limited warnings with working links.
  - world: `KATL` card says availability only.
  - index: search "holding" → the `holding-start` card.
  - at 400 px width, no horizontal page scroll; the sidebar shows Data ▸ Limitations and Data ▸ Accuracy and availability.
  - stop the server and open the HTML from `file://` → the error text with the CSV links shows.
- [ ] **Step 7: Commit** `git add -A content/accuracy tools/README.md PUBLISHING_CHECKLIST.md` then `git commit -m "accuracy pages: search lookup replaces the wide tables"`. Do not stage `_site/`, `.env.template`, `.gitignore` or the CERN logo; they are unrelated working-tree changes.

### Task 5: Condense the Data section

**Scope:** every page under the Data menu after Task 1: `content/data/index.qmd`, `access.qmd`, `dictionary.qmd`, `releases.qmd`, `limitations.qmd`, `v0.0.2.qmd`, `content/data-preview.qmd`, `content/limitations.qmd`, `content/accuracy/index.qmd`, `europe.qmd`, `world.qmd`. That is about 19,000 words now. Do it after Task 4, so the accuracy pages are condensed in their new form. Do it as five sub-tasks, each with its own review and commit:

- **5a.** `data/index.qmd`, `access.qmd`
- **5b.** `dictionary.qmd`. The generated column tables are not edited; only the prose around them is.
- **5c.** `releases.qmd`, `v0.0.2.qmd`, `data-preview.qmd`
- **5d.** `limitations.qmd`, `data/limitations.qmd`
- **5e.** `accuracy/index.qmd`, `europe.qmd`, `world.qmd`

**Goal (user's words):** "condense the text … I want the facts to stay the same, just shorter and easier to read. Avoid repetition. Make the data section of the page easy and quick to navigate."

**Rules:**
- **Facts unchanged.** Every fact, number, condition, caveat, link target and recommendation on the old page is still on the new page, or on the one page it now links to (when the fact was repeated). Nothing new is claimed.
- **Say each thing once.** When two pages explain the same thing, keep it on the page where it belongs and link there from the other. Known repeats:
  - the label definitions (`high`/`partial`/`low`, `good`/…/`no reference`): keep the full table on `accuracy/index.qmd#how-to-read-the-labels`; europe and world give a one-line reminder and the link;
  - the three days measured: define them on `accuracy/index.qmd` and refer to them elsewhere;
  - ADS-B reception and receiver coverage, on `data/limitations.qmd` versus `limitations.qmd#general-limits`;
  - "what is published" on `data/index.qmd` versus `dictionary.qmd`;
  - the release status text on `data/index.qmd`, `releases.qmd` and `v0.0.2.qmd`.
- **Quick to navigate:**
  - every page opens with at most 3 sentences on what it answers, and nothing else before the first heading except a release callout;
  - headings are short and name the thing ("Trino", "Python", "Daily files");
  - `data/index.qmd` replaces "The pages of this section" with a two-column table (page | what you find there) and moves it to directly after the intro;
  - prefer lists and short tables to long paragraphs where the content is a list of cases;
  - in `limitations.qmd`, keep the per-item three-heading pattern ("When it is missing", "When it can be wrong", "What not to conclude from it") and make each a short list.
- **Do not break:**
  - every heading anchor linked from elsewhere. Run `python3 tools/check_site.py --only anchor`. Keep a renamed heading's old id with `{#old-id}`; for example `v0.0.2.qmd#how-release-v0` and `#flights-and-their-identifiers` are linked;
  - every R chunk and inline `` `r …` `` expression (release text comes from `R/portal_config.R`);
  - every `[...]{.measured}` span, unchanged inside;
  - on `content/accuracy/*`, only numbers registered in `assets/pages/accuracy_numbers.csv`;
  - `limitations.qmd` must still name every published item (the `check_site` coverage check), and `dictionary.qmd` every event type as inline code;
  - the generated includes; the `FORBIDDEN_WORDS` list;
  - the house style: plain words, defining a term the first time, British spelling as now.
- **Target:** about 35–50 % fewer words per page, as measured by `wc -w` (code chunks count, so tables-heavy pages will shrink less). This is a guide; the facts rule wins.

**Steps per sub-task:**
- [ ] **Step 1: Record facts.** Run `git show HEAD:<page> > /tmp/...` (scratchpad). List the page's facts as a numbered checklist in the scratchpad, one line each.
- [ ] **Step 2: Rewrite** the page(s) under the rules.
- [ ] **Step 3: Verify.** `python3 tools/check_site.py` → `OK`. `uvx pytest tools/tests -q` and `node --test tools/tests/` pass. `quarto render <page>` succeeds. Compare `wc -w` before and after.
- [ ] **Step 4: Fact review.** The reviewer gets the old page, the new page and the checklist. They mark each fact as kept / moved (where) / lost, and flag anything new. Any "lost" or "new" fails the review.
- [ ] **Step 5: Commit** `git commit -m "data pages: condense <pages>"`, with the word counts before and after in the body.
