# OPDI Data Menu Cleanup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reorganise the Data part of the site into one "OPDI Data" menu (with Algorithms and a single Data quality section), merge overlapping pages, and remove every mention of the method studies.

**Architecture:** Pages are merged in place where their paths carry many inbound links (`content/limitations.qmd` keeps its `#lim-*` ids). Removed pages leave Quarto `aliases:` on the page that absorbs them, and every inbound link is rewritten (`tools/check_site.py` enforces links and anchors). The accuracy generator gains one merged airport payload (Europe and world together). Each item there carries a link to its limitations section, so one airport card shows availability, accuracy and limitations per item.

**Tech Stack:** Quarto 1.7 website, plain Python 3 generators in `tools/`, a vanilla JS widget (`assets/js/accuracy-lookup.js`), `uvx pytest`, `node --test tools/tests/*.test.mjs`.

**Spec:** The user's message of 2026-11-18, verbatim:
> 1. Rename the menu item Data to OPDI Data
> 2. under Data, why is there both Releases and Release v0.0.2
> 3. Could you merge Limitations, Accuracy and Availability under a single item? Merge the pages as well to show, for each European or Global airport (merge these too) the accuracy / limitations and availability for each of the events etc.
> 4. Put Algorithms under data. Remove plausibility checks from Algos.
> 5. Under Resources; You dumped our studies there. Remove them. We shouldn't mention these studies anywhere.

The earlier standing request also applies: *condense the text, facts unchanged, avoid repetition, quick to navigate*. Its rules and lessons are in `.superpowers/sdd/2026-11-18-accuracy-lookup/task-5-context.md`; every page-merging task follows them. Previous plan: `docs/superpowers/plans/2026-11-18-accuracy-lookup.md` (Tasks 1–5c done; 5d and 5e are superseded by this plan).

## Global Constraints

- **No study is mentioned on the site.** "The studies" are the method reports under `papers/`. No page under `content/` and no `index.qmd` may link to `papers/`, carry a "Full study" heading, or refer to these reports ("study", "studies", "report", "chapter", "version N of the study"). Generic uses stay, such as "your study" (a user's analysis) and the PRC Data Challenge text. The `papers/` folder and the tools that read data from it (`tools/draw_validation_figures.py`, `tools/check_papers_index.py`) stay, because they are not published pages.
- **Facts unchanged** when text moves or merges: every fact, number, qualifier and condition survives on the merged page. Exceptions: the studies and the plausibility-checks page links, which the user asked to remove. A fact that only said "see the study" goes; a fact about the data stays, with its link removed.
- Each fact is stated once. Prefer readability over word count, and keep the lookup tables (lessons in `task-5-context.md`).
- Removed pages: add their old URLs to `aliases:` on the absorbing page, rewrite every inbound link to the new target, and keep every linked id. `python3 tools/check_site.py` must print `OK`.
- On `content/accuracy/*` pages, prose numbers must be rows of `assets/pages/accuracy_numbers.csv` for that page key.
- No new dependency.
- Commits: short subject, **no Co-Authored-By trailer, no "Generated with" line**. Never stage `_site/`, `.env.template`, `.gitignore` or `Cern-Logo-500x281-3539449603.png`.
- Verification for every task: `python3 tools/check_site.py` → `OK`; `uvx pytest tools/tests -q`; `node --test tools/tests/*.test.mjs`; `python3 tools/gen_accuracy_tables.py --check` and `python3 tools/gen_item_figures.py --check` exit 0; `quarto render` of each touched page succeeds.

## Decisions taken here

- **Releases (point 2).** The two pages exist because "Releases" says what changed and how to move from v0.0.2, while "Release v0.0.2" documents the old files. They overlap, so `content/data/v0.0.2.qmd` merges into `content/data/releases.qmd` as a final `## Release v0.0.2` section. That page keeps v0.0.2's aliases plus `/content/data/v0.0.2.html`. The R-driven status text stays R-driven.
- **Data quality (point 3).** One sidebar section, "Data quality", with three pages:
  - "Overview": `content/accuracy/index.qmd` (what availability and accuracy mean, the labels, the references, the three days, the item lookup, advice, downloads).
  - "Limitations": `content/limitations.qmd`. It absorbs `content/data/limitations.qmd`, whose dataset-wide limits merge into its "General limits", without duplicates.
  - "Airports": the new `content/accuracy/airports.qmd`, which absorbs `europe.qmd` and `world.qmd`. One search covers all airports of both lists. Each item tile shows availability, accuracy and a link to that item's limitations.
- **Plausibility checks (point 4).** Delete `content/algorithms/plausibility-checks.qmd`, plus `content/methods/plausibility-checks.qmd` if nothing else links to it. The steps on other algorithm pages that say a check removes an event keep that fact, as plain text without the link.
- **Final sidebar:** `OPDI Data` → Overview, Access, Data dictionary, Releases, Data preview, `Algorithms` (its current subtree, minus Plausibility checks), `Data quality` (Overview, Limitations, Airports). Nothing else stays at top level from these pages.

## Review Focus

1. **Old URLs and anchors** (`accuracy/europe.html#off-block`, `data/v0.0.2.html`, `data/limitations.html#ads-b-reception`) must land on the new page, and on the section when Quarto's alias redirect keeps the hash. Owned by Tasks 3, 5 and 7 (render, then open the old URL with `file://` or the dev server and check where it lands).
2. **An airport in both lists** (for example LTFM, rank 1 in Europe and 16 in the world) must appear once, with both ranks and the European (referenced) figures. Owned by Task 4 (`test_airports_payload_merges_both_lists`).
3. **An airport outside Europe** (KATL) must show "availability only", and its limitation links must still work. Owned by Task 4 (JS test).
4. **A "study" mention left in generated text** (`_generated/` includes from `gen_item_figures.py`, or `gen_accuracy_tables.py`) would come back on every regeneration. Owned by Task 1 (the new check also scans included files).
5. **A widget hash for an item filtered out** (`#take-off` while the list filter is "World top 100") still opens the item view, for the current filter. Owned by Task 4 (JS test).

---

### Task 1: Remove every mention of the method studies

**Files:**
- Modify: `tools/check_site.py` (drop "Full study" from `ALGORITHM_HEADINGS` and from the methods template list at line 80; add a check `studies`), `tools/tests/test_check_site.py`
- Modify: `content/resources.qmd` (delete "## Method studies" and its table and index link; keep "References of the algorithms" and below), every `content/algorithms/*.qmd` with a "## Full study" section (delete the section), every `content/methods/*.qmd` with one, `content/algorithms/index.qmd`, `content/accuracy/*.qmd`, `content/data/limitations.qmd`, `content/about.qmd`, and any other page the new check flags
- Modify, if they emit study text: `tools/gen_item_figures.py`, `tools/gen_validation_summary.py`, `tools/gen_accuracy_tables.py`, followed by a regeneration

**Interfaces:**
- Produces: `check_studies(root: Path) -> list[Failure]`, registered in `CHECKS` as `"studies"`. It fails on (a) any link or `src`/`href` whose target contains `papers/`, (b) a heading `Full study`, in every page and every file the page includes (use `expanded_body`).

- [ ] **Step 1: Failing tests** in `tools/tests/test_check_site.py`, using the existing `site` fixture:
  - `test_link_to_a_study_fails`: a page with `[x](../papers/a/index.html)` → `"studies" in kinds(...)`.
  - `test_full_study_heading_fails`: an algorithm page with `## Full study` → `"studies"`.
  - `test_study_in_included_file_fails`: an include under `_generated/` that links to `papers/` → `"studies"`.
  - Update the template fixture helpers in that file that build algorithm pages with the nine headings: they must use the eight remaining headings.
- [ ] **Step 2:** `uvx pytest tools/tests/test_check_site.py -q` → the new tests FAIL.
- [ ] **Step 3: Implement** `check_studies`, change the heading tuples, and document the new kind in `tools/README.md` under the list of checks.
- [ ] **Step 4: Clean the pages.** Run `python3 tools/check_site.py --only studies` and fix every failure. Then run `grep -rniE "\bstud(y|ies)\b|\bchapter\b" content index.qmd` and remove each remaining reference to the method studies, using the rule in Global Constraints. In `resources.qmd`, also remove the intro words "The papers … behind the OPDI" if they refer to the studies. Keep the peer-reviewed "Publications" section: those are published papers, not the method studies.
- [ ] **Step 5: Verify** with every command in Global Constraints. Run `grep -rl "papers/" _site/content` after a render: no hits, ignoring `_site/papers/` itself.
- [ ] **Step 6: Commit** with the subject `site: remove the method studies`.

### Task 2: Remove the plausibility-checks page from Algorithms

**Files:**
- Delete: `content/algorithms/plausibility-checks.qmd`. Also delete `content/methods/plausibility-checks.qmd` if `grep -rn "methods/plausibility-checks" content index.qmd` finds no inbound link other than from pages being deleted.
- Modify: `_quarto.yml` (the sidebar entry), `content/algorithms/index.qmd`, `take-off-landing.qmd`, `block-times.qmd`, `surface-events.qmd`, `processing-steps.qmd`, `content/data/dictionary.qmd`, `content/resources.qmd` (if a row is left), and any generator or check list that names the page (`grep -rn "plausibility" tools/`)

- [ ] **Step 1:** Delete the page(s) and the sidebar entry. Run `python3 tools/check_site.py`: the broken links it lists are the work list.
- [ ] **Step 2:** For each broken link, keep the sentence's fact and drop the link, for example "13. A plausibility check then removes a block time that …". If a sentence only pointed to the page ("see Plausibility checks"), delete it.
- [ ] **Step 3: Verify** with every command in Global Constraints.
- [ ] **Step 4: Commit** with the subject `algorithms: remove the plausibility-checks page`.

### Task 3: Merge Release v0.0.2 into Releases

**Files:**
- Modify: `content/data/releases.qmd`, `_quarto.yml` (remove the "Release v0.0.2" entry), the 17 inbound links (`grep -rn "v0.0.2.qmd" content index.qmd tools/*.py R/`)
- Delete: `content/data/v0.0.2.qmd`

- [ ] **Step 1: Record facts.** Copy both pages, as they are at HEAD, to the workspace as `releases.pre-merge.qmd` and `v0.0.2.pre-merge.qmd`, and write `task-3-facts.md` (one fact per line).
- [ ] **Step 2: Merge.** Append `## Release v0.0.2 {#release-v002}` to releases.qmd, holding v0.0.2's content. Delete what releases.qmd already says (such as the file layout and "box around Europe"), so each fact appears once. Keep every heading id that something links to: `#measurements`, `#how-release-v0`, `#flights-and-their-identifiers`, `#flight-list-files`, `#flight-events`, `#departure-and-destination-airports`, `#moving-from-v0.0.2`. If an id collides with an existing one on releases.qmd, rename the releases one only if nothing links to it, and otherwise ask (NEEDS_CONTEXT). Copy v0.0.2's R chunks exactly. Merge the `aliases:` lists and add `/content/data/v0.0.2.html`.
- [ ] **Step 3:** Rewrite the inbound links from `v0.0.2.qmd#x` to `releases.qmd#x`. A link without an anchor goes to `releases.qmd#release-v002`. In `R/portal_config.R`, change only link targets, never release values.
- [ ] **Step 4: Verify** with every command in Global Constraints. Render releases.qmd, then open `_site/content/data/v0.0.2.html` and check that it redirects.
- [ ] **Step 5: Commit** with the subject `releases: one page for both releases`.

### Task 4: Merged airport payload and widget

**Files:**
- Modify: `tools/gen_accuracy_tables.py`, `tools/tests/test_gen_tools.py`, `assets/js/accuracy-lookup.js`, `assets/css/accuracy-lookup.css`, `tools/tests/accuracy_lookup.test.mjs`
- Generated: `assets/pages/accuracy_lookup_airports.json` and `content/accuracy/_generated/airports_lookup.md`. In this task the europe and world outputs are still written; Task 5 removes them.

**Interfaces:**
- Consumes: the existing `LookupPayload`, `Row`, `airport_payload`, `ground_airports`, `no_time_airports`, `item_group` and `lookup_include` (see the Task 2 brief of the previous plan, `.superpowers/sdd/2026-11-18-accuracy-lookup/task-2-brief.md`).
- Produces:
  - `LIMITATION_ANCHOR: dict[str, str]`, mapping an algorithm page to the `lim-*` id in `content/limitations.qmd`:

    | Algorithm page(s) | Limitation id |
    |---|---|
    | tracks, cleaning | lim-tracks |
    | airports | lim-airports |
    | aircraft | lim-aircraft |
    | first-last-seen | lim-first-last-seen |
    | flight-list-milestones, movements | lim-flight-list-milestones |
    | take-off-landing | lim-take-off-landing |
    | line-up-take-off-roll, xing-runway-threshold, runway-vacated, go-around, runway-crossings | lim-runway |
    | block-times | lim-block-times |
    | surface-events | lim-surface |
    | tops, level-segments, level-offs, flight-level-crossings | lim-vertical |
    | distance-rings | lim-rings |
    | fir-crossings | lim-fir |
    | holding | lim-holding |
    | point-merge | lim-point-merge |
    | distance-flown, time-passed | lim-measurements |

  - Every `items[*]` entry, in every payload, gains `"limits": "../limitations.qmd#<id>"`.
  - `airports_payload(eu_rows, world_rows, airports_csv) -> dict`, with `scope: "airports"`. It returns one airport per ICAO in the union of both lists:

    ```text
    {"icao","name","country",
     "ranks": {"europe": int|null, "world": int|null},
     "movements": {"nm": int|null, "opdi": int|null},   # from airports.csv
     "where", "flags", "limited",                         # as before; europe values when in the europe list
     "source": "europe" | "world"}                        # which CSV the rows came from
    ```

    `rows[icao]` comes from the European CSV when the airport is in that list, and from the world CSV otherwise.
  - JS: `mount` supports `data-scope="airports"`, with list-filter chips "All airports", "European top 100" and "Worldwide top 100". `renderItem(payload, itemOrAnchor, {list})` takes `list` = `"all"` | `"europe"` | `"world"`. `routeHash(payload, hash) -> {view, key} | null` never throws. `nextIndex(active, delta, n) -> int`.
- [ ] **Step 1: Failing Python tests**
  - `test_limitation_anchor_covers_every_item_page`: every value of `ITEM_PAGE` is a key of `LIMITATION_ANCHOR`, and every id appears as `{#<id>}` in `content/limitations.qmd`.
  - `test_airports_payload_merges_both_lists`: ICAOs = union of both CSVs. `LTFM.ranks == {"europe": 1, "world": 16}`. `LTFM.source == "europe"`. `KATL.ranks.europe is None`. `KATL.where == "outside Europe"`.
  - `test_items_carry_limits`: `items["ATOT"]["limits"] == "../limitations.qmd#lim-take-off-landing"`, in both the airports and the overall payloads.
  - `test_airports_include_keeps_anchors`: the include holds the ten MAIN anchor spans, plus `src="../../assets/js/accuracy-lookup.js"` and `data-scope="airports"`.
  - The size bound for the new JSON is 1.2 × the measured size; record the size in the commit body.
- [ ] **Step 2: Failing JS tests** (fixture `assets/pages/accuracy_lookup_airports.json`):
  - `renderAirport(p,"LTFM")` contains `"Europe"` and `"rank 1"` and `"16"`, and a link `../limitations.html#lim-take-off-landing`.
  - `renderAirport(p,"KATL")` contains `"availability only"` and `"limitations.html#lim-"`.
  - `renderItem(p,"take-off",{list:"world"})` does not contain `"LGIR"` (Europe only); `{list:"europe"}` does not contain `"KATL"`.
  - `routeHash(p,"#%E0%A4")` is `null` and does not throw; `routeHash(p,"#airport-egll")` is `{view:"airport",key:"EGLL"}`; `routeHash(p,"#take-off")` is `{view:"item",key:"ATOT"}`.
  - `nextIndex(-1,-1,5) === 4`, `nextIndex(4,1,5) === 0`.
  - The search box text follows the view: after `show("item",…)` the input shows the item title. Test this through a pure `inputLabel(payload, view, key) -> string`.
  - When `compared` is null, the row renders without `"— compared"`.
- [ ] **Step 3: Implement.** In the item tile and the compact rows, show "Limitations" as a link to `items[k].limits`, with `.qmd` changed to `.html`. In the overall payload too, so the Overview card links each item to its limitations.
- [ ] **Step 4: Verify** with every command in Global Constraints.
- [ ] **Step 5: Commit** with the subject `accuracy: one airport lookup for Europe and the world, with limitation links`.

### Task 5: One Airports page

**Files:**
- Create: `content/accuracy/airports.qmd`
- Delete: `content/accuracy/europe.qmd`, `content/accuracy/world.qmd`
- Modify: `tools/gen_accuracy_tables.py` (stop writing the europe and world payloads and includes; rename the numbers-registry page keys `accuracy/europe` and `accuracy/world` to `accuracy/airports`), `tools/gen_item_figures.py` (`airport_line`: one set of links to `../accuracy/airports.qmd#<anchor>`; the fallback goes to `#all-items`), `tools/tests/test_gen_tools.py` (expected `airports.qmd#take-off`), `_quarto.yml`, and the inbound links (`grep -rn "accuracy/europe.qmd\|accuracy/world.qmd\|europe.qmd#\|world.qmd#" content index.qmd tools`)

- [ ] **Step 1: Record facts** from both old pages, saved as `task-5-facts.md` in the workspace.
- [ ] **Step 2: Write `airports.qmd`.** Front matter: the title "Airports: availability, accuracy and limitations", and `aliases: [europe.html, world.html]`. Sections, in this order:
  - a two-sentence intro;
  - `## How to read a card {#how-to-read}`: a one-line reminder of the labels with a link to `index.qmd#how-to-read-the-labels`, plus the note "outside Europe there is no reference: availability only";
  - `## Look up an airport or an item {#lookup}`, holding the `airports_lookup.md` include;
  - `## Which airports {#which-airports}`: both lists, how each is ranked, and how many airports send records. Use only registered numbers, or none;
  - `## Per item {#per-item}`: one short paragraph;
  - `## Patterns {#patterns}`, `### Where an item is unreliable {#unreliable}`, `### Where ground events are nearly absent {#ground-reception}`, `### Where no take-off or landing time exists {#no-times}`: carried over, with their includes;
  - `## Every other item {#all-items}`: both CSV downloads plus `airports.csv`.

  Each fact is stated once.
- [ ] **Step 3:** Delete the old pages, regenerate (`python3 tools/gen_accuracy_tables.py`, `python3 tools/gen_item_figures.py`), and rewrite the inbound links.
- [ ] **Step 4: Verify** with every command in Global Constraints. Also open `_site/content/accuracy/europe.html#off-block` and record where it lands.
- [ ] **Step 5: Commit** with the subject `accuracy: one Airports page for Europe and the world`.

### Task 6: Condense the Data quality Overview

**Files:** `content/accuracy/index.qmd`

- [ ] **Step 1: Record facts**, saved as `task-6-facts.md`.
- [ ] **Step 2: Rewrite** under the condensing rules. Keep the labels table at `#how-to-read-the-labels`, the item lookup, the "What this means for your study" advice (a reader's study, which is allowed), the kept tables, and the downloads (point them at the Airports page). Move anything here that only concerns limitations to `content/limitations.qmd` (Task 7) and link to it: list such passages in the report for Task 7 rather than editing limitations.qmd now.
- [ ] **Step 3: Verify**, then **Step 4: Commit** with the subject `data quality: condense the overview`.

### Task 7: One Limitations page

**Files:**
- Modify: `content/limitations.qmd`
- Delete: `content/data/limitations.qmd`
- Modify: `_quarto.yml`, and the inbound links to `data/limitations.qmd` (`grep -rn "data/limitations.qmd" content index.qmd tools`)

- [ ] **Step 1: Record facts** from both pages, plus any passages Task 6 handed over (in `task-6-report.md`), saved as `task-7-facts.md`.
- [ ] **Step 2: Merge** the dataset-wide limits into `## General limits {#general-limits}`. Each fact is stated once: "Reception" and "ADS-B reception", for example, become one bullet. Keep `{#ads-b-reception}` and every other linked id from data/limitations.qmd on the merged bullet or heading. Add `aliases: [data/limitations.html]`, written relative to this page as Quarto expects (check by rendering).
- [ ] **Step 3: Condense** the per-item sections. Keep the three-heading pattern and every `#lim-*` id; each subsection becomes a short list. Keep every `[...]{.measured}` span, and keep every published item named (the `coverage` check). Each `lim-*` section ends with one line: "Per airport: [Airports](accuracy/airports.qmd#lookup)".
- [ ] **Step 4: Verify**, then **Step 5: Commit** with the subject `limitations: one page for the dataset and each item`.

### Task 8: The OPDI Data menu

**Files:** `_quarto.yml`, `tools/tests/test_check_site.py` (`test_quality_pages_sit_under_data`)

- [ ] **Step 1: Update the test** so it asserts the final sidebar from *Decisions taken here*:
  - `section: "OPDI Data"` exists and `section: "Data"` does not;
  - between `OPDI Data` and the next top-level entry (`PRC Data Challenges`) sit `content/algorithms/index.qmd`, `section: "Algorithms"`, `section: "Data quality"`, `content/accuracy/index.qmd`, `content/limitations.qmd` and `content/accuracy/airports.qmd`;
  - neither `plausibility-checks`, `v0.0.2.qmd` nor `data/limitations.qmd` appears anywhere in the sidebar.
- [ ] **Step 2:** Run it → FAIL. **Step 3:** Edit the sidebar. Use the same indentation style as the existing nested sections, and check that it parses: `uvx --with pyyaml python -c "import yaml; yaml.safe_load(open('_quarto.yml'))"`.
- [ ] **Step 4: Verify** with every command in Global Constraints, plus a full `quarto render`. Then take a screenshot of the sidebar of one page, expanded, at 1200 px.
- [ ] **Step 5: Commit** with the subject `sidebar: OPDI Data with Algorithms and Data quality`.
