# Tools for the OPDI website

These scripts check the website source and write the parts of it that come
from other files. None of them needs Quarto or R. Run them from the
repository root.

| Script | What it does | Needs |
|---|---|---|
| `check_site.py` | Checks links, the sidebar, the template and the numbers of the algorithm pages, that every published item is listed, the pointers of moved pages, and the wording of every page. Exit status 1 when anything fails. | Python 3 |
| `gen_item_figures.py` | Writes the availability and accuracy tables of the algorithm pages from the three-day tables in `assets/accuracy/`, and records each number in `assets/algorithms/item_figures.csv`. | Python 3 |
| `gen_accuracy_tables.py` | From `assets/accuracy/`, writes the tables of the Data quality pages, the labels of the Algorithms overview, the search payloads `assets/pages/accuracy_lookup_{airports,overall}.json` with their includes `content/accuracy/_generated/{airports,overall}_lookup.md`, and the numbers their prose may use (`assets/pages/accuracy_numbers.csv`). | Python 3 |
| `check_papers_index.py` | Checks that every study under `papers/` is listed once in `papers/index.qmd`. | Python 3 |
| `gen_data_dictionary.py` | Writes the column tables of the data dictionary from the pipeline's list of published columns. | Python 3, the `opdi` repository |
| `draw_validation_figures.py` | Draws the validation charts of the methods pages from committed study results, and records the numbers each page quotes. | Python 3, pandas, matplotlib |
| `gen_validation_summary.py` | Writes the table of the Validation summary page from the recorded numbers. | Python 3 |

## Before rendering the site

```bash
python tools/gen_data_dictionary.py --opdi ../opdi/src
python tools/gen_accuracy_tables.py
python tools/gen_item_figures.py
python tools/draw_validation_figures.py
python tools/gen_validation_summary.py --opdi ../opdi/src
python tools/check_site.py
python tools/check_papers_index.py
```

Then render with `quarto render`.

## check_site.py

```bash
python tools/check_site.py                    # every check
python tools/check_site.py --only link        # one kind of check (repeatable)
python tools/check_site.py --list-types       # event types and columns the checks expect
python tools/check_site.py --list-types --opdi ../opdi/src   # columns from the pipeline itself
python tools/check_site.py --root PATH        # another copy of the site
```

Each problem is printed on one line as `kind: file:line: message`, followed by a
count per kind. The kinds are:

- **link**: a relative link or image points to a file that does not exist. A
  link to `page.html` is accepted when `page.qmd` exists, or when a page lists
  `page.html` under `aliases:` in its front matter (this keeps old addresses
  working after a page moves).
- **anchor**: a link ends in `#name` and the target page has no heading or
  element with that identifier. Quarto makes an identifier from a heading by
  writing it in lower case, dropping punctuation, joining the words with
  hyphens and dropping any leading digits: `## 2025 PRC Data Challenge` becomes
  `#prc-data-challenge`. An explicit `## Heading {#my-id}` wins.
- **sidebar**: an entry of the sidebar in `_quarto.yml` points to a missing page.
- **community**: the community page is referenced, or is not excluded from rendering (see "The community page" below).
- **headings**: a methods page does not have the eight headings of the template.
- **numbers**: a methods page quotes a number that is not recorded in
  `assets/validation/validation_measures.csv`.
- **vocabulary**: a page uses a word the site avoids. The list is
  `FORBIDDEN_WORDS` in `check_site.py`: internal names of methods, and words
  that are vague or come from software jargon (for example "robust",
  "leverage", "rung"). "Gate-to-gate", the aviation term for a whole flight,
  is allowed.
- **studies**: a page, or a file it includes, links to the method studies (a
  link, `src` or `href` containing `papers/`) or has a `## Full study` heading.
  The studies are not mentioned on the site.
- **coverage**: `content/data/dictionary.qmd` does not describe one of the
  event types in `EVENT_TYPES`.

Checks that concern pages which do not exist yet find nothing to check.

## Writing an algorithm page

An algorithm page is a file `content/algorithms/<item>.qmd`, one per
published item or per pair of items that are two ends of one thing. Two
files in that folder are free-form: `index.qmd` (the list of every item) and
`processing-steps.qmd`. Every other page must have exactly these
second-level headings, in this order:

1. `## In one sentence`
2. `## What it needs`
3. `## How it is computed`
4. `## Based on`
5. `## What is published`
6. `## Use cases`
7. `## Limitations`
8. `## Availability and accuracy`

`Based on` names the tool or method the algorithm comes from, with a
link, only where the code or its docstrings record it; otherwise "Developed
for OPDI". `Limitations` links to the item's section of `content/limitations.qmd` (identifiers `#lim-<family>`), which has the
sub-headings "When it is missing", "When it can be wrong" and "What not to
conclude from it".

### Numbers of the algorithm pages

Every number under `## Availability and accuracy` must be a row of
`assets/algorithms/item_figures.csv` for the same page. The rows and the
tables are written by `gen_item_figures.py`; a page shows its table with

```markdown
{{< include _generated/figures_<item>.md >}}
```

Each number is one entry of `FIGURES` in `gen_item_figures.py`: the page, the
item and reference of `assets/accuracy/item_accuracy.csv` (rows of the three
days together), the column, and how to show it. To change a figure, change
its entry and run the script again; the pages need no edit. Availability and
accuracy are separate rows, each with the label of the staged table:
availability `high` (at least 80 %), `partial` (50 to 80 %), `low` (under
50 %); accuracy `good` (at least 80 % within 60 s, or equal), `fair` (50 to
80 %), `limited` (under 50 %), `too few compared`, `no reference`. Each table
ends with a sentence linking to the item's rows on the airport pages.

### Numbers of the accuracy pages

The tables of `content/accuracy/` are written by `gen_accuracy_tables.py`
into `content/accuracy/_generated/` and included by the pages; they are not
checked number by number, because they are the staged tables reformatted.
Every number a page under `content/accuracy/` writes itself must be a row of
`assets/pages/accuracy_numbers.csv` for that page (key `accuracy/index`,
`accuracy/airports`), which the same script writes from the
staged tables, with the thresholds and dates as rows of kind `rule`. On any
other page, a measured figure in prose is written as a span,
`[24.2 %]{.measured}`, and must be a row for that page (for example
`limitations`); Quarto renders the span as ordinary text. To quote a new
figure, add it to `numbers()` in `gen_accuracy_tables.py` (`off_block_numbers()`
for the off-block tables and the divided tracks) and run it.

### Coverage, identifiers and moved pages

- **coverage**: `content/algorithms/index.qmd` must name, as inline code,
  every event type of `EVENT_TYPES` and every flight list column and
  measurement of `assets/data/dictionary.csv`; `content/limitations.qmd`
  every event type.
- **identifiers**: no page shows a track identifier (16 hexadecimal
  characters, an underscore and digits), except the invented example
  `EXAMPLE_TRACK_ID` used to explain the format, and no page links to the
  working material of the spot checks.
- **pointer**: a page that has moved keeps its address with a short text and
  `moved-to: <new page>` in its front matter; the check verifies that the new
  page exists and is linked. Such a page is exempt from the methods template.

## Writing a methods page

The pages under `content/methods/` now point to the algorithm pages. The rules
below remain for a methods page that is not a pointer.

A methods page is a file `content/methods/<page>.qmd`, for example
`content/methods/holding.qmd`. Three files in that folder are free-form:
`index.qmd`, `pipeline.qmd` and `validation-summary.qmd`. Every other page
must have exactly these second-level headings, in this order, written exactly
like this:

1. `## What it is`
2. `## How it is computed`
3. `## Reference data`
4. `## Validation method`
5. `## Results`
6. `## Error distribution`
7. `## Headline accuracy`
8. `## Limitations`

Other pages link to these sections as `<page>.qmd#validation-method`,
`<page>.qmd#limitations` and so on. Use third-level headings (`###`) for
anything inside a section. An algorithm that has not been validated keeps the
same headings and says "Not validated" under them.

### Numbers

Every number written under `## Results` and `## Headline accuracy` must be a
row of `assets/validation/validation_measures.csv` for the same page. The
check accepts a number shown with fewer decimals than recorded (0.7368 may be
written 0.74). It ignores dates written as YYYY-MM-DD, numbers inside names
such as `FL100` or `xing-40nm`, numbers inside inline code, and numbers that
inline R code reads from the file at render time. Refer to figures and tables
by their Quarto label (`@fig-duration`), not as "Figure 1".

The columns of `validation_measures.csv` are:

| Column | Meaning |
|---|---|
| `page` | Methods page file name without `.qmd`, for example `holding` |
| `measure_id` | Short identifier, unique within the page, for example `f1` |
| `label` | What the number is, in words a student can follow |
| `value` | The number |
| `unit` | Unit of `value`: `%`, `s`, `min`, `NM`, `ft`, or empty for a ratio |
| `n` | Number of cases the value is computed over (flights, events, time windows) |
| `reference` | The reference data the value is measured against |
| `source_file` | Committed study file the value comes from, for example `papers/airspace-events-v1/data/holding/holding_score_2019-10-01.csv` |
| `source_field` | Column and row of that file |
| `headline` | `1` for the single headline number of an item, otherwise `0` |
| `item` | Event types, columns or measurements the number applies to, separated by `;`, written as `check_site.py --list-types` prints them |
| `kind` | `event`, `flight list column` or `measurement` |

Do not type rows into this file by hand: they are written by the figure
functions in `draw_validation_figures.py`, so that every number can be
produced again from the study files.

### Figures

Each chart is one function in `draw_validation_figures.py`, registered with
`@figure("<page>", "<figure>")`. The function reads committed study CSV files
under `papers/*/data/`, draws on the axes from `new_figure(title)`, saves with
`save(fig, root / OUT_DIR / "<page>_<figure>.svg")`, and returns the page's
numbers as a list of `measure(...)` rows. Put the number of cases in the title
(`n = 249`) and the unit in each axis label. `holding_episode_duration` is a
complete example. Running

```bash
python tools/draw_validation_figures.py --only <page>_<figure>
```

writes `assets/validation/<page>_<figure>.svg` and adds or replaces that
function's rows in `validation_measures.csv`. The page shows the chart under
`## Error distribution` with

```markdown
![Caption in one sentence.](../../assets/validation/<page>_<figure>.svg){#fig-<figure>}
```

A number that has no chart is recorded the same way, by a function registered
with `@measure_set("<page>")` instead of `@figure`. It reads the study files and
returns `measure(...)` rows but draws nothing; `--only <page>_numbers` runs it.
`measurements_numbers` is an example.

## Data dictionary

`gen_data_dictionary.py --opdi ../opdi/src` reads `src/opdi/output/schema.py`
from the pipeline repository and writes `content/data/_generated/*_columns.md`
(tables with the headings Column, Type, Unit, Description) and
`assets/data/dictionary.csv`. Include a table in a page with
`{{< include _generated/flight_list_columns.md >}}`. Add `--check` to only
report whether the files are out of date. Run it again whenever the
pipeline's published columns change.

## Validation summary

`gen_validation_summary.py` writes `content/methods/_generated/validation_summary.md`:
one line per event type (and, with `--opdi`, per flight list column and
measurement), with the headline row of `validation_measures.csv` for that item,
or "Not validated" when there is none. Every line links to the methods page
that describes the item; the page of each item is listed in `METHOD_PAGES` in
`gen_validation_summary.py`. The page
`content/methods/validation-summary.qmd` includes the table.

`EVENT_TYPES` in `check_site.py` is the list of event types the pipeline
publishes with its default settings (`EventConfig()` in the pipeline's
`src/opdi/config.py`). When the pipeline adds, removes or renames an event
type, update `EVENT_TYPES` and `METHOD_PAGES` together; a test fails when an
event type has no methods page.

## Tests

```bash
python -m pytest tools/tests
node --test tools/tests/*.test.mjs
```

The node tests cover the search widget `assets/js/accuracy-lookup.js`.

The tests build small websites in a temporary folder; they do not touch the
real site.

## The community page

The page `content/community.qmd` is kept in the repository but is hidden: it
is not rendered (so it is neither on the site nor in its search), it is not in
the sidebar, the home page has no card for it, and no page links to it.
`check_site.py` reports any reference to it (kind `community`), and also
reports when `_quarto.yml` does not exclude it from rendering with the entry
`"!content/community.qmd"` under `project: render:`, after the pages to render.

A site rendered before the page was excluded still holds
`_site/content/community.html` and an entry for it in `_site/search.json`.
Both disappear only when the site is rendered again; if
`_site/content/community.html` remains after that, delete it.

To show the page again, first remove the line `"community": check_community,`
from `CHECKS` in `check_site.py`, then make these four edits:

1. In `_quarto.yml`, under `project: render:`, remove the line
   `- "!content/community.qmd"`.

2. In `_quarto.yml`, under the sidebar entry `- href: content/resources.qmd`
   (with its line `text: Resources`), put back:

   ```yaml
         - href: content/community.qmd
           text: Community
   ```

   A commented copy of these lines is at that place in `_quarto.yml`.

3. In `index.qmd`, put this card back between the OPDI Sensors card and the
   Resources card. The member and country counts come from `community` in
   `R/portal_config.R`, which is kept.

   ````markdown
   ::: {.pillar-card .pillar-community}

   ### {{< iconify fa6-solid handshake >}} OPDI Community

   Working with research institutions, air navigation service providers, and open-data enthusiasts to advance open aviation performance data.

   ::: {layout-ncol=3}
   ```{r}
   bslib::value_box(
     title = "Members",
     value = cfg$community$members,
     showcase = bs_icon("building"),
     theme = value_box_theme(bg = "#6a3d9a")
   )
   ```

   ```{r}
   bslib::value_box(
     title = "Countries",
     value = cfg$community$countries,
     showcase = bs_icon("globe2"),
     theme = value_box_theme(bg = "#6a3d9a")
   )
   ```
   :::

   [Meet the community &rarr;](content/community.qmd){.btn .btn-sm style="background-color:#6a3d9a;color:white;border:none;"}

   :::

   ````

4. Link the page from other pages where it helps the reader, for example from
   `content/about.qmd`.
