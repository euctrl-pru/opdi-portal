# Tools for the OPDI website

These scripts check the website source and write the parts of it that come
from other files. None of them needs Quarto or R. Run them from the
repository root.

| Script | What it does | Needs |
|---|---|---|
| `check_site.py` | Checks links, the sidebar, the methods page template, the numbers on methods pages and the wording of every page. Exit status 1 when anything fails. | Python 3 |
| `check_papers_index.py` | Checks that every study under `papers/` is listed once in `papers/index.qmd`. | Python 3 |
| `gen_data_dictionary.py` | Writes the column tables of the data dictionary from the pipeline's list of published columns. | Python 3, the `opdi` repository |
| `draw_validation_figures.py` | Draws the validation charts of the methods pages from committed study results, and records the numbers each page quotes. | Python 3, pandas, matplotlib |
| `gen_validation_summary.py` | Writes the table of the Validation summary page from the recorded numbers. | Python 3 |

## Before rendering the site

```bash
python tools/gen_data_dictionary.py --opdi ../opdi/src
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
- **community**: the community page is referenced (see "The community page" below).
- **headings**: a methods page does not have the nine headings of the template.
- **numbers**: a methods page quotes a number that is not recorded in
  `assets/validation/validation_measures.csv`.
- **vocabulary**: a page uses a word the site avoids. The list is
  `FORBIDDEN_WORDS` in `check_site.py`: internal names of methods, and words
  that are vague or come from software jargon (for example "robust",
  "leverage", "rung"). "Gate-to-gate", the aviation term for a whole flight,
  is allowed.
- **coverage**: `content/data/dictionary.qmd` does not describe one of the
  event types in `EVENT_TYPES`.

Checks that concern pages which do not exist yet find nothing to check.

## Writing a methods page

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
9. `## Full study`

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
or "Not validated" when there is none. The page
`content/methods/validation-summary.qmd` includes it.

## Tests

```bash
python -m pytest tools/tests
```

The tests build small websites in a temporary folder; they do not touch the
real site.

## The community page

The page `content/community.qmd` is kept in the repository but is hidden: it
is not in the sidebar, the home page has no card for it, and no page links to
it. `check_site.py` reports any reference to it (kind `community`). To show
the page again, first remove the line `"community": check_community,` from
`CHECKS` in `check_site.py`, then make these three edits:

1. In `_quarto.yml`, under the sidebar entry `- href: content/resources.qmd`
   (with its line `text: Resources`), put back:

   ```yaml
         - href: content/community.qmd
           text: Community
   ```

   A commented copy of these lines is at that place in `_quarto.yml`.

2. In `index.qmd`, put this card back between the OPDI Sensors card and the
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

3. Link the page from other pages where it helps the reader, for example from
   `content/about.qmd`.
