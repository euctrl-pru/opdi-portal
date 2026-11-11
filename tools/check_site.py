#!/usr/bin/env python3
"""Check the OPDI website source for broken links and for rules on page content.

The checks read the Quarto source files directly, so Quarto does not need to be
installed. They cover the home page (index.qmd) and every page under content/.
Files whose name starts with an underscore are not pages: Quarto does not
render them on their own. They are checked where another page includes them.

Kinds of check (each failure is printed with its kind):

  link        A relative link or image points to a file or page that does not
              exist. A link to an .html page is accepted when the matching
              .qmd page exists, or when a page lists that address under
              `aliases:` in its front matter.
  anchor      A link ends in #name, and the target page has no heading or
              element with that identifier. Identifiers follow the rules
              Quarto uses: an explicit {#name} wins; otherwise the heading text
              is turned into lower case words joined by hyphens.
  sidebar     An entry in the sidebar of _quarto.yml points to a missing file.
  community   The community page (content/community.qmd) is referenced from
              _quarto.yml, the home page or another page. The page is kept as
              a file but must not be reachable. Comment lines in _quarto.yml
              are ignored, so a commented note on how to show the page again
              is allowed. The page must also be excluded from rendering with
              the entry "!content/community.qmd" under project: render:, placed
              after the pages to render; that entry is the one allowed mention.
  headings    A page in content/methods/ (other than index.qmd, pipeline.qmd
              and validation-summary.qmd) does not have exactly the nine
              second-level headings of the methods template, in order.
  numbers     A number written in the "Results" or "Headline accuracy" section
              of a methods page is not in assets/validation/validation_measures.csv
              for that page. Numbers produced by inline R code are not checked
              here, because they are read from the file at render time.
  vocabulary  A page uses a word from the list of internal or vague words the
              site avoids (see FORBIDDEN_WORDS).
  coverage    content/data/dictionary.qmd exists but does not mention one of
              the event types the pipeline publishes (see EVENT_TYPES).

Checks that concern pages which do not exist yet (content/methods/,
content/data/dictionary.qmd) find nothing to check and pass.

Usage:

  python tools/check_site.py                 # run every check, exit 1 on failure
  python tools/check_site.py --only link     # run one kind of check (repeatable)
  python tools/check_site.py --list-types    # print the event types and columns
  python tools/check_site.py --list-types --opdi ../opdi/src
                                             # columns taken from the pipeline schema
  python tools/check_site.py --root PATH     # check another copy of the site

Plain Python 3 with no extra packages.
"""
from __future__ import annotations

import argparse
import csv
import re
import sys
import unicodedata
from collections import Counter
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

#: The nine second-level headings of every methods page, in order.
METHOD_HEADINGS = (
    "What it is",
    "How it is computed",
    "Reference data",
    "Validation method",
    "Results",
    "Error distribution",
    "Headline accuracy",
    "Limitations",
    "Full study",
)

#: Methods pages that do not follow the nine-heading template.
METHOD_PAGES_WITHOUT_TEMPLATE = ("index.qmd", "pipeline.qmd", "validation-summary.qmd")

#: Sections of a methods page whose numbers must come from the measures file.
NUMBER_SECTIONS = ("Results", "Headline accuracy")

#: Columns of assets/validation/validation_measures.csv, in order.
MEASURE_COLUMNS = (
    "page",          # methods page file name without .qmd, e.g. holding
    "measure_id",    # short identifier, unique within the page
    "label",         # what the number is, in words
    "value",         # the number; a page may show it rounded to fewer decimals
    "unit",          # unit of value: %, s, NM, ft, flights, or empty for a ratio
    "n",             # number of cases the value is computed over
    "reference",     # the reference data the value is measured against
    "source_file",   # committed study file the value is read from
    "source_field",  # column or row of that file
    "headline",      # 1 for the headline number of an item, else 0
    "item",          # event types, columns or measurements it applies to, separated by ;
    "kind",          # event, flight list column, or measurement
)

MEASURES_CSV = Path("assets/validation/validation_measures.csv")

#: Words the site does not use. Each entry is (regular expression, advice).
FORBIDDEN_WORDS = (
    # "gate-to-gate", the aviation term for a whole flight, is allowed.
    (r"(?<!to-)(?<!to )\bgat(e|es|ed|ing)\b(?![- ]to[- ]gate)",
     "'gate': say what is checked or required (for an aircraft position, say 'stand')"),
    (r"\bguard(s|ed|ing)?\b", "'guard': say what the rule prevents"),
    (r"\brungs?\b", "'rung': name the method"),
    (r"\bladders?\b", "'ladder': name the methods compared"),
    (r"\barms?\b", "'arm': name the method or the setting"),
    (r"\bpin(s|ned|ning)?\b", "'pin': say 'fix' or 'set'"),
    (r"\bload-bearing\b", "'load-bearing': say 'important' or explain why"),
    (r"\bsurfac(ed|ing)\b|\bto surface\b", "'surface' as a verb: say 'show' or 'report'"),
    (r"\brobust(ly|ness)?\b", "'robust': say what it withstands"),
    (r"\bleverag(e|es|ed|ing)\b", "'leverage': say 'use'"),
    (r"\bdelv(e|es|ed|ing)\b", "'delve': say 'look at'"),
    (r"\bseamless(ly)?\b", "'seamless': remove or say what it means"),
    (r"\bcrucial(ly)?\b", "'crucial': say why it matters"),
    (r"\bcomprehensive(ly)?\b", "'comprehensive': say what is covered"),
    (r"\bA(8|9|10)\b", "internal method name: describe the method instead"),
)

#: Event types the pipeline publishes with its default settings (EventConfig()
#: in src/opdi/config.py). A part in braces stands for a family:
#: xing-{radius}nm is matched by xing-40nm, xing-100nm and so on, or by the
#: written form with braces.
#:
#: Names that only other settings produce are not listed: take-off,
#: first-xing-fl{level}, last-xing-fl{level}, top-of-climb-cco,
#: top-of-descent-cdo, airborne, touchdown, off-block and on-block. With the
#: default settings the lift-off and touchdown are published as ATOT and ALDT,
#: the block times as AOBT and AIBT, and top-of-climb and top-of-descent are
#: the tops of EUROCONTROL's vertical flight efficiency method.
EVENT_TYPES = (
    # Vertical events
    "top-of-climb", "top-of-descent",
    "level-start", "level-end",
    "level-off-climb-start", "level-off-climb-end",
    "level-off-descent-start", "level-off-descent-end",
    "xing-fl{level}",
    # Distance rings around the airports
    "xing-{radius}nm",
    # Airport surface (taxiway, apron, parking_position, threshold, hangar,
    # deicing_pad; the runway is covered by the runway events)
    "entry-{element}", "exit-{element}",
    # Runway events: take-off and landing
    "take-off-roll", "ATOT", "landing", "ALDT",
    # Other runway milestones
    "line-up", "runway-vacated", "go-around",
    "runway-crossing-entry", "runway-crossing-vacated",
    # Block times
    "AOBT", "AIBT",
    # Airspace events
    "holding-start", "holding-end",
    "pms-leg-entry", "pms-leg-exit", "pms-merge-point",
    "xing-fir",
    # Track limits
    "first_seen", "last_seen",
)

COMMUNITY_PATTERN = re.compile(r"community\.(qmd|html)|cfg\$community|pillar-community")

EXTERNAL = re.compile(r"^([a-zA-Z][a-zA-Z0-9+.-]*:|//)")
PAGE_SUFFIXES = (".qmd", ".md", ".ipynb", ".Rmd")


@dataclass(frozen=True)
class Failure:
    kind: str
    path: str
    line: int
    message: str

    def __str__(self) -> str:
        where = f"{self.path}:{self.line}" if self.line else self.path
        return f"{self.kind}: {where}: {self.message}"


# ---------------------------------------------------------------------------
# Reading pages
# ---------------------------------------------------------------------------

FENCE = re.compile(r"^\s*(`{3,}|~{3,})")


def split_front_matter(text: str) -> tuple[str, str]:
    """Return (front matter, body). The front matter is '' when absent."""
    if text.startswith("---"):
        end = re.search(r"^(---|\.\.\.)\s*$", text[3:], flags=re.M)
        if end:
            cut = 3 + end.end()
            return text[3:3 + end.start()], text[cut:]
    return "", text


def blank_code(text: str) -> str:
    """Replace code blocks and HTML comments by empty lines, keeping line numbers."""
    out = []
    fence = None
    for line in text.split("\n"):
        m = FENCE.match(line)
        if fence is None and m:
            fence = m.group(1)[0] * 3
            out.append("")
            continue
        if fence is not None:
            if line.strip().startswith(fence) and line.strip().strip(fence[0]) == "":
                fence = None
            out.append("")
            continue
        out.append(line)
    joined = "\n".join(out)
    return re.sub(r"<!--.*?-->", lambda m: "\n" * m.group(0).count("\n"), joined, flags=re.S)


INCLUDE = re.compile(r"\{\{<\s*include\s+(\S+)\s*>\}\}")


def expanded_body(path: Path, seen: tuple = ()) -> list[tuple[str, Path, int]]:
    """Lines of a page with included files expanded.

    Each item is (line text, file it comes from, line number in that file).
    Code blocks are blanked. Links in an included file resolve from the
    including page, as Quarto pastes the included text in place.
    """
    text = path.read_text(encoding="utf-8")
    front, body = split_front_matter(text)
    offset = text[: len(text) - len(body)].count("\n")
    lines = []
    for i, line in enumerate(blank_code(body).split("\n"), start=offset + 1):
        m = INCLUDE.search(line)
        if m:
            target = (path.parent / m.group(1)).resolve()
            if target.is_file() and target not in seen:
                lines.extend(expanded_body(target, seen + (path.resolve(),)))
                continue
        lines.append((line, path, i))
    return lines


def front_matter_list(front: str, key: str) -> list[str]:
    """Read a list value such as `aliases:` from front matter, without a YAML parser."""
    values = []
    m = re.search(rf"^{key}:[ \t]*(.*)$", front, flags=re.M)
    if not m:
        return values
    inline = m.group(1).strip()
    if inline.startswith("["):
        return [v.strip().strip("'\"") for v in inline.strip("[]").split(",") if v.strip()]
    if inline:
        return [inline.strip("'\"")]
    for line in front[m.end():].split("\n")[1:]:
        item = re.match(r"^\s+-\s+(.+?)\s*$", line)
        if item:
            values.append(item.group(1).strip("'\""))
        elif line.strip() and not line.startswith((" ", "\t")):
            break
    return values


def pages(root: Path) -> list[Path]:
    """Pages of the site: index.qmd and content/**/*.qmd, without _ files."""
    found = []
    if (root / "index.qmd").is_file():
        found.append(root / "index.qmd")
    content = root / "content"
    if content.is_dir():
        for path in sorted(content.rglob("*.qmd")):
            rel = path.relative_to(content)
            if any(part.startswith(("_", ".")) for part in rel.parts):
                continue
            found.append(path)
    return found


def rel(root: Path, path: Path) -> str:
    try:
        return str(path.resolve().relative_to(root.resolve()))
    except ValueError:
        return str(path)


# ---------------------------------------------------------------------------
# Heading identifiers
# ---------------------------------------------------------------------------

ATTRS_AT_END = re.compile(r"\s*\{([^{}]*)\}\s*$")


def plain_heading_text(text: str) -> str:
    """Heading text as Pandoc reads it: no shortcodes, raw LaTeX, links or markup."""
    text = re.sub(r"\{\{<.*?>\}\}", " ", text)
    text = re.sub(r"\\[A-Za-z]+(\{[^{}]*\})*", "", text)        # raw LaTeX such as \acr{OPDI}
    text = re.sub(r"!?\[([^\]]*)\]\([^)]*\)", r"\1", text)       # links and images keep their text
    text = re.sub(r"\[([^\]]*)\]\{[^}]*\}", r"\1", text)         # spans keep their text
    text = re.sub(r"<[^>]+>", "", text)                          # HTML tags
    text = re.sub(r"&[a-zA-Z]+;|&#\d+;", "", text)               # HTML entities
    text = text.replace("`", "")
    text = re.sub(r"(\*+|(?<!\w)_+|_+(?!\w))", "", text)         # emphasis markers
    return text


def heading_identifier(text: str) -> str:
    """The identifier Pandoc gives a heading without an explicit {#id}."""
    text = plain_heading_text(text).lower()
    kept = "".join(
        c for c in text
        if c.isspace() or c in "_-." or unicodedata.category(c)[0] in "LN"
    )
    ident = "-".join(kept.split())
    ident = re.sub(r"^[^a-z\u00e0-\uffff]+", "", ident)
    return ident or "section"


def page_identifiers(path: Path, cache: dict) -> set[str]:
    """Every identifier a link can point to on a page."""
    key = path.resolve()
    if key in cache:
        return cache[key]
    ids: set[str] = set()
    counts: Counter = Counter()
    if path.suffix in PAGE_SUFFIXES and path.is_file():
        lines = expanded_body(path)
        raw = path.read_text(encoding="utf-8")
        for line, _, _ in lines:
            m = re.match(r"^(#{1,6})\s+(.*?)\s*#*\s*$", line)
            if m:
                text = m.group(2)
                attrs = ATTRS_AT_END.search(text)
                explicit = None
                if attrs:
                    explicit = re.search(r"#([\w:.-]+)", attrs.group(1))
                    text = text[: attrs.start()]
                if explicit:
                    ids.add(explicit.group(1))
                else:
                    base = heading_identifier(text)
                    n = counts[base]
                    counts[base] += 1
                    ids.add(base if n == 0 else f"{base}-{n}")
            for m2 in re.finditer(r"\{[^{}]*?#([A-Za-z][\w:.-]*)[^{}]*\}", line):
                ids.add(m2.group(1))
            for m2 in re.finditer(r"\bid=[\"']([^\"']+)[\"']", line):
                ids.add(m2.group(1))
        # Code chunk labels (#| label: fig-x) become identifiers too.
        ids.update(re.findall(r"^#\|\s*label:\s*([\w-]+)", raw, flags=re.M))
    cache[key] = ids
    return ids


# ---------------------------------------------------------------------------
# Links
# ---------------------------------------------------------------------------

MD_LINK = re.compile(r"!?\[(?:[^\[\]]|\[[^\[\]]*\])*\]\(\s*<?([^)\s>]+)>?(?:\s+[\"'][^)]*[\"'])?\s*\)")
HTML_LINK = re.compile(r"\b(?:href|src)=[\"']([^\"']+)[\"']")


def alias_map(root: Path) -> dict[Path, Path]:
    """Map each address listed under `aliases:` to the page that declares it."""
    aliases = {}
    for page in pages(root):
        front, _ = split_front_matter(page.read_text(encoding="utf-8"))
        for alias in front_matter_list(front, "aliases"):
            target = (root / alias.lstrip("/")) if alias.startswith("/") else (page.parent / alias)
            aliases[target.resolve()] = page
    return aliases


def resolve_target(root: Path, base: Path, target: str, aliases: dict) -> Path | None:
    """The source file a relative link points to, or None when there is none."""
    target = target.split("?")[0]
    path = (root / target.lstrip("/")) if target.startswith("/") else (base.parent / target)
    if target.endswith("/") or path.is_dir():
        for name in ("index.qmd", "index.md", "index.html"):
            if (path / name).is_file():
                return path / name
        return None
    if path.is_file():
        return path
    if path.suffix == ".html":
        for suffix in PAGE_SUFFIXES:
            candidate = path.with_suffix(suffix)
            if candidate.is_file():
                return candidate
        alias = aliases.get(path.resolve())
        if alias is not None:
            return alias
    return None


def check_links(root: Path) -> list[Failure]:
    failures = []
    aliases = alias_map(root)
    id_cache: dict = {}
    for page in pages(root):
        for line, source, number in expanded_body(page):
            line = re.sub(r"`[^`]*`", "", line)
            targets = [m.group(1) for m in MD_LINK.finditer(line)]
            targets += [m.group(1) for m in HTML_LINK.finditer(line)]
            for target in targets:
                if EXTERNAL.match(target) or target.startswith(("{{", "`")):
                    continue
                file_part, _, anchor = target.partition("#")
                if file_part:
                    resolved = resolve_target(root, page, file_part, aliases)
                    if resolved is None:
                        failures.append(Failure("link", rel(root, source), number,
                                                f"{target} points to a file that does not exist"))
                        continue
                else:
                    resolved = page
                if anchor and resolved.suffix in PAGE_SUFFIXES:
                    if anchor not in page_identifiers(resolved, id_cache):
                        failures.append(Failure("anchor", rel(root, source), number,
                                                f"{target}: no heading or element #{anchor} in {rel(root, resolved)}"))
    return failures


# ---------------------------------------------------------------------------
# Sidebar and community page
# ---------------------------------------------------------------------------

def quarto_lines(root: Path) -> list[tuple[int, str]]:
    """Lines of _quarto.yml without comments."""
    path = root / "_quarto.yml"
    if not path.is_file():
        return []
    out = []
    for i, line in enumerate(path.read_text(encoding="utf-8").split("\n"), start=1):
        stripped = re.sub(r"(^|\s)#.*$", "", line)
        if stripped.strip():
            out.append((i, stripped))
    return out


def check_sidebar(root: Path) -> list[Failure]:
    failures = []
    in_sidebar = False
    indent = None
    for number, line in quarto_lines(root):
        if re.match(r"^\s*sidebar:\s*$", line):
            in_sidebar, indent = True, len(line) - len(line.lstrip())
            continue
        if in_sidebar and len(line) - len(line.lstrip()) <= indent:
            in_sidebar = False
        if not in_sidebar:
            continue
        m = re.match(r"^\s*-?\s*(?:href:|file:)?\s*[\"']?([^\s\"']+\.(?:qmd|md|ipynb|html))[\"']?\s*$", line)
        if not m or EXTERNAL.match(m.group(1)):
            continue
        target = root / m.group(1)
        if not (target.is_file() or (target.suffix == ".html" and target.with_suffix(".qmd").is_file())):
            failures.append(Failure("sidebar", "_quarto.yml", number, f"{m.group(1)} does not exist"))
    return failures


COMMUNITY_EXCLUSION = re.compile(r"^\s*-\s*[\"']?!content/community\.qmd[\"']?\s*$")


def render_entries(root: Path) -> list[tuple[int, str]]:
    """Entries of the list under `project: render:` in _quarto.yml, with line numbers."""
    entries = []
    in_project = in_render = False
    project_indent = render_indent = 0
    for number, line in quarto_lines(root):
        indent = len(line) - len(line.lstrip())
        if re.match(r"^project:\s*$", line):
            in_project, project_indent = True, indent
            continue
        if in_project and indent <= project_indent:
            in_project = in_render = False
        if not in_project:
            continue
        if re.match(r"^\s*render:\s*$", line):
            in_render, render_indent = True, indent
            continue
        if in_render and (indent < render_indent or
                          (indent == render_indent and not line.lstrip().startswith("-"))):
            in_render = False
        if in_render:
            m = re.match(r"^\s*-\s*[\"']?([^\"']+?)[\"']?\s*$", line)
            if m:
                entries.append((number, m.group(1)))
    return entries


def check_community(root: Path) -> list[Failure]:
    failures = []
    for number, line in quarto_lines(root):
        if COMMUNITY_PATTERN.search(line) and not COMMUNITY_EXCLUSION.match(line):
            failures.append(Failure("community", "_quarto.yml", number,
                                    "the community page is hidden and must not be listed"))
    hidden = (root / "content" / "community.qmd").resolve()
    if hidden.is_file():
        entries = render_entries(root)
        exclusions = [n for n, e in entries if e == "!content/community.qmd"]
        includes = [n for n, e in entries if not e.startswith("!")]
        if not exclusions:
            failures.append(Failure("community", "_quarto.yml", 1,
                                    'the community page is still rendered: add - "!content/community.qmd" '
                                    "under project: render:, after the pages to render"))
        elif includes and min(exclusions) < max(includes):
            failures.append(Failure("community", "_quarto.yml", min(exclusions),
                                    "the exclusion of the community page must come after the pages to render"))
    for page in pages(root):
        if page.resolve() == hidden:
            continue
        for number, line in enumerate(page.read_text(encoding="utf-8").split("\n"), start=1):
            if COMMUNITY_PATTERN.search(line):
                failures.append(Failure("community", rel(root, page), number,
                                        "the community page is hidden: remove this link, card or counter"))
    return failures


# ---------------------------------------------------------------------------
# Methods pages: headings and numbers
# ---------------------------------------------------------------------------

def method_pages(root: Path) -> list[Path]:
    folder = root / "content" / "methods"
    if not folder.is_dir():
        return []
    return [p for p in sorted(folder.glob("*.qmd"))
            if p.name not in METHOD_PAGES_WITHOUT_TEMPLATE and not p.name.startswith("_")]


def h2_sections(page: Path) -> list[tuple[str, int, list[tuple[str, Path, int]]]]:
    """Second-level sections of a page: (heading text, line, lines below it)."""
    sections = []
    for line, source, number in expanded_body(page):
        m = re.match(r"^##\s+(.*?)\s*$", line)
        if m:
            text = ATTRS_AT_END.sub("", m.group(1)).strip()
            sections.append((text, number, []))
        elif sections:
            sections[-1][2].append((line, source, number))
    return sections


def check_headings(root: Path) -> list[Failure]:
    failures = []
    for page in method_pages(root):
        found = tuple(text for text, _, _ in h2_sections(page))
        if found != METHOD_HEADINGS:
            missing = [h for h in METHOD_HEADINGS if h not in found]
            extra = [h for h in found if h not in METHOD_HEADINGS]
            detail = []
            if missing:
                detail.append("missing: " + ", ".join(missing))
            if extra:
                detail.append("not in the template: " + ", ".join(extra))
            if not missing and not extra:
                detail.append("order differs from the template")
            failures.append(Failure("headings", rel(root, page), 0,
                                    "the second-level headings must be the nine of the methods "
                                    "template, in order (" + "; ".join(detail) + ")"))
    return failures


NUMBER = re.compile(r"(?<![\w.,])[-\u2212]?(\d{1,3}(?:,\d{3})+|\d+)(\.\d+)?(?![\w])")


def numbers_in(text: str) -> list[str]:
    """Numbers a reader sees in a line of Markdown."""
    text = re.sub(r"`[^`]*`", " ", text)                     # inline code and inline R
    text = re.sub(r"!\[[^\]]*\]\([^)]*\)", " ", text)        # images
    text = re.sub(r"\]\([^)]*\)", "]", text)                 # link targets
    text = re.sub(r"\{[^{}]*\}", " ", text)                  # attributes and shortcodes
    text = re.sub(r"<[^>]+>", " ", text)                     # HTML tags
    text = re.sub(r"\[\^[^\]]*\]|@[\w:-]+", " ", text)       # footnotes, citations, cross-references
    text = re.sub(r"\b\d{4}-\d{2}-\d{2}(T[\d:]+Z?)?\b", " ", text)  # dates
    found = []
    for m in NUMBER.finditer(text):
        found.append(m.group(0).replace("\u2212", "-"))
    return found


def as_float(text: str) -> float | None:
    try:
        return float(str(text).replace(",", "").replace("\u2212", "-").strip())
    except ValueError:
        return None


def read_measures(root: Path) -> list[dict]:
    path = root / MEASURES_CSV
    if not path.is_file():
        return []
    with path.open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def allowed_numbers(rows: list[dict]) -> list[float]:
    values = []
    for row in rows:
        for field in ("value", "n"):
            v = as_float(row.get(field, ""))
            if v is not None:
                values.append(v)
        values.extend(v for v in (as_float(t) for t in numbers_in(row.get("label", ""))) if v is not None)
    return values


def number_matches(written: str, allowed: list[float]) -> bool:
    x = as_float(written)
    if x is None:
        return True
    decimals = len(written.split(".")[1]) if "." in written else 0
    return any(abs(round(v, decimals) - x) < 1e-9 for v in allowed)


def check_numbers(root: Path) -> list[Failure]:
    failures = []
    pages_ = method_pages(root)
    if not pages_:
        return failures
    measures = read_measures(root)
    for page in pages_:
        allowed = allowed_numbers([r for r in measures if r.get("page") == page.stem])
        for heading, _, lines in h2_sections(page):
            if heading not in NUMBER_SECTIONS:
                continue
            for line, source, number in lines:
                for written in numbers_in(line):
                    if not number_matches(written, allowed):
                        failures.append(Failure(
                            "numbers", rel(root, source), number,
                            f"{written} in '{heading}' is not in {MEASURES_CSV} for page '{page.stem}'"))
    return failures


# ---------------------------------------------------------------------------
# Vocabulary and coverage
# ---------------------------------------------------------------------------

def check_vocabulary(root: Path) -> list[Failure]:
    failures = []
    patterns = [(re.compile(p, flags=re.I if not p.startswith(r"\bA(") else 0), advice)
                for p, advice in FORBIDDEN_WORDS]
    for page in pages(root):
        text = page.read_text(encoding="utf-8")
        front, body = split_front_matter(text)
        offset = text[: len(text) - len(body)].count("\n")
        lines = [(i, l) for i, l in enumerate(front.split("\n"), start=2)
                 if not re.match(r"^\s*(aliases|-\s)", l)]
        lines += list(enumerate(blank_code(body).split("\n"), start=offset + 1))
        for number, line in lines:
            line = re.sub(r"\]\([^)]*\)|https?://\S+|\b(?:href|src)=[\"'][^\"']*[\"']", " ", line)
            line = re.sub(r"\{\{<.*?>\}\}", " ", line)
            for pattern, advice in patterns:
                m = pattern.search(line)
                if m:
                    failures.append(Failure("vocabulary", rel(root, page), number,
                                            f"'{m.group(0)}' is on the list of words to avoid ({advice})"))
    return failures


def type_pattern(name: str) -> re.Pattern:
    parts = re.split(r"(\{[^}]*\})", name)
    regex = "".join(r"(?:\{[^}]*\}|[A-Za-z0-9_]+)" if p.startswith("{") else re.escape(p) for p in parts)
    return re.compile(rf"`{regex}`")


def check_coverage(root: Path) -> list[Failure]:
    page = root / "content" / "data" / "dictionary.qmd"
    if not page.is_file():
        return []
    text = "\n".join(line for line, _, _ in expanded_body(page))
    return [Failure("coverage", rel(root, page), 0,
                    f"event type {name} is not described (write it as inline code, e.g. `{name}`)")
            for name in EVENT_TYPES if not type_pattern(name).search(text)]


# ---------------------------------------------------------------------------
# Listing and command line
# ---------------------------------------------------------------------------

CHECKS = {
    "link": check_links,  # also reports kind "anchor"
    "sidebar": check_sidebar,
    "community": check_community,
    "headings": check_headings,
    "numbers": check_numbers,
    "vocabulary": check_vocabulary,
    "coverage": check_coverage,
}


def run_checks(root: Path, only: list[str] | None = None) -> list[Failure]:
    root = Path(root)
    failures = []
    for name, check in CHECKS.items():
        if only and name not in only and not (name == "link" and "anchor" in only):
            continue
        found = check(root)
        if only:
            found = [f for f in found if f.kind in only]
        failures.extend(found)
    return failures


def load_schema(opdi_src: Path):
    """Load opdi/output/schema.py from the pipeline source by file path."""
    import importlib.util
    path = Path(opdi_src) / "opdi" / "output" / "schema.py"
    if not path.is_file():
        raise FileNotFoundError(f"{path} does not exist; give the pipeline's src folder")
    spec = importlib.util.spec_from_file_location("opdi_published_schema", path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def list_types(root: Path, opdi_src: Path | None) -> None:
    print("# Event types (a part in braces stands for a family of types)")
    for name in EVENT_TYPES:
        print(f"event\t{name}")
    if opdi_src is not None:
        schema = load_schema(opdi_src)
        for kind, columns in (("flight list column", schema.FLIGHT_LIST),
                              ("event column", schema.EVENT_COLUMNS),
                              ("measurement", schema.MEASUREMENT_COLUMNS)):
            for column in columns:
                print(f"{kind}\t{column.name}")
        return
    dictionary = root / "assets" / "data" / "dictionary.csv"
    if dictionary.is_file():
        with dictionary.open(encoding="utf-8", newline="") as handle:
            for row in csv.DictReader(handle):
                print(f"{row.get('table', 'column')} column\t{row.get('name', '')}")
    else:
        print("# No column list: run tools/gen_data_dictionary.py, or give --opdi")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Check the OPDI website source.")
    parser.add_argument("--root", type=Path, default=ROOT, help="site folder (default: this repository)")
    parser.add_argument("--only", action="append", choices=sorted(set(CHECKS) | {"anchor"}),
                        help="run only this kind of check; may be given more than once")
    parser.add_argument("--list-types", action="store_true",
                        help="print the event types and columns the checks expect, then stop")
    parser.add_argument("--opdi", type=Path, default=None,
                        help="pipeline src folder; with --list-types, columns come from its schema")
    args = parser.parse_args(argv)

    if args.list_types:
        list_types(args.root, args.opdi)
        return 0

    failures = run_checks(args.root, args.only)
    for failure in failures:
        print(failure)
    if failures:
        counts = Counter(f.kind for f in failures)
        summary = ", ".join(f"{kind} {n}" for kind, n in sorted(counts.items()))
        print(f"FAIL: {len(failures)} problem(s): {summary}")
        return 1
    print(f"OK: {len(pages(args.root))} pages checked")
    return 0


if __name__ == "__main__":
    sys.exit(main())
