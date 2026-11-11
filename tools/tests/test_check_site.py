"""Tests for tools/check_site.py, run on a small website built in a temporary folder.

Run from the repository root:

    python -m pytest tools/tests
"""
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import check_site  # noqa: E402

NINE = check_site.METHOD_HEADINGS

CSV_HEADER = ",".join(check_site.MEASURE_COLUMNS)

QUARTO_YML = """\
project:
  type: website
website:
  sidebar:
    contents:
      - href: index.qmd
        text: Home
      - content/about.qmd
      - section: "Methods"
        contents:
          - content/methods/holding.qmd
      # To show the community page again, add:
      #   - href: content/community.qmd
"""


def method_page(results="The rule finds 74.0 % of the labelled time.", drop=None):
    parts = ["---", 'title: "Holding"', "---", ""]
    for heading in NINE:
        if heading == drop:
            continue
        parts.append(f"## {heading}")
        parts.append("")
        if heading == "Results":
            parts.append(results)
        elif heading == "Headline accuracy":
            parts.append("F1 score 74.0 % over 346 flights.")
        else:
            parts.append("Plain text.")
        parts.append("")
    return "\n".join(parts)


@pytest.fixture
def site(tmp_path):
    (tmp_path / "content" / "methods").mkdir(parents=True)
    (tmp_path / "assets" / "validation").mkdir(parents=True)
    (tmp_path / "_quarto.yml").write_text(QUARTO_YML)
    (tmp_path / "index.qmd").write_text(
        "---\ntitle: Home\n---\n\nRead [about us](content/about.qmd#who-we-are) "
        "and the [old page](content/old-name.html).\n"
    )
    (tmp_path / "content" / "about.qmd").write_text(
        "---\ntitle: About\naliases:\n  - old-name.html\n---\n\n## Who we are\n\n"
        "Back [home](../index.qmd). See [the 2025 edition](#prc-data-challenge-fuel-burn).\n\n"
        "## 2025 PRC Data Challenge — Fuel burn\n\nText.\n"
    )
    (tmp_path / "content" / "community.qmd").write_text("---\ntitle: Community\n---\n\nHello.\n")
    (tmp_path / "content" / "methods" / "holding.qmd").write_text(method_page())
    (tmp_path / "assets" / "validation" / "validation_measures.csv").write_text(
        CSV_HEADER + "\n"
        "holding,holding_f1,F1 score,74.0,%,346,Hand-corrected labels,"
        "papers/x/data/score.csv,f1,1,holding-start;holding-end,event\n"
    )
    return tmp_path


def kinds(root):
    return sorted({f.kind for f in check_site.run_checks(root)})


def test_clean_site_passes(site):
    failures = check_site.run_checks(site)
    assert failures == [], [str(f) for f in failures]
    assert check_site.main(["--root", str(site)]) == 0


def test_broken_link_fails(site):
    page = site / "content" / "about.qmd"
    page.write_text(page.read_text() + "\nSee [nothing](missing.qmd).\n")
    assert "link" in kinds(site)
    assert check_site.main(["--root", str(site)]) == 1


def test_broken_anchor_fails(site):
    page = site / "content" / "about.qmd"
    page.write_text(page.read_text() + "\nSee [here](#no-such-heading).\n")
    assert "anchor" in kinds(site)


def test_explicit_heading_id_is_honoured(site):
    page = site / "content" / "about.qmd"
    page.write_text(page.read_text() + "\n## Contact {#write-to-us}\n\nSee [here](#write-to-us).\n")
    assert kinds(site) == []


def test_missing_sidebar_page_fails(site):
    (site / "content" / "about.qmd").rename(site / "content" / "about-us.qmd")
    assert "sidebar" in kinds(site)


def test_methods_page_missing_heading_fails(site):
    (site / "content" / "methods" / "holding.qmd").write_text(method_page(drop="Limitations"))
    assert "headings" in kinds(site)


def test_number_not_in_csv_fails(site):
    (site / "content" / "methods" / "holding.qmd").write_text(
        method_page(results="The rule finds 81.3 % of the labelled time.")
    )
    assert "numbers" in kinds(site)


def test_number_rounded_from_csv_passes(site):
    (site / "content" / "methods" / "holding.qmd").write_text(
        method_page(results="The rule finds 74 % of the labelled time in `r value` cases.")
    )
    assert kinds(site) == []


def test_forbidden_word_rung_fails(site):
    page = site / "content" / "about.qmd"
    page.write_text(page.read_text() + "\nThe next rung of the study.\n")
    assert "vocabulary" in kinds(site)


def test_link_to_community_page_fails(site):
    page = site / "index.qmd"
    page.write_text(page.read_text() + "\n[Meet the community](content/community.qmd)\n")
    assert "community" in kinds(site)


def test_community_in_sidebar_fails(site):
    yml = site / "_quarto.yml"
    yml.write_text(yml.read_text() + "      - content/community.qmd\n")
    assert "community" in kinds(site)


def test_site_without_methods_section_passes(site):
    for path in (site / "content" / "methods").iterdir():
        path.unlink()
    (site / "content" / "methods").rmdir()
    yml = site / "_quarto.yml"
    yml.write_text(yml.read_text().replace("          - content/methods/holding.qmd\n", ""))
    assert kinds(site) == []


def test_heading_identifiers_follow_pandoc_rules():
    ident = check_site.heading_identifier
    assert ident("2025 PRC Data Challenge — Fuel burn") == "prc-data-challenge-fuel-burn"
    assert ident("{{< iconify fa6-solid trophy >}} PRC Data Challenges") == "prc-data-challenges"
    assert ident("Get *Involved*") == "get-involved"
    assert ident("The \\acr{OPDI} data") == "the-data"
    assert ident("12345") == "section"


def test_list_types_prints_event_types(site, capsys):
    assert check_site.main(["--root", str(site), "--list-types"]) == 0
    out = capsys.readouterr().out
    assert "holding-start" in out
    assert "line-up" in out


def test_gate_to_gate_is_allowed_but_gate_is_not(site):
    page = site / "content" / "about.qmd"
    page.write_text(page.read_text() + "\nWe follow the flight gate-to-gate and gate to gate.\n")
    assert kinds(site) == []
    page.write_text(page.read_text() + "\nThis gate rejects bad points.\n")
    assert "vocabulary" in kinds(site)
