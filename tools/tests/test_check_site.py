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
  render:
    - index.qmd
    - content/
    - "!content/community.qmd"
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


@pytest.mark.parametrize("sentence", [
    "This step is crucial for the result.",
    "A comprehensive list of events.",
])
def test_vague_words_fail(site, sentence):
    page = site / "content" / "about.qmd"
    page.write_text(page.read_text() + "\n" + sentence + "\n")
    assert "vocabulary" in kinds(site)


def test_link_to_community_page_fails(site):
    page = site / "index.qmd"
    page.write_text(page.read_text() + "\n[Meet the community](content/community.qmd)\n")
    assert "community" in kinds(site)


def test_community_in_sidebar_fails(site):
    yml = site / "_quarto.yml"
    yml.write_text(yml.read_text() + "      - content/community.qmd\n")
    assert "community" in kinds(site)


def test_community_page_not_excluded_from_rendering_fails(site):
    yml = site / "_quarto.yml"
    yml.write_text(yml.read_text().replace('    - "!content/community.qmd"\n', ""))
    assert "community" in kinds(site)


def test_community_page_without_render_list_fails(site):
    yml = site / "_quarto.yml"
    yml.write_text(yml.read_text().replace(
        '  render:\n    - index.qmd\n    - content/\n    - "!content/community.qmd"\n', ""))
    assert "community" in kinds(site)


def test_community_exclusion_before_includes_fails(site):
    yml = site / "_quarto.yml"
    yml.write_text(yml.read_text().replace(
        '    - index.qmd\n    - content/\n    - "!content/community.qmd"\n',
        '    - "!content/community.qmd"\n    - index.qmd\n    - content/\n'))
    assert "community" in kinds(site)


def test_community_exclusion_not_needed_without_page(site):
    (site / "content" / "community.qmd").unlink()
    yml = site / "_quarto.yml"
    yml.write_text(yml.read_text().replace('    - "!content/community.qmd"\n', ""))
    assert kinds(site) == []


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


# --- algorithm pages, figures, pointers and identifiers ----------------------

ALGO = check_site.ALGORITHM_HEADINGS
FIG_HEADER = "page,measure_id,label,value,unit,n,kind,badge,reference,source_file,source_field"


def algorithm_page(figures="Found for 80.3 % of 19,062 records.", drop=None):
    parts = ["---", 'title: "Holding"', "---", ""]
    for heading in ALGO:
        if heading == drop:
            continue
        parts += [f"## {heading}", "", figures if heading == "Availability and accuracy" else "Text.", ""]
    return "\n".join(parts)


@pytest.fixture
def algo_site(site):
    folder = site / "content" / "algorithms"
    folder.mkdir()
    (folder / "holding.qmd").write_text(algorithm_page())
    (site / "assets" / "algorithms").mkdir()
    (site / "assets" / "algorithms" / "item_figures.csv").write_text(
        FIG_HEADER + "\nholding,found,Recorded holds found,80.3,%,19062,availability,high,"
        "records,papers/x/data/s.csv,row\n")
    return site


def test_algorithm_page_on_template_passes(algo_site):
    assert kinds(algo_site) == []


def test_algorithm_page_missing_heading_fails(algo_site):
    (algo_site / "content" / "algorithms" / "holding.qmd").write_text(algorithm_page(drop="Based on"))
    assert "headings" in kinds(algo_site)


def test_algorithm_figure_not_in_csv_fails(algo_site):
    (algo_site / "content" / "algorithms" / "holding.qmd").write_text(
        algorithm_page(figures="Found for 81.0 % of the records."))
    assert "numbers" in kinds(algo_site)


def test_algorithms_index_must_list_every_event_type(algo_site):
    (algo_site / "content" / "algorithms" / "index.qmd").write_text(
        "---\ntitle: Algorithms\n---\n\nOnly `holding-start` here.\n")
    failures = [f for f in check_site.run_checks(algo_site) if f.kind == "coverage"]
    assert any("line-up" in f.message for f in failures)


def test_track_identifier_fails_but_documented_example_passes(site):
    page = site / "content" / "about.qmd"
    page.write_text(page.read_text() + f"\nFor example `{check_site.EXAMPLE_TRACK_ID}`.\n")
    assert kinds(site) == []
    page.write_text(page.read_text() + "\nSee track `0a1b2c3d4e5f6789_1781412000`.\n")
    assert "identifiers" in kinds(site)


def test_pointer_page_is_exempt_from_methods_template(algo_site):
    (algo_site / "content" / "methods" / "holding.qmd").write_text(
        "---\ntitle: Holding\nmoved-to: ../algorithms/holding.qmd\n---\n\n"
        "See [Holding](../algorithms/holding.qmd).\n")
    assert kinds(algo_site) == []
    (algo_site / "content" / "methods" / "holding.qmd").write_text(
        "---\ntitle: Holding\nmoved-to: ../algorithms/nothing.qmd\n---\n\nMoved.\n")
    assert "pointer" in kinds(algo_site)


def accuracy_site(site, prose):
    (site / "content" / "accuracy" / "_generated").mkdir(parents=True)
    (site / "assets" / "pages").mkdir(parents=True)
    (site / "assets" / "pages" / "accuracy_numbers.csv").write_text(
        "page,measure_id,label,value,n,source_file\n"
        "accuracy/index,atot_ref,recorded take-offs with an ATOT,80.8,,x.csv\n"
        "limitations,adep_missing,flights without adep,24.2,,x.csv\n")
    (site / "content" / "accuracy" / "_generated" / "t.md").write_text("| a |\n|---|\n| 12.5 % |\n")
    (site / "content" / "accuracy" / "index.qmd").write_text(
        f"---\ntitle: Accuracy\n---\n\n{prose}\n\n{{{{< include _generated/t.md >}}}}\n")
    return site


def test_registered_number_on_accuracy_page_passes(site):
    assert kinds(accuracy_site(site, "Of the take-offs, 80.8 % have a time.")) == []


def test_unregistered_number_on_accuracy_page_fails(site):
    assert "numbers" in kinds(accuracy_site(site, "Of the take-offs, 81.3 % have a time."))


def test_measured_span_must_be_registered(site):
    accuracy_site(site, "Text.")
    page = site / "content" / "about.qmd"
    page.write_text(page.read_text() + "\nA threshold of 30 minutes is not checked.\n")
    assert kinds(site) == []
    (site / "content" / "limitations.qmd").write_text(
        "---\ntitle: Limitations\n---\n\nAbout [24.2 %]{.measured} of flights, [7.7 %]{.measured} of others.\n")
    failures = [f for f in check_site.run_checks(site) if f.kind == "numbers"]
    assert len(failures) == 1 and "7.7" in str(failures[0])


def test_quality_pages_sit_under_data():
    text = (Path(__file__).parents[2] / "_quarto.yml").read_text()
    data_start = text.index('- section: "Data"')
    algo_start = text.index('- section: "Algorithms"')
    data_menu = text[data_start:algo_start]
    for page in (
        "content/limitations.qmd",
        "content/data/limitations.qmd",
        "content/accuracy/index.qmd",
        "content/accuracy/europe.qmd",
        "content/accuracy/world.qmd",
    ):
        assert page in data_menu, page
    assert 'section: "Limitations"' in data_menu
    assert 'section: "Accuracy and availability"' in data_menu
    after_algorithms = text[algo_start:]
    assert "content/limitations.qmd" not in after_algorithms
    assert "content/accuracy/" not in after_algorithms
