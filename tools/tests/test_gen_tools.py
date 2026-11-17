"""Tests for the generators in tools/: data dictionary, figures and validation summary."""
import csv
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import draw_validation_figures as figures  # noqa: E402
import gen_data_dictionary  # noqa: E402
import gen_validation_summary  # noqa: E402
from check_site import MEASURE_COLUMNS  # noqa: E402

STUB = Path(__file__).resolve().parent / "stub_opdi"


def test_dictionary_tables_from_schema(tmp_path):
    assert gen_data_dictionary.main(["--opdi", str(STUB), "--root", str(tmp_path)]) == 0
    md = (tmp_path / "content/data/_generated/flight_list_columns.md").read_text()
    assert "| Column | Type | Unit | Description |" in md
    assert "Departure airport \\| ICAO code." in md
    events = (tmp_path / "content/data/_generated/flight_events_columns.md").read_text()
    assert "`event_id`" in events and "`flight_version`" in events and "`adep`" not in events
    # Event columns that share a name with a flight list column are still
    # columns of the events file, with their own description.
    assert "`flight_id`" in events
    assert "Version of the event method." in events
    assert "Version of the flight row method." not in events
    rows = list(csv.DictReader((tmp_path / "assets/data/dictionary.csv").open()))
    assert {r["table"] for r in rows} == {"flight_list", "flight_events"}
    assert gen_data_dictionary.main(["--opdi", str(STUB), "--root", str(tmp_path), "--check"]) == 0
    (tmp_path / "assets/data/dictionary.csv").write_text("stale\n")
    assert gen_data_dictionary.main(["--opdi", str(STUB), "--root", str(tmp_path), "--check"]) == 1


def row(page, measure_id, value, headline="0"):
    base = {c: "" for c in MEASURE_COLUMNS}
    base.update(page=page, measure_id=measure_id, label=measure_id, value=value, headline=headline)
    return base


def test_upsert_replaces_rows_by_page_and_measure(tmp_path):
    path = tmp_path / "validation_measures.csv"
    figures.upsert_measures([row("holding", "f1", "0.70"), row("holding", "recall", "0.71")], path)
    figures.upsert_measures([row("holding", "f1", "0.74"), row("track-identity", "f1", "0.99")], path)
    rows = list(csv.DictReader(path.open()))
    assert [(r["page"], r["measure_id"], r["value"]) for r in rows] == [
        ("holding", "f1", "0.74"), ("holding", "recall", "0.71"), ("track-identity", "f1", "0.99")]
    assert list(rows[0].keys()) == list(MEASURE_COLUMNS)


def test_summary_lists_unvalidated_items(tmp_path):
    measures = tmp_path / "validation_measures.csv"
    r = row("holding", "f1", "0.74", headline="1")
    r.update(item="holding-start;holding-end", kind="event", unit="", n="346", reference="Hand-corrected labels")
    figures.upsert_measures([r], measures)
    out = tmp_path / "summary.md"
    assert gen_validation_summary.main(["--measures", str(measures), "--out", str(out)]) == 0
    text = out.read_text()
    assert "`holding-start`" in text and "0.74" in text
    assert "holding.qmd#validation-method" in text
    line = next(l for l in text.splitlines() if "`first_seen`" in l)
    assert "Not validated" in line


def test_every_event_type_has_a_methods_page():
    import check_site
    pages = gen_validation_summary.METHOD_PAGES["event"]
    assert [name for name in check_site.EVENT_TYPES if name not in pages] == []
    for page in set(pages.values()):
        assert (check_site.ROOT / "content" / "methods" / f"{page}.qmd").is_file(), page


def test_summary_links_unvalidated_items_to_their_page(tmp_path):
    measures = tmp_path / "validation_measures.csv"
    figures.upsert_measures([row("holding", "f1", "0.74", headline="1")], measures)
    out = tmp_path / "summary.md"
    assert gen_validation_summary.main(["--measures", str(measures), "--out", str(out)]) == 0
    line = next(l for l in out.read_text().splitlines() if "`line-up`" in l)
    assert "Not validated" in line and "runway-milestones.qmd#limitations" in line


def test_item_figure_labels_come_from_the_staged_table():
    import gen_item_figures as g
    row = {"availability_own_label": "partial", "availability_ref_label": "high", "accuracy_label": "good"}
    own = g.own("p", "m", "l", "ATOT")
    ref = g.found("p", "m", "l", "ATOT")
    acc = g.w60("p", "m", "l", "ATOT")
    med = g.median("p", "m", "l", "ATOT")
    assert [g.badge(f, row) for f in (own, ref, acc, med)] == ["partial", "high", "good", ""]


def test_item_figures_link_to_the_airport_tables():
    import gen_item_figures as g
    assert "europe.qmd#take-off" in g.airport_line("take-off-landing")
    assert "world.qmd#all-items" in g.airport_line("tops")


def test_item_figures_are_up_to_date():
    import gen_item_figures as g
    assert g.main(["--check"]) == 0


def test_accuracy_tables_are_up_to_date():
    import gen_accuracy_tables as g
    assert g.main(["--check"]) == 0


def test_accuracy_summary_cell_keeps_both_labels():
    import gen_accuracy_tables as g
    r = {"availability_own_label": "low", "availability_own_pct": "0.35", "accuracy_label": "too few compared"}
    assert g.summary_cell(r, True) == "low · too few compared"
    assert g.summary_cell(r, False) == "0.3 % low"
    assert g.summary_cell(dict(r, availability_own_label="", availability_own_pct=""), True).startswith("no flights")


def test_every_staged_item_has_a_name_and_a_page():
    import gen_accuracy_tables as g
    items = {r["item"] for r in g.read(g.ROOT, "item_accuracy.csv")}
    assert items <= set(g.ITEM_NAME)
    for page in set(g.ITEM_PAGE.values()):
        assert (g.ROOT / "content" / "algorithms" / f"{page}.qmd").is_file(), page


def test_every_airport_appears_once_in_each_summary():
    import gen_accuracy_tables as g
    for name, staged in (("europe_summary", "airport_accuracy_europe.csv"),
                         ("world_summary", "airport_accuracy_world.csv")):
        text = (g.ROOT / g.OUT_DIR / f"{name}.md").read_text(encoding="utf-8")
        codes = {r["airport"] for r in g.read(g.ROOT, staged)}
        assert len(codes) == 100
        for code in codes:
            assert text.count(f"| `{code}` |") == 1, (name, code)
