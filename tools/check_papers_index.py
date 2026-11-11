#!/usr/bin/env python3
"""Check that every paper directory has exactly one entry in papers/index.qmd.

A paper directory is any directory directly under papers/ that holds a .qmd
file. Its main page is index.qmd when present, otherwise its only .qmd file
(a directory with several .qmd files and no index.qmd is reported, because
the main page cannot be told apart). The main page must be linked exactly
once from papers/index.qmd, as `](<dir>/<main>.qmd)`.

Plain Python 3, no dependencies. Exit status 0 when every paper is listed
once, 1 otherwise. Run from anywhere: paths resolve from this file.
"""
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PAPERS = ROOT / "papers"
INDEX = PAPERS / "index.qmd"


def main_page(directory: Path):
    qmds = sorted(p.name for p in directory.glob("*.qmd"))
    if not qmds:
        return None, None
    if "index.qmd" in qmds:
        return "index.qmd", None
    if len(qmds) == 1:
        return qmds[0], None
    return None, f"several .qmd files and no index.qmd: {', '.join(qmds)}"


def main() -> int:
    if not INDEX.is_file():
        print(f"missing {INDEX.relative_to(ROOT)}")
        return 1
    text = INDEX.read_text(encoding="utf-8")
    targets = re.findall(r"\]\(([^)\s#]+)", text)

    problems = []
    listed = 0
    for directory in sorted(p for p in PAPERS.iterdir() if p.is_dir()):
        if directory.name.startswith(("_", ".")):
            continue
        page, error = main_page(directory)
        if error:
            problems.append(f"papers/{directory.name}/: {error}")
            continue
        if page is None:
            continue
        link = f"{directory.name}/{page}"
        n = targets.count(link)
        if n == 0:
            problems.append(f"papers/{directory.name}/: {link} is not referenced from papers/index.qmd")
        elif n > 1:
            problems.append(f"papers/{directory.name}/: {link} is referenced {n} times from papers/index.qmd")
        else:
            listed += 1

    for line in problems:
        print(line)
    if problems:
        print(f"FAIL: {len(problems)} problem(s); {listed} paper(s) listed once")
        return 1
    print(f"OK: {listed} paper directories, each listed once in papers/index.qmd")
    return 0


if __name__ == "__main__":
    sys.exit(main())
