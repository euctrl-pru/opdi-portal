#!/usr/bin/env bash
# post-render: give each paper's PDF a real name, then publish the built site
# into the portal's _site/papers.
#
# The PDF is a deliverable people email and print, so a copy lives next to the
# .qmd where it can be found and linked, not only under _site. Doing that here
# rather than by hand is what keeps the two in step: a hand-copied PDF goes
# stale the first time someone edits the source and forgets, and a stale PDF of
# a paper whose numbers have changed is worse than no PDF at all.
#
# Order matters. The named copy beside the source is itself a project resource,
# so Quarto picks it up on the *next* render and copies it into the build. If
# publishing ran first, the site would serve the previous render's PDF -- which
# is exactly the staleness this script exists to prevent, just moved one step
# along. So: name it, place it in the build, and only then publish.
#
# Quarto runs post-render with the project directory (papers/) as cwd.
set -euo pipefail

BUILD=_site
PUBLISH=../_site/papers

[ -d "$BUILD" ] || { echo "no build output at $BUILD"; exit 0; }

for src in "$BUILD"/*/index.pdf; do
  [ -e "$src" ] || continue
  paper=$(basename "$(dirname "$src")")
  [ -d "$paper" ] || continue
  # Name it after the paper: a downloaded "index.pdf" is anonymous on disk.
  cp "$src" "$paper/$paper.pdf"          # beside the source, for the repo
  cp "$src" "$BUILD/$paper/$paper.pdf"   # into the build, for the download link
  echo "saved $paper/$paper.pdf"
done

# A paper directory can hold more than one document -- a reply to a review, say.
# Those already carry their own name, so they only need placing beside the
# source. Without this they exist solely inside the build, which is the one
# place nobody looks for a deliverable they were asked to send.
for src in "$BUILD"/*/*.pdf; do
  [ -e "$src" ] || continue
  paper=$(basename "$(dirname "$src")")
  doc=$(basename "$src" .pdf)
  [ -d "$paper" ] || continue
  [ "$doc" = "index" ] && continue
  [ "$doc" = "$paper" ] && continue        # already handled above
  [ -f "$paper/$doc.qmd" ] || continue     # only documents this paper owns
  cp "$src" "$paper/$doc.pdf"
  echo "saved $paper/$doc.pdf"
done

# Replace wholesale so files deleted from a paper do not survive in the site.
rm -rf "$PUBLISH"
mkdir -p "$PUBLISH"
cp -r "$BUILD"/. "$PUBLISH"/
echo "published -> $PUBLISH"
