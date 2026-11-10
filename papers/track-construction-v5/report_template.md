## 1. Summary

A flight that loses ADS-B reception for more than 30 minutes used to come out of OPDI as two tracks, and so as two rows in the flight list, neither with both airports. Version 4 of this study proposed a rule, `cruise_linked`, that keeps one `track_id` across such a gap when the data on both sides fits one continuous flight, and measured it. It also found two things that broke across the gap and recommended against making the rule the default until they were fixed.

This version fixes them, traces the side effects version 4 could not explain, has the rule audited independently, re-measures everything on the final code, and makes `cruise_linked` the default. In short:

- **Flights crossing the edge of the European box come out in one track {{ crossing_new }} of the time, against {{ crossing_base }} under the previous rule** ({{ crossing_flights }} such flights over two days). All flights: {{ all_base }} to {{ all_new }}.
- **Wrong joins stay rare.** {{ false_joins_phrase }} among {{ labelled_holes }} checkable linked gaps ({{ false_join_rate }}); {{ limit_joined }} of {{ limit_controls }} artificial gaps hiding a real landing were joined.
- **In a full pipeline run on one airframe in ten**, flights crossing the box with both airports right on one row went from {{ s2_both_base }} to {{ s2_both_new }} of {{ s2_crossing }}. Take-off and landing detections did not fall ({{ movement_change }}).
- **Reappearing on descent:** {% if descent_adopted %}a hole can now also link when the aircraft reappears descending below the cruise floor. The three false joins that kept this off in version 4 were estimated truth times (section 6.2).{% else %}no condition tried brought the descent case within the limits, so it stays split (section 6.2).{% endif %}
- **Nothing is computed across a gap any more.** Level segments spanning a gap: {{ level_tracks }} of {{ level_scored }} linked tracks checked (version 4: most of them). Cumulative distance across a gap is within 1 NM of the straight line on {{ dist_within_share }} of gaps checked.
- **A distance bug that affected every track is fixed.** Cumulative distance skipped every sample whose position cleaning had blanked, which left tracks {{ nohole_missing_share }} short in total on the day measured. That had nothing to do with gaps and had gone unnoticed.
- **The flight list named the wrong airport about 1% of the time**, through a Spark aggregation that does not return the row it was meant to. Fixed; this is what version 4 saw as airports "moving" on tracks the rule had not touched.

Making this the default changes `track_id` for everything produced from now on. Section 8 says what that means for anyone using the data and how to get the previous rule back.

## 2. The problem

Take a flight from New York to Frankfurt. Receivers hear it leave New York, lose it over the ocean, and hear it again near Ireland hours later. The previous rule sees a silence of over 30 minutes and starts a new track, so the flight list says this (an illustration, not a record from the data):

| Rule and row | Track | ADEP | ADES |
|---|---|---|---|
| Previous rule, first row | first piece | KJFK | none |
| Previous rule, second row | second piece | none | EDDF |
| New rule, the only row | one track | KJFK | EDDF |

Anyone counting flights from New York to Frankfurt counts none under the previous rule. Over the two days studied, the {{ crossing_flights }} flights crossing the edge of the European box made {{ crossing_base_tracks }} tracks, {{ crossing_rows_base }} per flight.

## 3. Words used in this report

- A **track** is one `track_id`: the samples OPDI treats as one flight. Tracks are built per aircraft (per `icao24` address) by deciding, sample by sample, whether a new track starts.
- A **hole** is a silence of more than 30 minutes between two consecutive samples of one track. Under the previous rule no track had one, because every such silence started a new track. Under the new rule a track can hold one or more.
- The **edges** of a hole are the samples just before and just after it. The rule reads its evidence there: altitude, vertical rate, speed, position and callsign.
- A **piece** is the run of samples between two holes (or between a hole and the end of the track). A track without a hole is one piece.
- A **linked hole** is a hole the rule decided to keep inside one track rather than split at.
- A **false join** is a linked hole whose two sides belong to two different flights of the same aircraft: the aircraft landed and took off again inside the hole, unseen. The landing and the take-off vanish from the data, and the flight list pairs an airport from one flight with an airport from the other. A split, by contrast, is visible and leaves two truthful pieces, so a false join is treated as the worse mistake throughout.

## 4. The rule

A hole stays inside one track only if all of these hold. If a value a check needs is missing, the check fails and the hole splits, except where the table says otherwise.

| Check | Setting | Why |
|---|---|---|
| Hole not too long | at most {{ max_gap_min }} min | Longer silences leave too much room for anything to have happened. |
| At cruise or climbing before the hole | lowest of the nearest {{ edge_samples }} altitudes within {{ edge_position_min }} min before the hole at or above {{ floor_ft }} ft; or, below that, climbing: mean of the nearest {{ edge_samples }} vertical rates above {{ vert_rate_fpm }} ft/min, resting on at least {{ min_rates }} of them | An aircraft lost at cruise, or still climbing out, is en route. |
| At cruise or descending after the hole | lowest of the nearest {{ edge_samples }} altitudes within {{ edge_position_min }} min after the hole at or above {{ floor_ft }} ft; or, below that but at or above 5,000 ft, descending: mean of the nearest {{ edge_samples }} vertical rates below −{{ vert_rate_fpm }} ft/min, resting on at least {{ min_rates }} of them | The same at the other side; an aircraft reappearing on its descent is arriving (section 6.2). |
| No descent then climb | not both descending faster than {{ vert_rate_fpm }} ft/min before and climbing faster than that after; a blank vertical rate does not count against it | That is what a stop inside the hole looks like. |
| Callsign agrees | the last callsign before the hole (at most {{ lookback_min }} min old) and the first after it (within {{ lookahead_min }} min) are not both present and different; blank counts as agreeing | A different callsign is a different flight. A blank says nothing. |
| Straight-line speed not too slow | at least {{ min_speed_kt }} kt: distance between the edge positions divided by the time between them | A stop uses up time, which lowers this speed. |
| Straight-line speed not impossible | at most {{ max_speed_kt }} kt | Faster than this is two aircraft sharing an address, not wind. |
| Real address | `icao24` not `000000`, `000001` or `ffffff` | These placeholders are shared by many aircraft. |

Every edge value is read past blank samples, within {{ edge_position_min }} min of the hole, because ADS-B sends position and altitude in separate messages and the sample right at an edge often lacks one of them. Everything else stays as under the previous rule: a callsign change, and a short silence at low altitude, still split. No sample is invented; the hole stays empty and only the identity changes.

One consequence for how OPDI runs: because a track can now reach back across a hole of up to {{ max_gap_min }} min, each day's run reads {{ read_back_hours }} hours of the previous day instead of 3, so that both days see the same evidence and decide a hole near midnight the same way.

## 5. How it was tested

Truth is Network Manager's European flight data for {{ day_new }} and {{ day_old }}, the two worldwide days with every table needed. Four tests:

| Test | What it does | What should happen |
|---|---|---|
| Real holes | Each linked hole checked against the truth, matched on aircraft and time (never on callsign, which the rule itself uses). True link: the hole lies inside one flight. False join: a landing or take-off of that aircraft falls inside the hole. Otherwise unknown. | No false joins |
| Hidden landing | Two consecutive flights of one aircraft. Delete from a cruise point in the first to a cruise point in the second, so a real landing sits inside an artificial hole. | Not joined |
| Cut flight | Delete the middle of one continuous flight, for {{ gap_60 }} to {{ gap_360 }} min. | Rejoined |
| Full pipeline day | {{ day_new }}, one airframe in ten, through the whole pipeline under both rules, from state vectors to the flight list, events and measurements. | More flights right, nothing else changed |

The limits a setting had to meet are version 4's: false joins at most 0.1% of checkable real holes; at most 1% of hidden landings joined, read as a 95% upper bound; at most 5% where the two flights share a callsign or one is blank, the case the callsign check cannot catch. The settings were chosen in version 4 and are not re-tuned here; this version measures them again on the final code.

In addition, the rule's code was audited by a reviewer who had not written it. The reviewer wrote a plain-Python copy of the rule from its documentation and ran it against the real Spark rule on {{ audit_decisions }} decisions over randomly generated aircraft crowding every threshold: the two agreed on every one. The audit also compared two day-runs of the same data and found no linking decision that differed. What it did find is in section 7.

## 6. Results

### 6.1 Two days against Network Manager

{{ table_one_track }}

"In one track" says the flight is not split. It does not say the track holds nothing else; the false-join tests answer that. The counts include {{ flights_no_track }} flights with no track at all, which read as not in one track.

| Evidence | Result | Reading |
|---|---|---|
| Real holes that can be checked | {{ false_joins_phrase }} of {{ labelled_holes }} ({{ false_join_rate }}) | Within the 0.1% limit. |
| Real holes that cannot be checked | {{ unknown_holes }} of {{ linked_holes }} ({{ unknown_share }}) | No evidence either way: the truth covers Europe only. |
| Real holes linked only because the aircraft was climbing before the hole | {{ climb_false }} false joins of {{ climb_labelled }} checkable | Still provisional: fewer than the 300 cases version 4 wanted. Zero of {{ climb_labelled }} still allows up to {{ climb_upper }} at 95% confidence. |
| Hidden landing | {{ limit_joined }} of {{ limit_controls }} joined | At most {{ limit_upper }}, at 95% confidence. |
| Hidden landing, same or blank callsign only | {{ same_joined }} of {{ same_controls }} joined | At most {{ same_upper }}. |
| Cut flight | {{ positive_rejoined }} of {{ positive_tests }} rejoined ({{ positive_rate }}) | |

With the climbing-edge allowance switched off, crossing flights in one track would be {{ crossing_no_climb }} instead of {{ crossing_new }}; the allowance stays on, as version 4 decided.

### 6.2 Reappearing on descent

Version 4 let an aircraft lost while still *climbing* link, but not one that reappears *descending* below the cruise floor: holes linked only that way showed 3 false joins in 119 checkable cases, over the 0.1% limit. With so few cases that applied the limit without explaining anything. This version looked at those holes.

A hole "linked only by descent" has every other check passing and a new edge below {{ floor_ft }} ft, at or above 5,000 ft, descending faster than {{ vert_rate_fpm }} ft/min on at least {{ min_rates }} vertical-rate samples. The danger is specific: an aircraft that landed and took off again inside the hole reappears descending towards the *next* flight's destination, and "descending" alone cannot tell the two apart.

**The three false joins were not false joins.** Each is a flight from Europe to Seoul whose Network Manager arrival time, an estimate because Seoul lies outside its area, falls one to six minutes *before* an ADS-B sample showing the aircraft still airborne and descending, with the same callsign on both sides. The holes are {{ relabelled_gap_range }} minutes long and were crossed at {{ relabelled_speed_range }} kt in a straight line, which leaves no time for a landing. The study's labelling now takes an event at an aerodrome outside the European box as no evidence either way, for every hole, not only these; on the two days that changes the label of {{ descent_relabelled }} holes linked only by descent and {{ relabelled_other }} other linked holes, so the false joins among all checkable linked holes go from 3 in version 4 to {{ false_joins }} here.

Three extra conditions had also been fixed in advance and were tried, alone and combined, on every hole linked only by descent over the two days ({{ descent_holes_exported }} holes with a descending new edge, {{ descent_controls_exported }} hidden-landing tests of the same shape):

- **C1 floor:** the new edge is still at or above {{ descent_floor_ft }} ft, so the aircraft is further from any airport.
- **C2 callsign:** a callsign is present on both sides of the hole and equal. A blank says nothing, and every hidden landing that got through in version 4 had a blank or equal callsign.
- **C3 shorter hole:** the hole is at most {{ descent_max_gap_min }} min.

A condition qualifies if, on the holes it keeps, there is no false join among those that can be checked, at most 1% of hidden landings are joined (as a 95% upper bound), at most 5% of those with the same or a blank callsign, at least 50 holes can be checked, and at least 20 of the holes it keeps lie on flights crossing the box.

{{ table_descent_candidates }}

{% if descent_adopted %}**Adopted: {{ descent_chosen }}.** With the labels corrected, the descending edge as defined meets every limit on its own: {{ descent_kept_false }} false joins among {{ descent_kept_checkable }} checkable holes, {{ descent_kept_hidden }} of {{ descent_kept_controls }} hidden landings joined (at most {{ descent_kept_hidden_upper }} at 95% confidence; the straight-line speed check rejects them before the edge is asked), and {{ descent_kept }} holes linked, {{ descent_crossing_kept }} of them on flights crossing the box. Each extra condition only removed true links. With it, flights crossing the box in one track go from {{ crossing_no_descent }} to {{ crossing_new }}. Like the climbing edge, it rests on about a hundred checkable cases and is provisional.{% else %}**Nothing adopted.** No condition brought the descent case within the limits on these two days, so a new edge below the floor still splits unless it is at cruise. The table is kept so the question is not reopened without new data.{% endif %}

### 6.3 One day through the whole pipeline

{{ day_new }}, one airframe in ten, both rules run through every step. "Previous rule" is `debounced`; "new rule" is `cruise_linked`.

| What | Previous rule | New rule |
|---|---|---|
| Flights crossing the box with both airports right on one row (of {{ s2_crossing }}) | {{ s2_both_base }} | {{ s2_both_new }} |
| Rows in the flight list per crossing flight | {{ s2_rows_base }} | {{ s2_rows_new }} |
| All flights with both airports right on one row (of {{ s2_all }}) | {{ s2_all_both_base }} | {{ s2_all_both_new }} |
| Take-offs and landings detected, against {{ movements_truth }} in the truth | {{ movements_base }} | {{ movements_new }} ({{ movement_change }}) |
| Linked holes in the day | — | {{ s2_linked_holes }} on {{ s2_linked_tracks }} tracks |
| Level segments spanning a hole | — | {{ level_tracks }} of {{ level_scored }} tracks checked |
| Cumulative distance across a hole, difference from the straight line between the edges | — | median {{ dist_median }} NM; {{ dist_off }} of {{ dist_compared }} holes off by over 1 NM |
| Flight-list rows without a track | {{ orphans_base }} | {{ orphans_new }} |
| Flight-list rows naming an airport that is also in their own list of alternatives | {{ pick_dup_base }} | {{ pick_dup_new }} |
| Tracks identical under both rules: {{ unchanged_tracks }}. Of those, with different airports / different events | — | {{ unchanged_other_airports }} / {{ unchanged_other_events }} |
| Running time, steps 02 to 04b | {{ time_base }} | {{ time_new }} |

Everything that is not a hole came out the same under both rules, which is the point of the last rows: version 4 could not say this. Of the previous rule's {{ s2_boundaries_base }} track boundaries, the new rule removed {{ s2_removed_boundaries }}: {{ s2_removed_at_holes }} at a linked hole, and {{ s2_memory_removed }} removed and {{ s2_memory_added }} added because the new rule forgets a callsign not heard for ten hours where the previous rule never forgets one (section 7). None moved for any other reason.

{{ table_events }}

`first_seen`, `last_seen` and the tops of climb and descent each fall by {{ s2_scored_holes }}, the number of linked holes inside the flights scored: each is one per track, and two pieces are now one track. `AOBT` and `AIBT` (off-block and on-block) fall by {{ ev_aobt }} and {{ ev_aibt }} for the same reason: the block detector gives a track at most one of each, the first off-block and the last on-block. This is not evidence of glued flights: all {{ truth_holes }} linked holes of the sample were labelled against every Network Manager flight of the aircraft, whatever its day, and {{ truth_false }} are false joins ({{ truth_true }} are true links, {{ truth_unknown }} unknown, {{ truth_out }} on aircraft Network Manager does not know). Block events on both sides of a hole occur on {{ truth_true_both }} of the {{ truth_true }} true links too, so they are not proof of a stop. The small rises in level-off and flight-level crossings come from pieces that now carry their track's cumulative measures across the hole.

## 7. What changed since version 4

**Detectors stop at a hole.** The event detectors that pair or interpolate samples, level segments and flight-level and ring crossings, now work per piece. A level segment can no longer begin before a hole and end after it, and a crossing cannot be placed at an instant inside one. Detectors that read a whole track, such as first and last seen, tops of climb and descent and the runway and block milestones, are unchanged. A track without a hole gives exactly the events it gave before, and a test holds that.

**Cumulative distance reads the last known position.** The released measurement (`distance_v0.0.2`) measured each sample against the row before it, so a sample whose position cleaning had blanked contributed nothing, and neither did the sample after it. On {{ day_new }}, {{ nohole_missing_share }} of the distance flown was missing from tracks that have no hole at all: {{ nohole_short_tracks }} of {{ nohole_tracks }} tracks were over 1 NM short, by a median of {{ nohole_median_short }} NM. The new measurement (`distance_v0.0.3`) reads from the last position that was known, and across a hole it adds the straight line between the hole's edges, a lower bound on what was flown and the only figure the data supports. Reprocessing a released month with the legacy configuration still gives `distance_v0.0.2`.

**The flight list names the aerodrome it ranked first.** After ranking the candidate aerodromes, the code took the first row of each group with Spark's `first()`, which returns whichever row the aggregation meets first, not the row ranked first. That is usually the same row, but not once a task holds enough groups for Spark to fall back to a sort-based aggregation; the test that reproduces it fails on exactly the 128th flight in a task. About 1% of named airports were in fact one of the alternatives, and the named airport then also appeared in its own list of alternatives. Version 4 saw this as airports changing between the two rules on tracks the rules had not touched. Exact ties between two candidates, an aerodrome listed under two identifiers, are now settled by the identifier.

**A day run rebuilds its endpoint candidates.** The table of candidate aerodromes for each track's two ends is a cache. Version 4's pipeline day re-used one built from an earlier set of tracks, and 279 of its flight-list rows had no track. Its numbers for the new rule were therefore wrong; those in section 6.3 replace them. The scorer now stops if any row has no track.

**A descending new edge can link.** Section 6.2: the three cases that kept it off were estimated truth times, and on corrected labels it meets the limits on its own. `link_new_edge_descent=False` reproduces the version-4 rule.

**Truth events outside Europe no longer count.** Network Manager's take-off and landing times at aerodromes outside the European box are estimates. The labelling that scores every linked hole now treats such an event as no evidence; a hole with nothing else reads unknown. Over the two days {{ relabelled_all }} linked holes had such an event: the three descent cases and {{ relabelled_other }} of the three false joins version 4 reported on cruise edges, which were the same kind of estimate.

**Audit of the rule.** The independent audit found the linking decisions correct and found one real defect, in the callsign guard the rule shares with the previous rule: a callsign is a *flicker* (one or two garbled samples that revert) only relative to the callsign seen before, and the guard remembered that earlier callsign without limit. One day's run can still hold a callsign the next day's run starts after, and the two then judged a flicker after a hole differently, so a piece could be written twice or by neither day. The new rule now forgets a callsign not heard for longer than the longest hole it can link ({{ max_gap_min }} min), which every day's run holds. The previous rule, selectable by name, keeps its published behaviour, where the same disagreement needs three hours without a callsign and is rare. Three smaller items: samples inside one second are now taken in time order (only possible with ADS-C merged); a `NaN` coordinate is read past like a blank one; and two expressions were simplified without changing a decision.

## 8. What changes for users of the data

- **`track_id` changes for everything produced from now on.** A flight that crossed a reception hole now has one id where it had two. This is the fourth such boundary (after the moves to A8 on 2026-08-27, to A9, and this one); nothing in the published tables says which rule produced a row, so anyone comparing across the boundary has to be told. A join across it returns nothing rather than an error.
- **The flight list has fewer rows and more of them carry both airports.** Rows per crossing flight fall from {{ s2_rows_base }} to {{ s2_rows_new }}.
- **A track can hold a hole.** Consumers who compute along a track, as the detectors did, should not interpolate across a silence of more than 30 minutes. The `cumulative_distance_nm` measurement adds the straight line across one.
- **`distance_v0.0.3`** is the version string of the corrected distance; `distance_v0.0.2` is the released one, reproducible under the legacy configuration.
- **The previous rule is still there.** `config.segmentation.method = "debounced"` gives the previous ids (A9); `"recommended"` gives A8; `"legacy"` the rule before 2026-08-27.
- **Each day's run takes longer.** It reads {{ read_back_hours }} hours of the previous day instead of 3, and the segmentation step reads up to 60 samples around every sample. On the pipeline day, steps 02 to 04b took {{ time_new }} under the new rule against {{ time_base }} under the previous one.

## 9. Limits

- About {{ unknown_share }} of linked holes cannot be checked, because the truth covers Europe only. The false-join rate is measured on the rest.
- The climbing-edge allowance rests on {{ climb_labelled }} checkable holes, fewer than wanted. {% if descent_adopted %}The descending-edge allowance rests on {{ descent_kept_checkable }}.{% else %}An aircraft reappearing on descent stays split: {{ descent_false }} false joins of {{ descent_labelled }} checkable holes without a condition, and no condition tried qualified (section 6.2).{% endif %}
- The callsign check reads 60 minutes back, longer than the 30 minutes that start a new track, so it can read a callsign from the previous flight of the same aircraft and split a hole that should link. It errs towards splitting and both days' runs agree on it.
- A decision in the last ten minutes of a day's read, or a hole straddling its end, sees less than the next day's read does. The run warns about tracks still alive there; under the new rule the warning fires more often and names tracks that may simply be long.
- Two days of truth. Both are in June.

## 10. Reproducing this report

Every number above comes from a CSV under `data/`, produced by exactly one job in `benchmarks/regenerate_track_v5.py` with the arguments it declares, and stamped with the code it was produced by (`data/*/_manifest.json`: {{ manifest_shas }}). `python benchmarks/regenerate_track_v5.py --check` says what is stale; without `--check` it re-runs only that, on the cluster. The pipeline day needs `benchmarks/run_track_link_day.py` to have built both arms first. The prose is filled from the CSVs by `benchmarks/build_track_v5_paper.py`, which refuses to build with a number missing.
