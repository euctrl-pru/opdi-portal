**Superseded by [the next version](../track-construction-v5/index.qmd).** This version proposed the rule and measured it, and recommended against making it the default until two detectors were fixed. The next version fixes them, audits the rule, re-measures on the final code and makes it the default. The numbers below are those this version published; its pipeline-day figures for the new rule were later found to rest on a stale candidate table, and the next version replaces them. This page is frozen: rendering it no longer re-runs the analysis.

## What we found

Today a reception gap of over 30 minutes ends a track. A flight crossing the Atlantic therefore becomes two tracks and two rows in the flight list, and neither row has both airports. The new rule keeps one `track_id` across such a gap when the evidence at its two edges fits one continuous flight.

- **Flights crossing the European box in one track: {{ crossing_base }} under the current rule, {{ crossing_new }} under the new one** ({{ crossing_flights }} flights over two days). "Crossing" means exactly one of the two airports lies inside the box.
- All flights in one track: {{ all_base }} to {{ all_new }}.
- **Two things took crossing flights from {{ early_crossing }} to {{ crossing_new }}.** Reading altitude, vertical rate and speed at a gap's edge past blank samples gives {{ off_crossing }}. Letting a flight lost while still climbing link gives {{ crossing_new }}. A flight that reappears descending stays split, because that case failed its own limit.
- **Wrong joins are rare where we can check them:** {{ false_joins }} of {{ labelled_holes }} checkable linked gaps ({{ false_join_rate }}), and {{ limit_joined }} of {{ limit_controls }} artificial gaps hiding a real landing. But {{ unknown_share }} of linked gaps cannot be checked at all, because our truth covers Europe only.
- **The climb part is provisional.** Gaps linked only because the flight was climbing show {{ climb_false }} false joins in {{ climb_labelled }} checkable cases, under the 300 we wanted. All {{ climb_hidden }} artificial gaps that joined have a climbing edge and the same or a blank callsign.
- In a real pipeline run on one airframe in ten, crossing flights with both airports right on one row went from {{ s2_both_base }} to {{ s2_both_new }}, of {{ s2_crossing }}.

**We do not recommend making it the default yet.** Two things computed along a track break across a gap, and neither is fixed. A level segment spans a gap on {{ level_share }} of the linked tracks checked ({{ level_tracks }} of {{ level_scored }}). Cumulative distance comes up short across a gap, by a median of {{ dist_median }} NM. Switching would also create another silent `track_id` boundary. The rule is in the code, and it is not the default. Its settings are provisional for climbing edges.

## The problem

Take a flight from New York to Frankfurt. Receivers hear it leave New York, lose it over the ocean, and hear it again near Ireland hours later. The current rule sees a gap over 30 minutes and starts a new track. The flight list then says this (an illustration, not a record from the data):

| Rule and row | Track | ADEP | ADES |
|---|---|---|---|
| Current rule, first row | first piece | KJFK | none |
| Current rule, second row | second piece | none | EDDF |
| New rule, the only row | one track | KJFK | EDDF |

Anyone asking how many flights went from New York to Frankfurt counts none under the current rule. Over the two days studied, the {{ crossing_flights }} crossing flights made {{ crossing_base_tracks }} tracks, or {{ crossing_rows_base }} per flight. In the pipeline sample, {{ s2_both_base }} of {{ s2_crossing }} crossing flights had both airports right on one row.

Below, a **hole** is a gap over 30 minutes, and its **edges** are the samples either side of it.

## The rule in plain words

A hole stays inside one track only if all of these hold. If a value needed for a check is missing, the check fails and the hole splits, except where the table says otherwise.

| Check | Setting | Why |
|---|---|---|
| Hole not too long | at most {{ max_gap_min }} min | Longer holes leave too much room for anything to have happened. |
| At cruise or climbing before the hole | lowest of the nearest {{ edge_samples }} altitudes within {{ edge_position_min }} min before the hole at or above {{ floor_ft }} ft; or, below that, climbing: the mean of the nearest {{ edge_samples }} vertical rates above {{ vert_rate_fpm }} ft/min, resting on at least {{ min_rates }} rates | An aircraft at altitude is not about to land, and one still climbing has just left. A blank rate does not count as climbing. |
| At cruise after the hole | lowest of the nearest {{ edge_samples }} altitudes within {{ edge_position_min }} min after the hole at or above {{ floor_ft }} ft | The same at the other side. A descending aircraft below the floor does not pass: see Results. |
| No descent then climb | not both descending faster than {{ vert_rate_fpm }} ft/min before and climbing faster than that after; a blank vertical rate does not count against it | That is what a stop in the hole looks like. |
| Callsign agrees | the last callsign before (at most {{ lookback_min }} min old) and the first after (within {{ lookahead_min }} min) are not both present and different; blank counts as agreeing | A different callsign is a different flight. A blank says nothing. |
| Implied speed not too slow | at least {{ min_speed_kt }} kt: straight-line distance between the edge positions over the time between them | A stop uses up time, which lowers this speed. |
| Implied speed not impossible | at most {{ max_speed_kt }} kt | Faster than this is two aircraft sharing an address, not wind. |
| Real address | `icao24` not `000000`, `000001` or `ffffff` | These placeholders are shared by many aircraft. |

**Every edge value is read past blank samples, within {{ edge_position_min }} min of the hole.** ADS-B sends position and altitude separately from speed and vertical rate, so the sample right at an edge often lacks one or the other. So position uses the last value within {{ edge_position_min }} min before the hole and the first within {{ edge_position_min }} min after it. Altitude, vertical rate and speed each use their own nearest {{ edge_samples }} values within the same {{ edge_position_min }} min. With no altitude in that span the check fails. With no vertical rate, the descent-then-climb check passes, and the climbing alternative does not.

Everything else stays as it is. A callsign change and a short gap at low altitude still split. No sample is invented: the hole stays empty and only the identity changes. In the code this is `cruise_linked`, also called A10: the current rule, A9 `debounced`, with this one change. A day's run must read {{ read_back_hours }} hours back rather than 3, or a track across midnight would publish samples twice, and the code enforces that.

**What others do.** Nobody publishes a tested method, as far as our literature review, "Link flight pieces only where stops cannot fit", could find. Public practice splits on a time gap of 10 to 30 minutes and mostly stops there. OpenSky's 2021 dataset paper rejoins pieces when a straight extrapolation fits within 0.5 degrees, and GAIA publishes rules for splitting; neither reports how often it links wrongly. We found no study that tests linking against real landings hidden in withheld data, which may be a gap in our search rather than proof that none exists. So this study measures its own error rate.

## How we tested it

A **false join** is two different flights of one airframe sharing a track. A landing and a take-off vanish, the flight list pairs an airport from one flight with an airport from another, and nothing in the data shows it. A split, by contrast, is visible and leaves two truthful pieces. So we treat a false join as worse, and unclear evidence means split. That weighting is a project decision, not a result.

The days are {{ day_new }} and {{ day_old }}, the only worldwide full days with the tables needed. Truth is Network Manager, the European flight data.

| Test | What it does | What should happen | Size |
|---|---|---|---|
| Real holes | Each linked hole against Network Manager, matched on airframe and time and never on callsign, since that would score the callsign check against itself. True link: the hole lies inside one flight. False join: a landing or take-off of that airframe falls inside the hole. Otherwise unknown. | No false joins | {{ linked_holes }} linked, of {{ real_holes }} holes |
| Hidden landing | Two consecutive flights of one airframe. Delete from a cruise point in the first to a cruise point in the second, so a real landing sits inside an artificial hole. A second kind starts the cut at the first flight's last climbing sample below the floor. The cruise side of the cuts that count sits 30 min or more into cruise, where speed alone cannot give it away. | No joins | {{ limit_controls }}: {{ cruise_hidden_tests }} between cruise edges, {{ climb_hidden_tests }} from a climbing edge |
| Cut flight | Delete the middle of one continuous flight, for {{ gap_60 }} to {{ gap_360 }} min. | Rejoined | {{ positive_tests }} |
| Real pipeline day | {{ day_new }}, one airframe in ten, through the real pipeline under both rules, to the flight list and events. | More flights right, nothing else moved | {{ s2_crossing }} crossing flights |

A setting had to meet three limits, pooled over the two days: false joins at most 0.1% of checkable real holes; at most 1% of hidden landings joined, read as a 95% upper bound; and at most 5% where the two flights have the same or a blank callsign, the case the callsign check cannot reject. Among settings within those limits, we took the most crossing flights in one track.

An edge allowed below the floor also has to pass the same three on its own, so a good result elsewhere cannot hide a bad one: at most 1% of the hidden landings of that kind, at most 5% of those with the same or a blank callsign, and at most 0.1% false joins among checkable holes linked only that way. A rate over a limit fails however small the sample. A rate within it, on under 300 checkable holes, passes only provisionally.

**Climbing and descending are separate settings because of these same two days.** We split them after a read of the same data showed one clean and one not. So the limits confirm a pattern already seen in that data. They are not a test of a prediction.

## Results

{{ table_one_track }}

The counts include {{ flights_no_track }} flights with no track at all, which read as not in one track. "In one track" says the flight is not split. It does not say the track holds nothing else: the false-join tests below answer that.

**False joins.**

| Evidence | Result | Reading |
|---|---|---|
| Real holes that can be checked | {{ false_joins }} false joins of {{ labelled_holes }} ({{ false_join_rate }}) | Within the 0.1% limit. {{ false_joins_day_old }} on {{ day_old }}, {{ false_joins_day_new }} on {{ day_new }}; {{ false_joins_near_edge }} had the event within {{ near_edge_min }} min of an edge, where a timing error could explain it. |
| Real holes that cannot be checked | {{ unknown_holes }} of {{ linked_holes }} ({{ unknown_share }}) | No evidence either way. |
| Real holes linked only because the old edge was climbing | {{ climb_false }} false joins of {{ climb_labelled }} checkable, among {{ climb_linked }} linked | Under the 300 wanted, so provisional. Zero of {{ climb_labelled }} still allows up to {{ climb_upper }} at 95% confidence, well above the 0.1% limit, and {{ climb_unknown_share }} of these holes cannot be checked. |
| Hidden landing, cuts deep in cruise | {{ limit_joined }} of {{ limit_controls }} joined, from {{ hidden_pairs }} flight pairs | At most {{ limit_upper }}, at 95% confidence. |
| The same, same or blank callsign only | {{ same_joined }} of {{ same_controls }} joined | At most {{ same_upper }}. Every join is in this group. |
| The same, callsign check removed | {{ no_callsign_joined }} of {{ limit_controls }} joined | At most {{ no_callsign_upper }}. So the safety does not rest on the callsign check alone. |
| Hidden landing, shallow cuts | {{ shallow_joined }} of {{ shallow_controls }} joined | Easy cases, near an airport. Reported, not counted toward the limit. |

**Hidden landings by kind of edge.** The chosen rule allows the first two kinds. We also ran {{ low_edge_tests }} tests of the last two. With the chosen settings a descending edge fails the altitude check by construction, so those are not counted above, and they appear here as what turning descent on would face.

{{ table_hidden_families }}

**Climbing and descending edges, decided separately.** All four combinations, at the chosen settings otherwise.

{{ table_edges }}

**These two results point in opposite directions, and each could be chance. They do not show that the two kinds of edge differ.** On real holes, climbing has {{ climb_false }} false joins of {{ climb_labelled }} and descending {{ descent_false }} of {{ descent_labelled }} ({{ descent_rate }}). If those {{ descent_false }} fell at random among the {{ low_labelled }} checkable holes, the chance of all of them landing on the descending side is {{ chance_real }}. On hidden landings it is the reverse: {{ climb_hidden }} of {{ climb_hidden_tests }} joined with a climbing edge and {{ descent_hidden }} of {{ descent_hidden_tests }} with a descending one. If those {{ climb_hidden }} fell at random among the {{ low_hidden_tests }} tests, the chance of all of them landing on the climbing side is {{ chance_hidden }}. Timing may also play a part: {{ descent_near_edge }} of the {{ descent_false }} descent false joins have the landing or take-off within {{ near_edge_min }} min of an edge, where an error in the truth time could produce a false join by itself. In the chosen rule, {{ false_joins_near_edge }} of the {{ false_joins }} are that close. We have not checked those cases one by one.

**Climbing on is a judgement.** It gains {{ climb_points }} points of crossing flights and adds a way for a hidden landing to get through. At this sample size, "within the limit" means only that no false join was observed: {{ climb_false }} of {{ climb_labelled }} still allows up to {{ climb_upper }}, at 95% confidence.

**Descending off applies the limit. It has not been shown worse.** It rests on {{ descent_false }} of {{ descent_labelled }}. With so few checkable holes, a single false join would already read as {{ descent_one_rate }}, over the limit, so one or two cases decide it. Turning it on as well would give {{ both_crossing }} of crossing flights, {{ both_extra_flights }} flights more, with {{ both_false }} false joins of {{ both_labelled }} ({{ both_rate }}) overall.

**How settings trade off.** All of these have the speed ratio and time-for-a-stop checks off, which the Choices section covers. The chosen setting is the first row. All of them have climbing edges on, so "yes" here carries the same provisional status.

{{ table_settings }}

Hidden-landing counts differ because only holes the setting allows are counted. The longest hole allowed matters for coverage, and also for hidden landings: at {{ min_speed_kt }} kt they go from {{ limit_joined }} joined at {{ max_gap_min }} min to {{ hidden_at_480 }} at {{ s315_480_gap }} min and {{ hidden_at_360 }} at {{ gap_short_min }} min. The speed floor hardly matters on real flights, but it matters for false joins and for cut flights.

**What keeps the remaining holes split.** These are holes inside the flights scored. On crossing flights, {{ crossing_holes_linked }} are linked and {{ crossing_holes_split }} stay split. Each is listed under the first check it fails, in the order the code applies them.

{{ table_reasons }}

No one cause dominates on crossing flights. {{ new_edge_holes }} holes ({{ new_edge_share }} of those left split) fail below the floor after the hole, {{ speed_holes }} ({{ speed_share }}) on implied speed, and {{ old_edge_holes }} ({{ old_edge_share }}) below the floor before it. Most of them would fail another check too: nothing else fails for only {{ new_edge_only }} of the {{ new_edge_holes }} and {{ speed_only }} of the {{ speed_holes }}. Length is not the problem: {{ too_long_holes }} holes are too long.

"Below the floor" here also includes an edge with no altitude at all within {{ edge_position_min }} min, and these files do not separate the two. **This corrects the earlier version of this report,** which read this group as aircraft reappearing on approach. That was a guess and it was wrong: most of that group then were simply blank altitudes, now read past. Turning on descending edges adds only {{ both_extra_flights }} crossing flights in one track.

## How the method got here

Each row explains a final choice. The comparisons come from earlier runs of the study, except the last row, and the second figure in each comparison of the row before it, which are final.

| What we found | What it did | What we chose |
|---|---|---|
| A position is often blank right at a hole's edge, and the first version split those holes because it could not compute a speed. In the final data, {{ blank_edge_holes }} of all {{ real_holes }} holes ({{ blank_edge_share }}) use a position read past a blank. | At {{ floor_high_ft }} ft, {{ min_speed_kt }} kt and {{ gap_short_min }} min, crossing flights in one track went from {{ before_blank_crossing }} to {{ after_blank_crossing }}, and linked holes from {{ before_blank_linked }} to {{ after_blank_linked }}. | Read positions past blank samples. |
| All flights moved little ({{ all_base }} to {{ before_blank_all }}) because short European flights dominate, while crossing flights went only from {{ crossing_base }} to {{ before_blank_crossing }}. | The headline hid the flights this is for. | Choose settings by crossing flights first. |
| Lower floors and longer holes let more through, but floors made little difference. | Before altitude was read past blanks, at {{ min_speed_kt }} kt and {{ max_gap_min }} min: {{ high_floor_long_crossing }} at {{ floor_high_ft }} ft, {{ early_crossing }} at {{ floor_ft }} ft, {{ low_floor_crossing }} at {{ floor_low_ft }} ft. | {{ floor_ft }} ft and {{ max_gap_min }} min. See Choices below. |
| Altitude is often blank right after a hole, as position is, and a blank altitude failed the check at any floor. | Reading altitude, vertical rate and speed past blanks, at the same settings: crossing flights {{ early_crossing }} to {{ off_crossing }}, linked holes {{ early_linked }} to {{ off_linked }}, false joins {{ early_false }} of {{ early_labelled }} to {{ off_false }} of {{ off_labelled }}. | Read them past blanks. We treat it as a bug fix, and it stayed within the limits. Its cost is under Limits of the evidence. |
| An edge below the floor may still fit one flight if its direction does: an old edge climbing or a new edge descending. | Climbing: crossing flights {{ off_crossing }} to {{ crossing_new }}, {{ climb_points }} points, with {{ climb_false }} false joins of {{ climb_labelled }} checkable and {{ climb_same_joined }} of {{ climb_same_tests }} hard hidden landings joined. Descending: {{ off_crossing }} to {{ descent_crossing }}, {{ descent_points }} points, with {{ descent_false }} of {{ descent_labelled }}. | Climbing on, provisional, since {{ climb_labelled }} is under 300. Descending off, since {{ descent_rate }} is over its 0.1% limit. Each is one setting. |

Two counts say how often a position is read past a blank, and they answer different questions. {{ blank_edge_holes }} of all {{ real_holes }} holes ({{ blank_edge_share }}) is how common it is among holes, whatever the rule decides, at every length and altitude. {{ final_blank_linked }} of {{ linked_holes }} linked holes ({{ final_blank_share }}) is how often a link rests on such a position, among the holes the chosen settings keep. Both count a hole when either edge reads past a blank, and neither counts a hole with no position within {{ edge_position_min }} min, which cannot link. Of the {{ false_joins }} false joins, {{ false_joins_blank_edge }} used such a position. Altitude is counted in Limits of the evidence below.

## Real pipeline day

{{ day_new }}, one airframe in ten, flights that take off and land on the day.

{{ table_flight_list }}

Crossing flights on one row went from {{ s2_one_row_base_share }} to {{ s2_one_row_new_share }}. Both airports right on one row went from {{ s2_both_base }} to {{ s2_both_new }}, which is {{ s2_both_share }} of them. The rule finds no new airports: across every row of the comparison, {{ s2_none_to_right }} answers went from none to right. It puts answers on one row that were already on two. The ceiling is detection: {{ s2_far_none }} of {{ s2_crossing }} crossing flights ({{ s2_far_none_share }}) have no row naming the airport outside the box, matching flights by callsign. On crossing flights, a destination went from right to wrong on {{ cross_ades_right_to_wrong }} and from wrong to right on {{ cross_ades_wrong_to_right }}. {{ s2_more_rows }} crossing flight gained a row, which we did not look into.

**A small loss inside Europe.** Of {{ intra_n }} flights inside Europe, {{ intra_gained }} gained both airports right and {{ intra_lost }} lost them. Destination went from right to wrong on {{ intra_ades_right_to_wrong }} and from wrong to right on {{ intra_ades_wrong_to_right }}. We do not know why, as these CSVs cannot say. Those {{ intra_lost }} flights should be looked at one by one before any promotion.

**Track boundaries moved only where they should.** Re-segmenting the sample under both rules, over the whole window a day's run reads, gives {{ boundaries_base }} track boundaries under the current rule and {{ boundaries_new }} under the new one. All {{ boundaries_removed }} removed sit at holes the rule links ({{ boundaries_at_holes }} there, {{ boundaries_elsewhere }} elsewhere), and {{ boundaries_added }} were added. The pipeline's {{ arm_tracks }} tracks for the day are exactly those re-segmented tracks. Of {{ moves_total }} airport take-offs and landings in APDF, the detector finds {{ moves_base }} under the current rule and {{ moves_new }} under the new one.

**Events that changed**, on airframes with a linked track, inside the flights scored:

{{ table_events }}

Most of this is arithmetic. `first_seen`, `last_seen`, `top-of-climb` and `top-of-descent` each fall by one per joined hole, {{ scored_holes }} of them inside the flights scored, out of {{ linked_anywhere }} linked holes in the sample. The level events fall by {{ level_holes }}, one per hole inside a level segment, covered in the next section. `level-off-descent-start` and `level-off-descent-end` change by {{ ev_level_off_start }} and {{ ev_level_off_end }}. `xing-fl245` changes by {{ ev_fl245 }}, `xing-fl100` by {{ ev_fl100 }} and `xing-fl50` by {{ ev_fl50 }}. Our reading, not measured, is that when one edge sits below a level and the other above, the hole itself reads as a crossing: the floor allows that at FL245, and a climbing old edge, which can be at any altitude, allows it lower down.

**Airport surface events are not arithmetic, and we have not traced them.** `entry-taxiway` changes by {{ ev_entry_taxiway }}, `exit-taxiway` by {{ ev_exit_taxiway }}, `entry-apron` by {{ ev_entry_apron }}, `exit-apron` by {{ ev_exit_apron }}, `entry-parking_position` by {{ ev_entry_parking }}, `exit-parking_position` by {{ ev_exit_parking }} and `runway-vacated` by {{ ev_vacated }}. These events are only matched at airports named on the track's flight-list row, so a row whose airports change can lose them. That is our reading from the code, not a measurement. They should be looked at alongside the {{ intra_lost }} flights above.

**`AOBT` and `AIBT` change by {{ ev_aobt }} and {{ ev_aibt }}. This is not evidence of glued flights.** The block detector gives a track at most one of each, the first off-block and the last on-block, so two pieces that each had one now have one between them. That is the detector's logic; we did not trace each of these individually. Could an on-block on the older piece and an off-block on the newer one mean the aircraft parked in the hole? To check, all {{ truth_holes }} linked holes in the sample were labelled against every Network Manager flight of the airframe, whatever its day. The labels use Network Manager times only. Detected events are counted beside them and never decide a label.

{{ table_truth }}

{{ truth_false }} holes are false joins. Such events on both sides of a hole occur on checked true links too ({{ truth_true_both }} of {{ truth_true }}), so block events there are not proof of a stop. The event columns are lower bounds: a later piece that starts after midnight is absent because the next day was not built, and an event is counted anywhere on a piece rather than near the hole, so they support nothing beyond that sentence. The {{ truth_unknown }} unknown and {{ truth_out }} not-in-truth holes stay unchecked, the same limit as in the two-day figures.

## What this breaks, and is not fixed

**Level segments span holes.** On {{ level_tracks }} of {{ level_scored }} linked tracks inside the flights scored ({{ level_share }}), a level start before a hole pairs with a level end after it: {{ level_segments }} segments containing {{ level_holes }} holes. A user computing level time, or how much of a flight is flown level, gets hours of "level flight" during which nothing was received. We set the condition before measuring that damage on most linked tracks withdraws the recommendation, and this meets it.

**Cumulative distance is short across holes.** On {{ dist_off }} of the {{ dist_compared }} holes compared, the distance added differs from the straight-line distance between the edges by over 1 NM. The median shortfall is {{ dist_median }} NM, and 95% of holes are short by at least {{ dist_p95 }} NM. All {{ dist_masked }} have a blank position on a sample next to the hole, and the distance function reads a blank as zero distance, so the hole adds next to nothing. The stored measurement agrees with this calculation on {{ dist_tracks }} tracks checked, with {{ dist_tracks_off }} off, so this is what would be published. A user reading distance flown from a linked track, or comparing it with a route, sees a long-haul flight far too short, with no warning.

The detectors were not changed in this work, and any fix gets its own `version`.

## Limits of the evidence

- **Truth is European, and most linked holes cannot be checked.** {{ unknown_share }} are unknown. A stop outside Network Manager's area is invisible, so how the rule performs elsewhere is unmeasured.
- **Two days.** {{ false_joins }} false joins, {{ false_joins_day_new }} on one day and {{ false_joins_day_old }} on the other, are too few to say anything about variation.
- **The two-day measurements re-segment existing tracks, not raw data.** They take `opdi-world/osn_tracks` as given. Only the pipeline day starts from the state vectors.
- **The pipeline day is a 10% sample with few crossing flights,** {{ s2_crossing }} of them. Its one-row share under the current rule is {{ s2_one_row_base_share }}, against {{ crossing_base }} in the two-day data, which shows how far a sample this size can sit from the larger figure.
- **Flights inside one day only.** Eastbound crossings leaving before midnight UTC drop out, so the sample leans westbound. A flight whose first part belongs to a track of the day before reads as fewer tracks than it has.
- **A flight that never reaches the floor is untested.** It cannot link, and no test covers it.
- **Climb evidence is thin.** {{ climb_labelled }} checkable holes, under the 300 wanted, and {{ climb_unknown_share }} of holes linked this way cannot be checked. The {{ climb_hidden }} hidden landings it let through all fall in the {{ climb_same_tests }} tests with a climbing edge that the callsign check cannot reject. No cut-flight test measures climbing alone: the low-edge cut removes a flight's whole cruise, so it needs climbing and descending together, and rejoins {{ low_cut_chosen }} of {{ low_cut_tests }} under the chosen rule against {{ low_cut_both }} with both on.
- **The hidden-landing test does not cover a low climbing edge.** It cuts at a flight's last climbing sample below the floor, so just under {{ floor_ft }} ft, whereas the rule accepts an old edge at any altitude while it climbs. So its {{ climb_hidden }} of {{ climb_hidden_tests }} covers the high-climb case only, and a low climbing edge is covered only by the {{ climb_labelled }} real holes. A departure lost low that lands outside Network Manager's area reads as unknown, and most climbing links are unknown ({{ climb_unknown_share }}). There the {{ min_speed_kt }} kt speed test does most of the protecting. That is our reading, not a measurement.
- **A reading can be up to {{ edge_position_min }} min old.** An edge's altitude may come from a sample {{ edge_position_min }} min from the hole, so an aircraft could really be below the floor at the edge itself. {{ alt_new }} linked holes ({{ alt_new_share }}) read the new edge's altitude past a blank, with {{ alt_new_false }} false join among them, and {{ alt_old }} ({{ alt_old_share }}) read the old edge's, with {{ alt_old_false }}. These count every hole with no altitude in the edge's own second, not only those the fix newly lets through. We have no measure of how far the aircraft moved in those minutes.
- **Hard same-callsign cases are few.** There are {{ same_controls }}, so the bound is {{ same_upper }} overall and much weaker in places: the smallest group ({{ smallest_same_family }}, holes of {{ smallest_same_band }} min, cut {{ smallest_same_depth }} min into cruise) has {{ smallest_same_n }} tests and a bound of {{ smallest_same_upper }}. The {{ limit_controls }} tests come from at most {{ pairs }} flight pairs, and cuts of one pair are not independent, so the bounds are somewhat too tight.
- **Artificial holes are cleaner than real ones,** and cut flights lean to well-covered flights.
- **Cut flights are often not rejoined: {{ positive_missed_share }} at {{ min_speed_kt }} kt, against {{ s315_missed }} at {{ slow_speed_kt }} kt.** Implied speed is the first failed check on {{ positive_too_slow }} of the {{ positive_missed }} misses ({{ positive_too_slow_share }}). This barely shows on real flights: going from {{ slow_speed_kt }} to {{ min_speed_kt }} kt loses {{ tests_linked_drop }} of rejoined cut flights but {{ real_linked_drop }} of linked real holes. We can only partly explain it. Cut-flight tests are not split by flight type, and {{ other_flight_share }} of flights do not cross the box, so most tests are on shorter flights, where a cut can sit in slower flying or on a less direct path. Tests of {{ gap_360 }} min, which only long flights can host, miss {{ miss_360 }}, which fits. But misses first rise with length, {{ miss_60 }} at {{ gap_60 }} min, {{ miss_120 }} at {{ gap_120 }} and {{ miss_180 }} at {{ gap_180 }}, which does not. That explanation is ours and not measured, and the pattern remains unexplained.

## Choices made, and what they cost

These are judgement calls, and someone could reasonably go the other way.

| Choice | What it costs, and the alternative |
|---|---|
| **Real-hole limit tightened from 1% to 0.1%, after seeing results.** The first limit could not separate settings, and we want a wrong join to be rare. | Set after the fact, so it is not an independent test. At 1%, {{ slow_speed_kt }} kt and {{ max_gap_min }} min would pass, with {{ s315_600_crossing }} of crossing flights and {{ s315_600_false }} false joins of {{ s315_600_labelled }} ({{ s315_600_rate }}). |
| **Floor {{ floor_ft }} ft, chosen by judgement, not by the selection rule.** Applied across floors in the earlier runs, the rule would have picked {{ floor_low_ft }} ft. | **These floor figures predate reading altitude past blanks.** Lower floors barely helped then, and the reason was the same blanks: a blank altitude fails at every floor. In those runs, {{ floor_low_ft }} ft and {{ min_speed_kt }} kt stayed within the limits only with a speed ratio check of {{ ratio_value }}: {{ low_floor_ratio_crossing }} of crossing flights, {{ low_floor_ratio_false }} false joins of {{ low_floor_ratio_labelled }} ({{ low_floor_ratio_rate }}), against {{ early_crossing }} and {{ early_false }} of {{ early_labelled }} at {{ floor_ft }} ft. Without that check it gave {{ low_floor_crossing }} but {{ low_floor_false }} of {{ low_floor_labelled }} ({{ low_floor_rate }}), over the limit. We gave that up for half the false joins and, in our judgement, edges further from busy airspace near airports. {{ floor_low_ft }} ft has not been run again since the fix, so what it would give now is unknown. |
| **{{ min_speed_kt }} kt and {{ max_gap_min }} min.** The selection rule's pick. | {{ slow_speed_kt }} kt and {{ s315_480_gap }} min comes within {{ crossing_tie_gap }} flights, {{ s315_480_crossing_n }} crossing flights against {{ chosen_crossing_n }}, but with {{ s315_480_false }} false joins of {{ s315_480_labelled }} ({{ s315_480_rate }}) it is just over the limit, and it joins {{ s315_480_hidden }} hidden landings against {{ limit_joined }}. Within the limits the nearest is {{ slow_speed_kt }} kt and {{ max_gap_min }} min with a speed ratio check of {{ ratio_value }}: {{ ratio315_crossing_n }} crossing flights, {{ ratio315_false }} false joins, {{ ratio315_hidden }} hidden landings joined, and {{ ratio315_missed }} of cut flights missed against {{ positive_missed_share }}. |
| **Time-for-a-stop check off.** It asks whether the time left over at a fast ground speed is too short for a stop. A larger allowance is looser. | At {{ stop35_min }} min, crossing flights fall to {{ stop35_crossing }}, and at {{ stop20_min }} min to {{ stop20_crossing }}. The safety gain is real: false joins {{ false_joins }} to {{ stop35_false }} and hidden landings {{ limit_joined }} to {{ stop35_hidden }}. The coverage loss is far larger, which is why it stays off. |
| **Speed ratio check off.** It compares implied speed with the aircraft's own speed at the edge. | At {{ ratio_value }}, crossing flights {{ ratio_crossing }} and false joins {{ ratio_false }}, against {{ crossing_new }} and {{ false_joins }}. A small trade either way, and we kept the simpler rule. |
| **Climbing edges on, on evidence short of what we wanted.** | Gains {{ climb_points }} points of crossing flights, and adds a way for a hidden landing to get through: {{ climb_hidden }} of {{ climb_hidden_tests }}. Off gives {{ off_crossing }} and {{ cruise_hidden }} of {{ cruise_hidden_tests }} joined. |
| **Descending edges off.** | Gives up {{ both_crossing }} for {{ crossing_new }} of crossing flights. They fail their own limit, on a small sample. See Results. |
| **A blank callsign counted with same callsign** in the hard case. | {{ same_blank }} of {{ same_controls }} tests ({{ same_blank_share }}) are blank. The check cannot reject them, so this is the cautious way, but the group is not purely same-callsign flights. |
| **Any landing or take-off in a hole counts as a false join.** | Overstates false joins, if anything. |

## What is not decided

- **Making it the default.** Not recommended now. The rule ships off and `standard` still points to `debounced`.
- **A new silent `track_id` boundary.** Promotion would add one after those at A8 and A9. A join across it returns nothing rather than an error, and nothing in the published data says which rule produced a row, as `osn_tracks` has no version column. Consumers would have to be told.
- **The two fixes needed first.** Level detection must not pair a start and an end across a hole. Cumulative distance must carry across one. Both are detector changes with their own `version`, and neither is designed yet. After them, the event comparison should be run again.
- **Whether climbing edges stay on.** More checkable holes would firm up or remove the provisional status. Holes from more days are the way to get them.
- **Whether descending edges should be tried again** on a larger sample, once the timing of the near-edge cases is understood.
- **What a lower floor gives now.** {{ floor_low_ft }} ft has not been run with altitude read past blanks.
- **Whether to add an airport test,** asking if any airport the aircraft could have stopped at fits inside the hole. Not built here, because it needs a join outside the rule's current form. Our literature review calls it the most promising and least proven idea. Whether it is needed depends on the unknown holes above.

## Reproducing this

Run from the `opdi` repository. `run_track_link_day.py` and the regeneration need the cluster, S3 and Network Manager truth. The pipeline day's tables go under `s3a://eurocontrol/opdi-world/research/a10/`.

```
export PYSPARK_DRIVER_PYTHON=$PWD/.venv310/bin/python
unset PYSPARK_PYTHON
python benchmarks/run_track_link_day.py --day 2026-06-14 --arm debounced --steps 01 02 02a 03 04 04b
python benchmarks/run_track_link_day.py --day 2026-06-14 --arm cruise_linked --steps 02 02a 03 04 04b
python benchmarks/regenerate_track_v4.py --check     # what is stale; no cluster needed
python benchmarks/regenerate_track_v4.py --force     # final_grid and final_edges over two days, and the pipeline scoring
python benchmarks/build_track_v4_paper.py            # fills this report; no cluster needed
```

**Data.** All in `papers/track-construction-v4/data/`. `final_grid/` has the settings search and the headline, `final_edges/` the four combinations of climbing and descending edges at the chosen settings, `stage2/` the pipeline day. `round1`, `round2`, `round3_fl250`, `round3_fl180`, `round3_fl100` and `round4_fl180` are earlier runs, kept to explain the method. They came from earlier code and are not regenerated, and all of them predate reading altitude past blanks, so they are not comparable with the final figures except where this report says so.

**Provenance.** Each file's script, arguments and commit are in the `_manifest.json` beside it. The final files record commit {{ manifest_shas }}. They came from `regenerate_track_v4.py --force` on that commit, and `regenerate_track_v4.py --check` reports all three jobs as current. The pipeline day's temporary tables (state vectors and raw tracks under `research/a10`) were deleted afterwards to return bucket space, so re-scoring the pipeline day needs `run_track_link_day.py` run again first.

**Render.** `index.qmd` holds this text with literal numbers and the regeneration step, but has not been rendered, because Quarto and R are not available on the machine that built it. `index.html` and `track-construction-v4.pdf` are built from the same text and CSVs by `benchmarks/build_track_v4_paper.py` (the PDF with `--pdf`, using reportlab), not through Quarto.
