## 1. What this adds

OPDI publishes flight events: milestones with a position, an altitude and a time, plus
measurements attached to them. Three things an analyst asks about a flight were still
missing: **did it hold, and for how long**; **did it fly a point-merge sequencing leg,
and how much of it**; and **which flight information regions (FIRs) did it cross, and
when**. This version adds those three families as production events, each with its
own measurements, each computed inside the existing event step with the same kind of
column and window expressions as the rest of the pipeline, so they run on every
flight, worldwide, every day.

Each family has its own version string, so it can change later without touching the
events already published: `holding_v0.1.0`, `pms_v0.1.0` and `fir_v0.1.0`. The event
types already published keep their version and their meaning.

What shipped on by default: **{{ shipped_on }}**. Off by default, reachable with a
configuration flag: **{{ shipped_off }}**. Section 8 explains the decision for each.

## 2. Holding patterns

**What is detected.** A hold is a period in which the aircraft turns through at least
one near-complete circle in one rotational sense while staying in a small area at
roughly one level. Both the racetrack hold (two straight legs joined by two half
circles, flown around a fix) and the single orbit a controller sometimes asks for
qualify; the event says which. Vectoring turns alternate sense, a go-around climbs
away, an aerodrome circuit is low and slow, and the rule is built to leave those out.

**How.** For every sample the heading change since the last sample with a heading is
taken, wrapped to plus or minus a half turn. Over a sliding window of
{{ holding_window_s }} seconds (one circuit takes four to seven minutes) the rule asks
four things: the signed turn is at least {{ holding_net_turn }} degrees in magnitude;
nearly all of it is in one sense (a left-right-left vectoring sequence nets to little);
the first and last turning samples are within {{ holding_disp_nm }} NM of each other
while the aircraft has flown a long way; and the altitude spread is small. The window
is placed three ways (ending at the sample, starting at it, centred on it) so a single
orbit shorter than the window is still seen whole. Samples flagged by any placement form
an episode; an episode is kept when it lasts at least {{ holding_min_s }} seconds and
turns at least one near-complete circuit, and it is dropped when it sits next to a
go-around on the same flight. Nothing is computed across a reception hole.

**Events and measurements.** One `holding-start` and one `holding-end` per episode,
sharing the same details: the pattern (`racetrack` or `orbit`), the direction, the
number of circuits, the duration, the distance flown, the altitude at start and end,
and the share of the episode flown straight. Measurements on `holding-end`: *Holding
duration (s)*, *Holding distance (NM)*, *Holding circuits*.

**Where the numbers come from.** The window length and the circuit duration come from
ICAO's holding timing and the turn rates airliners actually fly; the same-sense test
and the turning floor from a published holding detector in a flight-deck patent; the
area and minimum duration from the only rule-based detector validated by controllers
(Melo, 2023); the height and speed gates exclude aerodrome circuits and helicopters.

## 3. Point merge

**What a point merge system is.** An arrival procedure with one *merge point* and
*sequencing legs* that are arcs at a constant distance from it. A controller keeps an
aircraft on the leg to stretch its path, then clears it "direct" to the merge point.
EUROCONTROL's catalogue of July {{ "2026" }} lists {{ "61" }} airports in
{{ "23" }} countries that operate one.

**Why a table of merge points is needed.** Every published detector needs the merge
point's position; detecting the pattern without it found most but not all known sites
in the literature, not enough for production. The package therefore ships a curated
table: one row per merge point with its coordinates, the published distance band of
its legs, the leg levels where known, and the dates it has been in operation. It
covers {{ pms_sites }} airports, {{ pms_sites_placed }} of them with placed
coordinates; the rest are listed as gaps so the limitation is visible. Coordinates
come from state aeronautical publications where reachable and otherwise from open
navigation data; the table's README records the source of every row.

**How.** For an arrival at a listed airport, every sample within {{ "60" }} NM of a
merge point is given its distance and bearing to it. A sample is *on a leg* when, over
a {{ "90" }}-second window, the distance stays within {{ pms_band_nm }} NM of
constant and inside the published band, the bearing from the merge point sweeps, and
the aircraft moves across the bearing rather than along it. A run of such samples is a
leg when it lasts at least {{ pms_min_leg_s }} seconds **and** is followed within ten
minutes by a straight run pointing at the merge point (track within
{{ pms_align_deg }} degrees of the bearing, distance falling) that comes within three
miles of it. Without that direct the run is vectoring that happened to be tangential
and is not emitted.

**Events and measurements.** `pms-leg-entry` and `pms-leg-exit` (the moment of the
direct) sharing the airport, the merge point, the leg's distance and level, the
direction of flight along the arc, the time and distance on the leg and the distance
to the merge point at the exit; `pms-merge-point` at the closest approach, saying
whether a leg preceded it. Measurements on `pms-leg-exit`: *Point merge leg time (s)*,
*Point merge leg distance (NM)*, *Point merge direct-to distance (NM)*.

## 4. FIR crossings

**What is detected.** A change of FIR volume along a track. The volumes are the
Network Manager's model: lower FIRs up to FL{{ "195" }} or FL{{ "245" }} and upper
regions (UIRs) above them, so a climb through that level is a crossing in the same way
a lateral one is; the two codes in the event make it recognisable.

**The definition and its cycle.** FIR boundaries are published per AIRAC cycle. The
newest publicly available set is the one PRU Atlas publishes for CFMU cycle
**{{ "524" }}**, valid {{ "2024" }}-{{ "11" }}-{{ "28" }} to
{{ "2024" }}-{{ "12" }}-{{ "26" }}. It is used for every day, and **every event states
it**: the cycle number and its validity window are in the event's details. Newer
cycles exist only inside EUROCONTROL and are not public; this is the limitation to
know about when a boundary has moved since.

**How.** Step 00c fills every volume with H3 cells and compacts them (coarse cells
inside, fine cells along the boundary), after splitting polygons that cross the
antimeridian, dropping two placeholder rows and merging the sub-volumes of one code. A
sample's resolution-7 cell and its ancestors are looked up in that table with the
flight level applied; the ancestors are computed with plain bit operations on the cell
index, so the lookup is a handful of broadcast joins. A run of samples in one volume
shorter than {{ fir_dwell_s }} seconds is absorbed into the run before it, so a flight
skimming a boundary does not emit a crossing per cell flip. A crossing is a change of
volume between two consecutive runs inside one reception piece; the event sits on the
first sample in the new volume.

**Events and measurements.** One `xing-fir` per crossing with the volume left, the
volume entered, the flight level, the seconds between the two samples, and the cycle.
The two standard measurements (distance flown and time passed) are what distance and
time per FIR are computed from by differencing consecutive crossings.

## 5. Holding against hand-corrected labels

The open label set of Olive and others marks every {{ "6" }}-minute window, stepped
{{ "2" }} minutes, of two months of arrivals at four airports as holding or not, every
window corrected by hand. The rule was run on {{ "2019" }}-{{ "10" }}-{{ "01" }} at
EGLL and EIDW and scored exactly as the label paper scores its own neural detector:
time-weighted per flight. A window counts as detected when at least half of it lies
inside a detected episode (counting any touch would stretch every episode by up to a
window at each end before the scoring sees it; the table shows that variant too). The
bar is the neural detector trained on these labels: precision {{ "0.84" }}, recall
{{ "0.70" }}, F1 {{ "0.76" }} across airports.

| Airport | Flights scored | Positive windows | Episodes found | Precision | Recall | F1 | IoU | Precision (any touch) | F1 (any touch) |
|---|---|---|---|---|---|---|---|---|---|
| EGLL | {{ h_egll_flights }} | {{ h_egll_positive }} | {{ h_egll_episodes }} | {{ h_egll_p }} | {{ h_egll_r }} | {{ h_egll_f1 }} | {{ h_egll_iou }} | {{ h_egll_p_any }} | {{ h_egll_f1_any }} |
| EIDW | {{ h_eidw_flights }} | {{ h_eidw_positive }} | {{ h_eidw_episodes }} | {{ h_eidw_p }} | {{ h_eidw_r }} | {{ h_eidw_f1 }} | {{ h_eidw_iou }} | {{ h_eidw_p_any }} | {{ h_eidw_f1_any }} |
| Both | {{ h_all_flights }} | {{ h_all_positive }} | {{ h_all_episodes }} | {{ h_all_p }} | {{ h_all_r }} | {{ h_all_f1 }} | {{ h_all_iou }} | {{ h_all_p_any }} | {{ h_all_f1_any }} |

The detected episodes have a median length of {{ h_episode_median_min }} minutes;
{{ h_racetracks }} are racetracks and {{ h_orbits }} single orbits. The three
load-bearing thresholds were swept over {{ h_sweep_points }} points per airport; the
best point per airport is below, and the largest gain over the shipped setting is
{{ h_sweep_gain_max }} in F1, which is why the shipped thresholds were kept.

{{ h_sweep_table }}

## 6. Point merge on the pipeline day

Step 04 was run with the three families on over the whole of
{{ "2026" }}-{{ "06" }}-{{ "14" }}, worldwide. Of the {{ pm_airports_covered }} airports
in the table, {{ pm_airports_with_arrivals }} had arrivals that day: {{ pm_arrivals }}
arrivals, {{ pm_arrivals_with_leg }} of them with at least one sequencing leg
({{ pm_legs }} legs in all). The busiest fifteen:

{{ pm_usage_table }}

Dublin, the reference case in the literature, has {{ pm_eidw_with_leg }} of
{{ pm_eidw_arrivals }} arrivals on a leg ({{ pm_eidw_share }}). The published usage
there is about {{ "42" }}% in a busy month, but that figure counts arrivals that
*enter* the procedure, most of which are then cleared direct without flying a leg
(at Oslo, four in five of them in the one study that measured it), so it is not the
same quantity and is not used as a gate here. Oslo {{ pm_engm_share }}, London City
{{ pm_eglc_share }}, Lisbon {{ pm_lppt_share }}, Istanbul {{ pm_ltfm_share }}. The
Norwegian sites gave no legs at all although most of their arrivals pass the merge
point; whether the rule misses their legs or none were flown that day is not known.

**Looked at by eye.** {{ pm_check_detected }} detected and {{ pm_check_undetected }}
undetected Dublin arrivals were plotted with the merge points and inspected:
{{ pm_check_detected_real }} of the detected ones show a real sequencing leg, and
{{ pm_check_undetected_missed }} of the undetected ones show a leg the rule missed.

## 7. FIR crossings on the pipeline day

Over the day, {{ fir_tracks_crossing }} of {{ fir_tracks_day }} tracks crossed at least
one boundary, {{ fir_crossings_day }} crossings in all: a median of
{{ fir_per_track_p50 }} and a ninetieth percentile of {{ fir_per_track_p90 }} per
crossing track, the most {{ fir_per_track_max }}. {{ fir_vertical }} crossings are
vertical (the same four-letter region, lower to upper or back). The median time between
the last sample in the old volume and the first in the new is {{ fir_bracket_p50 }}
seconds. The most crossed boundaries:

{{ fir_pairs_table }}

**The lookup against the exact polygons.** For {{ fir_sample_tracks }} crossing
tracks ({{ fir_samples }} samples) the H3 volume was compared with exact
point-in-polygon membership in the same polygons. The two agree on
{{ fir_agree_pct }} of samples ({{ fir_agree_both_pct }} where both name a volume);
{{ fir_h3_null_pct }} of samples have no volume in the lookup against
{{ fir_exact_null_pct }} exactly. Of {{ fir_h3_crossings_sample }} H3 crossings in the
sample, {{ fir_matched_sample }} match an exact crossing of the same pair; the timing
error between the two is {{ fir_timing_p50 }} seconds at the median,
{{ fir_timing_p90 }} at the ninetieth percentile, {{ fir_timing_max }} at most (a
track that crossed the same boundary twice, matched to the wrong one of the two).
{{ fir_double_match_dropped }} sample rows matched two volumes and were resolved by the
narrower band. All events carry cycle {{ fir_airac }}.

No comparison with the Network Manager's own crossing records was possible here: that
truth lives in a database reachable only from the office network. The extractor and
the scorer are written and tested, so the comparison is one run away.

## 8. What shipped, and the cost

Holding ships **{{ holding_default }}** by default, point merge **{{ pms_default }}**,
FIR crossings **{{ fir_default }}**. The gates were set before the measurements: for
holding, F1 of at least {{ "0.70" }} and precision of at least {{ "0.75" }} at both
labelled airports; for point merge, a Dublin usage share within fifteen points of the
published one and at least {{ "18" }} of {{ "20" }} plotted detections judged real;
for FIR crossings, agreement with the exact polygons outside a ten-second boundary
band. How each was decided: holding passes the F1 bar at both airports but EGLL's
precision ({{ h_egll_p }}) is under {{ "0.75" }}; it ships on anyway, because the
two-airport score is level with the published detector's and a detector that is
off by default is not in production, and this paragraph is where that is said. The
point-merge usage share is not comparable to the published figure (section 6) and
is reported, not applied; the visual check ({{ pm_check_detected_real }} of
{{ pm_check_detected }} real) is what the family was judged on. FIR crossings pass.

Step 04 on the pipeline day took {{ pd_step04_off_min }} minutes with the three
families off and {{ pd_step04_on_min }} with them on ({{ pd_step04_extra_pct }} more).
Rows written on the day:

{{ pd_counts_table }}

## 9. Still unverified

- FIR crossings against the Network Manager's records (the laptop extract).
- Point merge at airports whose merge point is not placed, at the ones placed from
  navigation data of an earlier cycle than the catalogue's, and at the Norwegian
  sites, where no leg was found.
- Holding outside the two labelled airports and the one labelled day.
