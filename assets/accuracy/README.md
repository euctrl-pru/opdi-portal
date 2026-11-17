# Accuracy and availability tables

Made by `benchmarks/airport_accuracy.py` in the `opdi` repository (branch
`industrialisation`) from three processed days: Wednesday 10 June,
Sunday 14 June and Wednesday 17 June 2026, worldwide, with the default
settings of the pipeline (174,997, 161,867 and 180,479 flight-list rows;
6.42, 6.06 and 6.63 million events). `_manifest.json` gives, for each
file, the command, the commit, the inputs and their row counts. The tables
were made with

    python benchmarks/airport_accuracy.py extract --day 2026-06-10   # and 14, 17 June
    python benchmarks/airport_accuracy.py tables --out <this folder>

The files hold counts and shares only: no flight, track id or call sign.

## Two measures, kept apart

- **Availability**: how often an item exists.
  - *Among OPDI's own flights* (`flights_eligible`, `flights_with_item`,
    `availability_own_pct`): of the flights that could carry the item, the
    share that do. Departure items (take-off, off-block, line-up, ...) count
    among departures, arrival items among arrivals, the other items among all
    flights. No reference is needed, so this is given for every item and every
    airport.
  - *Against a reference* (`reference_movements`, `found`,
    `availability_ref_pct`): of the movements in the reference records, the
    share for which OPDI has the item. This also counts the flights OPDI did
    not receive at all.
- **Accuracy**: how close the item is to the reference when both exist
  (`compared` values). Found is not the same as correct: an item can be
  available and wrong, or rare and exact.

## References

- **Airport records** (`airport records`): the movement records the airport
  operators send to EUROCONTROL (off-block, take-off, landing and in-block
  times, runway, stand, crossing times of the 40 and 100 NM circles). Only
  some European airports send them. Many give times to the whole minute.
- **Network Manager** (`Network Manager`): EUROCONTROL's records of every
  flight in or through European airspace: departure and destination airport,
  aircraft type, route length, and a take-off time (off-block time plus taxi
  time) and landing time. These two times are themselves estimates; in June
  2026 they agree with the airport records within 60 s for 97% (take-off)
  and 89% (landing) of the movements. They are used for times only at
  European airports that send no airport records.
- **No reference** (`no reference`): nothing to compare with. Only
  availability is given.

A record is matched to an OPDI flight on the aircraft address (the
transponder's 24-bit code), the call sign and the airport, the nearest within
6 hours. Records without an aircraft address cannot be matched and are not
counted.
A movement of a flight whose OPDI row belongs to the day before (for
example a landing just after midnight) counts as not found, so availability
against a reference is slightly low at the ends of each day.

## Items

- Event items are the event types. The ring crossings are split by direction
  (`xing-40nm inbound` is the crossing of the 40 NM circle around the
  destination on the way in, `outbound` around the departure airport on the
  way out). `ATOT (acdm)` / `ATOT (legacy)` and `ALDT (acdm)` / `ALDT (legacy)`
  are the take-off and landing times by the method that produced them (only
  in `item_accuracy.csv`).
- `ATOT runway (info.runway)`, `ALDT runway (info.runway)`,
  `AOBT stand (info.stand)`, `AIBT stand (info.stand)`: the runway or stand
  named in the event's `info`.
- `flight list: <COLUMN>`: a column of the published flight list.
- `measurement: <name>`: a measurement attached to events.

`side` says which flights could carry the item: `departure`, `arrival`, or
`any` (every flight). An item that names an airport (runway, surface and block
events) is counted only at that airport.

## Files and columns

Shares are in percent (0 to 100), times in seconds. An empty cell means "not
applicable" or "nothing to compute".

### item_accuracy.csv

One row per item and reference, for the three days together
(`days` = `all three days`) and for each day.

| column | meaning |
|---|---|
| `days` | `all three days` or the date |
| `item`, `kind`, `side` | the item (see above); `event`, `flight list` or `measurement`; the flights that could carry it |
| `values` | number of events (or filled flight-list cells, or measurements) |
| `values_per_day` | `values` divided by the number of days |
| `flights_eligible` | OPDI's flights that could carry the item: those with a departure airport (departure items), with a destination airport (arrival items), or all |
| `flights_with_item` | those of them that carry it |
| `availability_own_pct`, `availability_own_label` | the share, and its label |
| `reference` | `airport records`, `Network Manager` or `no reference` |
| `compared_with` | the reference value, in words (for example "take-off time") |
| `reference_scope` | which movements the reference covers here |
| `reference_movements` | reference movements (with an aircraft address); for runways and stands, those whose record names one |
| `reference_tracked` | airport records only: the records of flights that are in OPDI's flight list (the scorecard's "records") |
| `found` | reference movements for which OPDI has the item |
| `availability_ref_pct`, `availability_ref_label` | `found` / `reference_movements`, and its label |
| `compared` | OPDI values compared with a reference value (a record matched by two events is compared twice) |
| `within_30s_pct`, `within_60s_pct`, `within_300s_pct` | times: share of the compared values within 30, 60, 300 s of the reference |
| `median_abs_diff_s`, `p90_abs_diff_s` | times: median and 90th percentile of the absolute difference |
| `mean_signed_diff_within_300s_s` | times: mean of OPDI minus reference over the differences of at most 300 s (positive: OPDI later) |
| `share_equal_pct` | airports, runways, stands, aircraft types: share equal to the reference among the compared |
| `median_ratio`, `within_10pct_pct` | distance flown: median of distance flown / Network Manager route length, and share within 10% of it |
| `accuracy_measure` | which share the accuracy label uses: `share within 60 s`, `share equal`, or `share within 10% of the route length` |
| `accuracy_value_pct` | that share |
| `accuracy_label` | see the label rules |

### airport_accuracy_europe.csv

The 100 European airports with the most Network Manager flights (departures
plus arrivals) on the three days; one row per airport and item, the three
days together. European: ICAO codes starting with E or L, and BI, BK, GC, UK,
UD, UG, UB. Columns as in `item_accuracy.csv`, plus `rank`, `airport` (ICAO
code), `name`, `country` (ISO code), `nm_movements` and `has_airport_records`
(the airport sends airport records). The reference of each row:
departure and destination airport, aircraft type and distance flown are
compared with Network Manager at every airport; times, runways and stands
with the airport records where the airport sends them; at an airport that
sends none, take-off and landing times (event and flight list) are compared
with Network Manager and the other items have `no reference` (availability
among OPDI's flights only).

### airport_accuracy_world.csv

The 100 airports with the most movements in OPDI's own flight list on the
three days (departures plus arrivals). Columns as for Europe, with
`opdi_movements` instead of `nm_movements`, `in_europe`, and
`reference_note`, which says in words what can be checked there. Outside
Europe there are no airport records and Network Manager knows only the
flights to and from Europe: those rows give availability among OPDI's own
flights, and accuracy only for departure and destination airport and
aircraft type of those flights. Availability there is not accuracy: an item
found at a US airport is not confirmed by anything.

### airports.csv

One row per airport in either list: `airport`, `name`, `country`,
`rank_europe`, `rank_world`, `nm_movements`, `opdi_movements`,
`airport_record_movements` (all movements in the airport records, three
days), `has_airport_records`, `in_europe`, and six reception indicators
among OPDI's own flights at the airport: `departures_with_off_block_pct`,
`departures_with_line_up_pct`, `departures_with_stand_pct` (flight-list
departure stand), `departures_with_take_off_pct`,
`arrivals_with_landing_pct`, `arrivals_with_in_block_pct`. Low off-block,
line-up and stand shares mean the receivers near the airport do not see
aircraft on the ground; the times are then missing, not wrong.

### day_spread.csv

For each item, reference and measure (`availability_own_pct`,
`availability_ref_pct`, `within_60s_pct`, `median_abs_diff_s`,
`share_equal_pct`): the value on each day, `spread` (largest minus
smallest), and `stable`: true when a share moves less than 5 points, or a
time less than 10 s or less than a fifth of its middle value.

## Label rules

- Availability (`availability_own_label`, `availability_ref_label`): `high`
  at least 80%, `partial` 50% to under 80%, `low` under 50%.
- Accuracy (`accuracy_label`), from `accuracy_value_pct`: `good` at least
  80%, `fair` 50% to under 80%, `limited` under 50%. `no reference` when
  there is no reference or fewer than 30 reference movements;
  `too few compared` when the reference has at least 30 movements but fewer
  than 30 OPDI values could be compared (the item is then mostly missing:
  read the availability).
- Labels go beside the numbers, never instead of them.

## Things to know when reading the tables

- An empty `flights_eligible` at an airport means OPDI has no flight of that
  side there (for example no arrival at all: LGIR, LATI and LUKK in the
  European table have departures but no arrivals in OPDI's flight list).
- The rows `ATOT runway (info.runway)` and the like have no availability
  among OPDI's flights of their own; read the row of the event (`ATOT`).
- The airport records give many times to the whole minute. A share within
  30 s is therefore lower than the true agreement; the share within 60 s is
  the one the labels use.
