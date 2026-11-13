<!-- Written by tools/gen_validation_summary.py from assets/validation/validation_measures.csv. Do not edit by hand. -->

| Item | Kind | Headline accuracy | Reference | n | Details |
|---|---|---|---|---:|---|
| `top-of-climb` | event | Not validated |  |  | [Validation](vertical-events.qmd#validation-method), [Limitations](vertical-events.qmd#limitations) |
| `top-of-descent` | event | Not validated |  |  | [Validation](vertical-events.qmd#validation-method), [Limitations](vertical-events.qmd#limitations) |
| `level-start` | event | Not validated |  |  | [Validation](vertical-events.qmd#validation-method), [Limitations](vertical-events.qmd#limitations) |
| `level-end` | event | Not validated |  |  | [Validation](vertical-events.qmd#validation-method), [Limitations](vertical-events.qmd#limitations) |
| `level-off-climb-start` | event | Not validated |  |  | [Validation](vertical-events.qmd#validation-method), [Limitations](vertical-events.qmd#limitations) |
| `level-off-climb-end` | event | Not validated |  |  | [Validation](vertical-events.qmd#validation-method), [Limitations](vertical-events.qmd#limitations) |
| `level-off-descent-start` | event | Not validated |  |  | [Validation](vertical-events.qmd#validation-method), [Limitations](vertical-events.qmd#limitations) |
| `level-off-descent-end` | event | Not validated |  |  | [Validation](vertical-events.qmd#validation-method), [Limitations](vertical-events.qmd#limitations) |
| `xing-fl{level}` | event | Not validated |  |  | [Validation](vertical-events.qmd#validation-method), [Limitations](vertical-events.qmd#limitations) |
| `xing-{radius}nm` | event | Arrival crossing of the 40 NM ring: share of reference arrivals with an OPDI time within 10 s: 88.48% | Ring crossings recorded with the airport operator records (APDF) for arrivals at the 20 best-received airports, 2026-06-05 to 2026-06-07 | 11481 | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `entry-{element}` | event | Not validated |  |  | [Validation](surface-events.qmd#validation-method), [Limitations](surface-events.qmd#limitations) |
| `exit-{element}` | event | Not validated |  |  | [Validation](surface-events.qmd#validation-method), [Limitations](surface-events.qmd#limitations) |
| `take-off-roll` | event | Not validated |  |  | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `ATOT` | event | Take-off time as published: share of reference departures with an OPDI time within 60 s: 86.97% | Airport operator records (APDF) of the 20 best-received airports, matched on aircraft address, call sign and day, 2026-06-05 to 2026-06-07 | 11675 | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `xing-runway-threshold` | event | Not validated |  |  | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `ALDT` | event | Landing time as published: share of reference arrivals with an OPDI time within 60 s: 94.36% | Airport operator records (APDF) of the 20 best-received airports, matched on aircraft address, call sign and day, 2026-06-05 to 2026-06-07 | 11657 | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `line-up` | event | Not validated |  |  | [Validation](runway-milestones.qmd#validation-method), [Limitations](runway-milestones.qmd#limitations) |
| `runway-vacated` | event | Not validated |  |  | [Validation](runway-milestones.qmd#validation-method), [Limitations](runway-milestones.qmd#limitations) |
| `go-around` | event | Not validated |  |  | [Validation](runway-milestones.qmd#validation-method), [Limitations](runway-milestones.qmd#limitations) |
| `runway-crossing-entry` | event | Not validated |  |  | [Validation](runway-milestones.qmd#validation-method), [Limitations](runway-milestones.qmd#limitations) |
| `runway-crossing-vacated` | event | Not validated |  |  | [Validation](runway-milestones.qmd#validation-method), [Limitations](runway-milestones.qmd#limitations) |
| `AOBT` | event | Off-block time: share of reference departures with an OPDI time within 60 s: 42.66% | Airport operator records (APDF) of the 20 best-received airports, matched on aircraft address, call sign and day, 2026-06-05 to 2026-06-07 | 11674 | [Validation](block-times.qmd#validation-method), [Limitations](block-times.qmd#limitations) |
| `AIBT` | event | In-block time: share of reference arrivals with an OPDI time within 60 s: 41.02% | Airport operator records (APDF) of the 20 best-received airports, matched on aircraft address, call sign and day, 2026-06-05 to 2026-06-07 | 11657 | [Validation](block-times.qmd#validation-method), [Limitations](block-times.qmd#limitations) |
| `holding-start` | event | F1 score: the balance of precision and recall (1 is perfect): 0.74 | Hand-corrected holding labels of Olive and others, London Heathrow (EGLL) and Dublin (EIDW), 1 October 2019 | 346 | [Validation](holding.qmd#validation-method), [Limitations](holding.qmd#limitations) |
| `holding-end` | event | F1 score: the balance of precision and recall (1 is perfect): 0.74 | Hand-corrected holding labels of Olive and others, London Heathrow (EGLL) and Dublin (EIDW), 1 October 2019 | 346 | [Validation](holding.qmd#validation-method), [Limitations](holding.qmd#limitations) |
| `pms-leg-entry` | event | Detected legs that show a real sequencing leg when plotted: 95.0% | Visual inspection of 20 detected and 20 undetected arrivals at Dublin (EIDW), 2026-06-14 | 20 | [Validation](point-merge.qmd#validation-method), [Limitations](point-merge.qmd#limitations) |
| `pms-leg-exit` | event | Detected legs that show a real sequencing leg when plotted: 95.0% | Visual inspection of 20 detected and 20 undetected arrivals at Dublin (EIDW), 2026-06-14 | 20 | [Validation](point-merge.qmd#validation-method), [Limitations](point-merge.qmd#limitations) |
| `pms-merge-point` | event | Detected legs that show a real sequencing leg when plotted: 95.0% | Visual inspection of 20 detected and 20 undetected arrivals at Dublin (EIDW), 2026-06-14 | 20 | [Validation](point-merge.qmd#validation-method), [Limitations](point-merge.qmd#limitations) |
| `xing-fir` | event | Detected crossings that match a crossing of the same two regions found with the exact boundaries: 96.7% | Exact point-in-polygon test against the same FIR boundaries (AIRAC cycle 524), 2,000 tracks of 2026-06-14 | 8352 | [Validation](fir-crossings.qmd#validation-method), [Limitations](fir-crossings.qmd#limitations) |
| `first_seen` | event | Not validated |  |  | [Validation](first-last-seen.qmd#validation-method), [Limitations](first-last-seen.qmd#limitations) |
| `last_seen` | event | Not validated |  |  | [Validation](first-last-seen.qmd#validation-method), [Limitations](first-last-seen.qmd#limitations) |
| `flight_id` | flight list column | Share of reference flights that come out as exactly one track: 95.2% | EUROCONTROL Network Manager flight data for Europe, 2025-06-14 and 2026-06-14 | 58436 | [Validation](track-identity.qmd#validation-method), [Limitations](track-identity.qmd#limitations) |
| `icao24` | flight list column | Not validated |  |  | [Validation](track-identity.qmd#validation-method), [Limitations](track-identity.qmd#limitations) |
| `flt_id` | flight list column | Not validated |  |  | [Validation](track-identity.qmd#validation-method), [Limitations](track-identity.qmd#limitations) |
| `dof` | flight list column | Not validated |  |  | [Validation](track-identity.qmd#validation-method), [Limitations](track-identity.qmd#limitations) |
| `adep` | flight list column | Departure airport: share of the airports named that are right (June 2025 sample): 98.2% | EUROCONTROL Network Manager movement data, 2025-06-05 to 2025-06-07, matched on aircraft address, call sign and date | 95116 | [Validation](airports.qmd#validation-method), [Limitations](airports.qmd#limitations) |
| `ades` | flight list column | Destination airport: share of the airports named that are right (June 2025 sample): 98.6% | EUROCONTROL Network Manager movement data, 2025-06-05 to 2025-06-07, matched on aircraft address, call sign and date | 95116 | [Validation](airports.qmd#validation-method), [Limitations](airports.qmd#limitations) |
| `adep_p` | flight list column | Not validated |  |  | [Validation](airports.qmd#validation-method), [Limitations](airports.qmd#limitations) |
| `ades_p` | flight list column | Not validated |  |  | [Validation](airports.qmd#validation-method), [Limitations](airports.qmd#limitations) |
| `adep_source` | flight list column | Not validated |  |  | [Validation](airports.qmd#validation-method), [Limitations](airports.qmd#limitations) |
| `ades_source` | flight list column | Not validated |  |  | [Validation](airports.qmd#validation-method), [Limitations](airports.qmd#limitations) |
| `registration` | flight list column | Not validated |  |  | [Processing steps](pipeline.qmd#flight-list) |
| `model` | flight list column | Not validated |  |  | [Processing steps](pipeline.qmd#flight-list) |
| `typecode` | flight list column | Not validated |  |  | [Processing steps](pipeline.qmd#flight-list) |
| `icao_aircraft_class` | flight list column | Not validated |  |  | [Processing steps](pipeline.qmd#flight-list) |
| `icao_operator` | flight list column | Not validated |  |  | [Processing steps](pipeline.qmd#flight-list) |
| `first_seen` | flight list column | Not validated |  |  | [Validation](first-last-seen.qmd#validation-method), [Limitations](first-last-seen.qmd#limitations) |
| `last_seen` | flight list column | Not validated |  |  | [Validation](first-last-seen.qmd#validation-method), [Limitations](first-last-seen.qmd#limitations) |
| `atot` | flight list column | Take-off time as published: share of reference departures with an OPDI time within 60 s: 86.97% | Airport operator records (APDF) of the 20 best-received airports, matched on aircraft address, call sign and day, 2026-06-05 to 2026-06-07 | 11675 | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `aldt` | flight list column | Landing time as published: share of reference arrivals with an OPDI time within 60 s: 94.36% | Airport operator records (APDF) of the 20 best-received airports, matched on aircraft address, call sign and day, 2026-06-05 to 2026-06-07 | 11657 | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `aobt` | flight list column | Off-block time: share of reference departures with an OPDI time within 60 s: 42.66% | Airport operator records (APDF) of the 20 best-received airports, matched on aircraft address, call sign and day, 2026-06-05 to 2026-06-07 | 11674 | [Validation](block-times.qmd#validation-method), [Limitations](block-times.qmd#limitations) |
| `aibt` | flight list column | In-block time: share of reference arrivals with an OPDI time within 60 s: 41.02% | Airport operator records (APDF) of the 20 best-received airports, matched on aircraft address, call sign and day, 2026-06-05 to 2026-06-07 | 11657 | [Validation](block-times.qmd#validation-method), [Limitations](block-times.qmd#limitations) |
| `rwy_dep` | flight list column | Take-off runway named by the runway-pass method: share equal to the runway in the airport record: 86.05% | Airport operator records (APDF) of the 20 best-received airports, matched on aircraft address, call sign and day, 2026-06-05 to 2026-06-07 | 8944 | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `rwy_dep_bearing_deg` | flight list column | Not validated |  |  | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `rwy_arr` | flight list column | Landing runway named by the runway-pass method: share equal to the runway in the airport record: 92.56% | Airport operator records (APDF) of the 20 best-received airports, matched on aircraft address, call sign and day, 2026-06-05 to 2026-06-07 | 9369 | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `rwy_arr_bearing_deg` | flight list column | Not validated |  |  | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `stnd_dep` | flight list column | Not validated |  |  | [Validation](block-times.qmd#validation-method), [Limitations](block-times.qmd#limitations) |
| `stnd_arr` | flight list column | Not validated |  |  | [Validation](block-times.qmd#validation-method), [Limitations](block-times.qmd#limitations) |
| `c40_arr` | flight list column | Arrival crossing of the 40 NM ring: share of reference arrivals with an OPDI time within 10 s: 88.48% | Ring crossings recorded with the airport operator records (APDF) for arrivals at the 20 best-received airports, 2026-06-05 to 2026-06-07 | 11481 | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c50_arr` | flight list column | Not validated |  |  | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c60_arr` | flight list column | Not validated |  |  | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c100_arr` | flight list column | Arrival crossing of the 100 NM ring: share of reference arrivals with an OPDI time within 10 s: 87.92% | Ring crossings recorded with the airport operator records (APDF) for arrivals at the 20 best-received airports, 2026-06-05 to 2026-06-07 | 11423 | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c110_arr` | flight list column | Not validated |  |  | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c120_arr` | flight list column | Not validated |  |  | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c40_dep` | flight list column | Not validated |  |  | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c50_dep` | flight list column | Not validated |  |  | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c60_dep` | flight list column | Not validated |  |  | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c100_dep` | flight list column | Not validated |  |  | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c110_dep` | flight list column | Not validated |  |  | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `c120_dep` | flight list column | Not validated |  |  | [Validation](distance-rings.qmd#validation-method), [Limitations](distance-rings.qmd#limitations) |
| `track_duration_min` | flight list column | Not validated |  |  | [Validation](first-last-seen.qmd#validation-method), [Limitations](first-last-seen.qmd#limitations) |
| `superseded_dep` | flight list column | Not validated |  |  | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `superseded_arr` | flight list column | Not validated |  |  | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `movement_dep` | flight list column | Not validated |  |  | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `movement_arr` | flight list column | Not validated |  |  | [Validation](takeoff-landing.qmd#validation-method), [Limitations](takeoff-landing.qmd#limitations) |
| `version` | flight list column | Not validated |  |  |  |
| `release` | flight list column | Not validated |  |  |  |
| `distance_flown_nm` | measurement | Not validated |  |  | [Validation](measurements.qmd#validation-method), [Limitations](measurements.qmd#limitations) |
| `time_passed_s` | measurement | Not validated |  |  | [Validation](measurements.qmd#validation-method), [Limitations](measurements.qmd#limitations) |
| `holding_duration_s` | measurement | Not validated |  |  | [Validation](holding.qmd#validation-method), [Limitations](holding.qmd#limitations) |
| `holding_distance_nm` | measurement | Not validated |  |  | [Validation](holding.qmd#validation-method), [Limitations](holding.qmd#limitations) |
| `holding_circuits` | measurement | Not validated |  |  | [Validation](holding.qmd#validation-method), [Limitations](holding.qmd#limitations) |
| `pms_leg_time_s` | measurement | Not validated |  |  | [Validation](point-merge.qmd#validation-method), [Limitations](point-merge.qmd#limitations) |
| `pms_leg_distance_nm` | measurement | Not validated |  |  | [Validation](point-merge.qmd#validation-method), [Limitations](point-merge.qmd#limitations) |
| `pms_direct_to_distance_nm` | measurement | Not validated |  |  | [Validation](point-merge.qmd#validation-method), [Limitations](point-merge.qmd#limitations) |
