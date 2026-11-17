<!-- Written by tools/gen_accuracy_tables.py from assets/accuracy/; do not edit. -->

| Item | Availability among OPDI's flights | Reference | Availability against the reference | Accuracy |
|---|---|---|---|---|
| [`first_seen` (flight list)](first-last-seen.qmd) | high | none |  | no reference |
| [`last_seen` (flight list)](first-last-seen.qmd) | high | none |  | no reference |
| [`first_seen`](first-last-seen.qmd) | high | none |  | no reference |
| [`last_seen`](first-last-seen.qmd) | high | none |  | no reference |
| [`adep` (flight list)](airports.qmd) | partial | Network Manager | high | good |
| [`ades` (flight list)](airports.qmd) | partial | Network Manager | partial | good |
| [`typecode` (flight list)](aircraft.qmd) | high | Network Manager | high | good |
| [`ATOT`](take-off-landing.qmd) | partial | airport records | high | good |
| [`ATOT`](take-off-landing.qmd) | partial | Network Manager | low | good |
| [`ATOT` with `info.method` = `acdm`](take-off-landing.qmd) | low | airport records | low | good |
| [`ATOT` with `info.method` = `legacy`](take-off-landing.qmd) | low | airport records | low | good |
| [`ATOT` runway (`info.runway`)](take-off-landing.qmd) |  | airport records | high | good |
| [`ALDT`](take-off-landing.qmd) | partial | airport records | high | good |
| [`ALDT`](take-off-landing.qmd) | partial | Network Manager | low | good |
| [`ALDT` with `info.method` = `acdm`](take-off-landing.qmd) | low | airport records | partial | good |
| [`ALDT` with `info.method` = `legacy`](take-off-landing.qmd) | low | airport records | low | good |
| [`ALDT` runway (`info.runway`)](take-off-landing.qmd) |  | airport records | high | good |
| [`atot` (flight list)](flight-list-milestones.qmd) | partial | airport records | high | good |
| [`atot` (flight list)](flight-list-milestones.qmd) | partial | Network Manager | low | good |
| [`aldt` (flight list)](flight-list-milestones.qmd) | partial | airport records | high | good |
| [`aldt` (flight list)](flight-list-milestones.qmd) | partial | Network Manager | low | good |
| [`rwy_dep` (flight list)](flight-list-milestones.qmd) | partial | airport records | high | good |
| [`rwy_arr` (flight list)](flight-list-milestones.qmd) | partial | airport records | high | good |
| [`line-up`](line-up-take-off-roll.qmd) | low | airport records | low | fair |
| [`take-off-roll`](line-up-take-off-roll.qmd) | low | airport records | low | good |
| [`xing-runway-threshold`](xing-runway-threshold.qmd) | low | airport records | low | good |
| [`runway-vacated`](runway-vacated.qmd) | low | airport records | low | fair |
| [`go-around`](go-around.qmd) | low | none |  | no reference |
| [`runway-crossing-entry`](runway-crossings.qmd) | low | none |  | no reference |
| [`runway-crossing-vacated`](runway-crossings.qmd) | low | none |  | no reference |
| [`AOBT`](block-times.qmd) | low | airport records | low | limited |
| [`AOBT` stand (`info.stand`)](block-times.qmd) |  | airport records | low | fair |
| [`AIBT`](block-times.qmd) | low | airport records | low | fair |
| [`AIBT` stand (`info.stand`)](block-times.qmd) |  | airport records | low | fair |
| [`aobt` (flight list)](flight-list-milestones.qmd) | low | airport records | low | limited |
| [`aibt` (flight list)](flight-list-milestones.qmd) | low | airport records | low | fair |
| [`stnd_dep` (flight list)](flight-list-milestones.qmd) | low | airport records | low | fair |
| [`stnd_arr` (flight list)](flight-list-milestones.qmd) | low | airport records | low | fair |
| [`entry-parking_position`](surface-events.qmd) | low | airport records | low | fair |
| [`exit-parking_position`](surface-events.qmd) | low | airport records | low | limited |
| [`entry-apron`](surface-events.qmd) | low | none |  | no reference |
| [`exit-apron`](surface-events.qmd) | low | none |  | no reference |
| [`entry-taxiway`](surface-events.qmd) | low | none |  | no reference |
| [`exit-taxiway`](surface-events.qmd) | low | none |  | no reference |
| [`entry-threshold`](surface-events.qmd) | low | none |  | no reference |
| [`exit-threshold`](surface-events.qmd) | low | none |  | no reference |
| [`entry-deicing_pad`](surface-events.qmd) | low | none |  | no reference |
| [`exit-deicing_pad`](surface-events.qmd) | low | none |  | no reference |
| [`entry-hangar`](surface-events.qmd) | low | none |  | no reference |
| [`exit-hangar`](surface-events.qmd) | low | none |  | no reference |
| [`top-of-climb`](tops.qmd) | high | none |  | no reference |
| [`top-of-descent`](tops.qmd) | high | none |  | no reference |
| [`level-start`](level-segments.qmd) | high | none |  | no reference |
| [`level-end`](level-segments.qmd) | high | none |  | no reference |
| [`level-off-climb-start`](level-offs.qmd) | low | none |  | no reference |
| [`level-off-climb-end`](level-offs.qmd) | low | none |  | no reference |
| [`level-off-descent-start`](level-offs.qmd) | low | none |  | no reference |
| [`level-off-descent-end`](level-offs.qmd) | low | none |  | no reference |
| [`xing-fl50`](flight-level-crossings.qmd) | partial | none |  | no reference |
| [`xing-fl70`](flight-level-crossings.qmd) | partial | none |  | no reference |
| [`xing-fl100`](flight-level-crossings.qmd) | partial | none |  | no reference |
| [`xing-fl245`](flight-level-crossings.qmd) | low | none |  | no reference |
| [`xing-40nm`, inbound](distance-rings.qmd) | partial | airport records | high | good |
| [`xing-40nm`, outbound](distance-rings.qmd) | partial | airport records | high | good |
| [`xing-50nm`, inbound](distance-rings.qmd) | partial | none |  | no reference |
| [`xing-50nm`, outbound](distance-rings.qmd) | partial | none |  | no reference |
| [`xing-60nm`, inbound](distance-rings.qmd) | partial | none |  | no reference |
| [`xing-60nm`, outbound](distance-rings.qmd) | partial | none |  | no reference |
| [`xing-100nm`, inbound](distance-rings.qmd) | partial | airport records | high | good |
| [`xing-100nm`, outbound](distance-rings.qmd) | partial | airport records | high | good |
| [`xing-110nm`, inbound](distance-rings.qmd) | partial | none |  | no reference |
| [`xing-110nm`, outbound](distance-rings.qmd) | partial | none |  | no reference |
| [`xing-120nm`, inbound](distance-rings.qmd) | partial | none |  | no reference |
| [`xing-120nm`, outbound](distance-rings.qmd) | partial | none |  | no reference |
| [`c40_arr` (flight list)](distance-rings.qmd) | partial | airport records | high | good |
| [`c40_dep` (flight list)](distance-rings.qmd) | partial | airport records | high | good |
| [`c50_arr` (flight list)](distance-rings.qmd) | partial | none |  | no reference |
| [`c50_dep` (flight list)](distance-rings.qmd) | partial | none |  | no reference |
| [`c60_arr` (flight list)](distance-rings.qmd) | partial | none |  | no reference |
| [`c60_dep` (flight list)](distance-rings.qmd) | partial | none |  | no reference |
| [`c100_arr` (flight list)](distance-rings.qmd) | partial | airport records | high | good |
| [`c100_dep` (flight list)](distance-rings.qmd) | partial | airport records | high | good |
| [`c110_arr` (flight list)](distance-rings.qmd) | partial | none |  | no reference |
| [`c110_dep` (flight list)](distance-rings.qmd) | partial | none |  | no reference |
| [`c120_arr` (flight list)](distance-rings.qmd) | partial | none |  | no reference |
| [`c120_dep` (flight list)](distance-rings.qmd) | partial | none |  | no reference |
| [`xing-fir`](fir-crossings.qmd) | low | none |  | no reference |
| [`holding-start`](holding.qmd) | low | none |  | no reference |
| [`holding-end`](holding.qmd) | low | none |  | no reference |
| [`holding_duration_s`](holding.qmd) | low | none |  | no reference |
| [`holding_distance_nm`](holding.qmd) | low | none |  | no reference |
| [`holding_circuits`](holding.qmd) | low | none |  | no reference |
| [`pms-leg-entry`](point-merge.qmd) | low | none |  | no reference |
| [`pms-leg-exit`](point-merge.qmd) | low | none |  | no reference |
| [`pms-merge-point`](point-merge.qmd) | low | none |  | no reference |
| [`pms_leg_time_s`](point-merge.qmd) | low | none |  | no reference |
| [`pms_leg_distance_nm`](point-merge.qmd) | low | none |  | no reference |
| [`pms_direct_to_distance_nm`](point-merge.qmd) | low | none |  | no reference |
| [`distance_flown_nm`](distance-flown.qmd) | high | Network Manager | high | good |
| [`time_passed_s`](time-passed.qmd) | high | none |  | no reference |

: Labels of every item, the three days together. The numbers behind them are on Accuracy and availability. {.striped .hover}
