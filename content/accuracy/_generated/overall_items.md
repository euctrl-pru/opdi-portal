<!-- Written by tools/gen_accuracy_tables.py from assets/accuracy/; do not edit. -->

| Item | Values per day | OPDI flights that could carry it | Available among them | Reference | Compared with | Reference movements | Available against the reference | Compared | Accuracy | Median difference | 90th percentile | Accuracy label |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| [`first_seen` (flight list)](../algorithms/first-last-seen.qmd) | 172,400 | 517,300 | 100.0 % high | none |  |  |  |  |  |  |  | no reference |
| [`last_seen` (flight list)](../algorithms/first-last-seen.qmd) | 172,400 | 517,300 | 100.0 % high | none |  |  |  |  |  |  |  | no reference |
| [`first_seen`](../algorithms/first-last-seen.qmd) | 173,800 | 517,300 | 100.0 % high | none |  |  |  |  |  |  |  | no reference |
| [`last_seen`](../algorithms/first-last-seen.qmd) | 173,800 | 517,300 | 100.0 % high | none |  |  |  |  |  |  |  | no reference |
| [`adep` (flight list)](../algorithms/airports.qmd) | 130,700 | 517,300 | 75.8 % partial | Network Manager | departure airport | 96,390 | 85.9 % high | 82,780 | 98.6 % equal |  |  | good |
| [`ades` (flight list)](../algorithms/airports.qmd) | 111,000 | 517,300 | 64.3 % partial | Network Manager | destination airport | 96,390 | 78.5 % partial | 75,660 | 98.5 % equal |  |  | good |
| [`typecode` (flight list)](../algorithms/aircraft.qmd) | 141,700 | 517,300 | 82.2 % high | Network Manager | aircraft type | 96,390 | 82.8 % high | 79,850 | 98.1 % equal |  |  | good |
| [`ATOT`](../algorithms/take-off-landing.qmd) | 62,590 | 363,000 | 50.8 % partial | airport records | take-off time | 57,340 | 80.8 % high | 46,350 | 93.2 % within 60 s | 21 s | 56 s | good |
| [`ATOT`](../algorithms/take-off-landing.qmd) | 62,590 | 363,000 | 50.8 % partial | Network Manager | take-off time (off-block + taxi time) | 27,530 | 37.6 % low | 10,340 | 85.5 % within 60 s | 29 s | 69 s | good |
| [`ATOT` with `info.method` = `acdm`](../algorithms/take-off-landing.qmd) | 34,390 | 363,000 | 27.5 % low | airport records | take-off time | 57,340 | 48.1 % low | 27,580 | 94.3 % within 60 s | 18 s | 54 s | good |
| [`ATOT` with `info.method` = `legacy`](../algorithms/take-off-landing.qmd) | 28,200 | 363,000 | 23.3 % low | airport records | take-off time | 57,340 | 32.7 % low | 18,770 | 91.5 % within 60 s | 24 s | 59 s | good |
| [`ATOT` runway (`info.runway`)](../algorithms/take-off-landing.qmd) |  |  |  | airport records | runway | 57,340 | 80.8 % high | 46,350 | 97.0 % equal |  |  | good |
| [`ALDT`](../algorithms/take-off-landing.qmd) | 73,060 | 331,100 | 65.3 % partial | airport records | landing time | 57,550 | 81.7 % high | 47,010 | 94.5 % within 60 s | 18 s | 51 s | good |
| [`ALDT`](../algorithms/take-off-landing.qmd) | 73,060 | 331,100 | 65.3 % partial | Network Manager | landing time | 27,630 | 47.5 % low | 13,110 | 83.1 % within 60 s | 24 s | 78 s | good |
| [`ALDT` with `info.method` = `acdm`](../algorithms/take-off-landing.qmd) | 40,040 | 331,100 | 35.4 % low | airport records | landing time | 57,550 | 58.6 % partial | 33,760 | 97.1 % within 60 s | 14 s | 43 s | good |
| [`ALDT` with `info.method` = `legacy`](../algorithms/take-off-landing.qmd) | 33,020 | 331,100 | 29.9 % low | airport records | landing time | 57,550 | 23.0 % low | 13,260 | 87.9 % within 60 s | 27 s | 64 s | good |
| [`ALDT` runway (`info.runway`)](../algorithms/take-off-landing.qmd) |  |  |  | airport records | runway | 57,550 | 81.7 % high | 47,010 | 96.6 % equal |  |  | good |
| [`atot` (flight list)](../algorithms/flight-list-milestones.qmd) | 61,490 | 363,000 | 50.8 % partial | airport records | take-off time | 57,340 | 80.5 % high | 46,190 | 93.2 % within 60 s | 21 s | 56 s | good |
| [`atot` (flight list)](../algorithms/flight-list-milestones.qmd) | 61,490 | 363,000 | 50.8 % partial | Network Manager | take-off time (off-block + taxi time) | 27,530 | 37.3 % low | 10,270 | 85.5 % within 60 s | 29 s | 69 s | good |
| [`aldt` (flight list)](../algorithms/flight-list-milestones.qmd) | 72,040 | 331,100 | 65.3 % partial | airport records | landing time | 57,550 | 81.4 % high | 46,850 | 94.5 % within 60 s | 18 s | 51 s | good |
| [`aldt` (flight list)](../algorithms/flight-list-milestones.qmd) | 72,040 | 331,100 | 65.3 % partial | Network Manager | landing time | 27,630 | 47.2 % low | 13,050 | 83.1 % within 60 s | 24 s | 78 s | good |
| [`rwy_dep` (flight list)](../algorithms/flight-list-milestones.qmd) | 61,490 | 363,000 | 50.8 % partial | airport records | runway | 57,340 | 80.5 % high | 46,190 | 97.0 % equal |  |  | good |
| [`rwy_arr` (flight list)](../algorithms/flight-list-milestones.qmd) | 72,040 | 331,100 | 65.3 % partial | airport records | runway | 57,550 | 81.4 % high | 46,850 | 96.7 % equal |  |  | good |
| [`line-up`](../algorithms/line-up-take-off-roll.qmd) | 30,510 | 363,000 | 24.3 % low | airport records | take-off time | 57,340 | 43.9 % low | 25,190 | 70.0 % within 60 s | 38 s | 96 s | fair |
| [`take-off-roll`](../algorithms/line-up-take-off-roll.qmd) | 30,630 | 363,000 | 24.4 % low | airport records | take-off time | 57,340 | 43.9 % low | 25,190 | 98.0 % within 60 s | 18 s | 39 s | good |
| [`xing-runway-threshold`](../algorithms/xing-runway-threshold.qmd) | 27,910 | 331,100 | 24.6 % low | airport records | landing time | 57,550 | 39.1 % low | 22,520 | 97.6 % within 60 s | 13 s | 42 s | good |
| [`runway-vacated`](../algorithms/runway-vacated.qmd) | 35,480 | 331,100 | 31.0 % low | airport records | landing time | 57,550 | 49.6 % low | 28,530 | 73.7 % within 60 s | 39 s | 84 s | fair |
| [`go-around`](../algorithms/go-around.qmd) | 527 | 331,100 | 0.4 % low | none |  |  |  |  |  |  |  | no reference |
| [`runway-crossing-entry`](../algorithms/runway-crossings.qmd) | 137 | 517,300 | 0.1 % low | none |  |  |  |  |  |  |  | no reference |
| [`runway-crossing-vacated`](../algorithms/runway-crossings.qmd) | 137 | 517,300 | 0.1 % low | none |  |  |  |  |  |  |  | no reference |
| [`AOBT`](../algorithms/block-times.qmd) | 21,540 | 363,000 | 17.5 % low | airport records | off-block time | 57,340 | 35.0 % low | 20,080 | 40.3 % within 60 s | 99 s | 484 s | limited |
| [`AOBT` stand (`info.stand`)](../algorithms/block-times.qmd) |  |  |  | airport records | stand | 57,310 | 25.7 % low | 14,740 | 61.9 % equal |  |  | fair |
| [`AIBT`](../algorithms/block-times.qmd) | 21,580 | 331,100 | 19.2 % low | airport records | in-block time | 57,550 | 33.4 % low | 19,230 | 72.0 % within 60 s | 34 s | 161 s | fair |
| [`AIBT` stand (`info.stand`)](../algorithms/block-times.qmd) |  |  |  | airport records | stand | 57,510 | 24.7 % low | 14,220 | 74.1 % equal |  |  | fair |
| [`aobt` (flight list)](../algorithms/flight-list-milestones.qmd) | 21,230 | 363,000 | 17.5 % low | airport records | off-block time | 57,340 | 34.8 % low | 19,950 | 40.3 % within 60 s | 99 s | 482 s | limited |
| [`aibt` (flight list)](../algorithms/flight-list-milestones.qmd) | 21,270 | 331,100 | 19.2 % low | airport records | in-block time | 57,550 | 33.2 % low | 19,120 | 72.1 % within 60 s | 34 s | 160 s | fair |
| [`stnd_dep` (flight list)](../algorithms/flight-list-milestones.qmd) | 19,200 | 363,000 | 13.2 % low | airport records | stand | 57,310 | 28.2 % low | 16,150 | 58.6 % equal |  |  | fair |
| [`stnd_arr` (flight list)](../algorithms/flight-list-milestones.qmd) | 15,700 | 331,100 | 14.2 % low | airport records | stand | 57,510 | 25.8 % low | 14,840 | 72.5 % equal |  |  | fair |
| [`entry-parking_position`](../algorithms/surface-events.qmd) | 152,300 | 517,300 | 21.3 % low | airport records | in-block time (nearest event per record) | 57,550 | 28.9 % low | 16,660 | 79.0 % within 60 s | 26 s | 108 s | fair |
| [`exit-parking_position`](../algorithms/surface-events.qmd) | 152,300 | 517,300 | 21.3 % low | airport records | off-block time (nearest event per record) | 57,340 | 31.0 % low | 17,780 | 32.0 % within 60 s | 115 s | 386 s | limited |
| [`entry-apron`](../algorithms/surface-events.qmd) | 63,880 | 517,300 | 26.4 % low | none |  |  |  |  |  |  |  | no reference |
| [`exit-apron`](../algorithms/surface-events.qmd) | 63,880 | 517,300 | 26.4 % low | none |  |  |  |  |  |  |  | no reference |
| [`entry-taxiway`](../algorithms/surface-events.qmd) | 579,900 | 517,300 | 38.2 % low | none |  |  |  |  |  |  |  | no reference |
| [`exit-taxiway`](../algorithms/surface-events.qmd) | 579,900 | 517,300 | 38.2 % low | none |  |  |  |  |  |  |  | no reference |
| [`entry-threshold`](../algorithms/surface-events.qmd) | 3,977 | 517,300 | 2.2 % low | none |  |  |  |  |  |  |  | no reference |
| [`exit-threshold`](../algorithms/surface-events.qmd) | 3,977 | 517,300 | 2.2 % low | none |  |  |  |  |  |  |  | no reference |
| [`entry-deicing_pad`](../algorithms/surface-events.qmd) | 644 | 517,300 | 0.2 % low | none |  |  |  |  |  |  |  | no reference |
| [`exit-deicing_pad`](../algorithms/surface-events.qmd) | 644 | 517,300 | 0.2 % low | none |  |  |  |  |  |  |  | no reference |
| [`entry-hangar`](../algorithms/surface-events.qmd) | 325 | 517,300 | 0.1 % low | none |  |  |  |  |  |  |  | no reference |
| [`exit-hangar`](../algorithms/surface-events.qmd) | 325 | 517,300 | 0.1 % low | none |  |  |  |  |  |  |  | no reference |
| [`top-of-climb`](../algorithms/tops.qmd) | 158,100 | 517,300 | 91.5 % high | none |  |  |  |  |  |  |  | no reference |
| [`top-of-descent`](../algorithms/tops.qmd) | 158,100 | 517,300 | 91.5 % high | none |  |  |  |  |  |  |  | no reference |
| [`level-start`](../algorithms/level-segments.qmd) | 530,900 | 517,300 | 81.0 % high | none |  |  |  |  |  |  |  | no reference |
| [`level-end`](../algorithms/level-segments.qmd) | 530,900 | 517,300 | 81.0 % high | none |  |  |  |  |  |  |  | no reference |
| [`level-off-climb-start`](../algorithms/level-offs.qmd) | 113,100 | 517,300 | 23.2 % low | none |  |  |  |  |  |  |  | no reference |
| [`level-off-climb-end`](../algorithms/level-offs.qmd) | 113,100 | 517,300 | 23.2 % low | none |  |  |  |  |  |  |  | no reference |
| [`level-off-descent-start`](../algorithms/level-offs.qmd) | 282,900 | 517,300 | 46.0 % low | none |  |  |  |  |  |  |  | no reference |
| [`level-off-descent-end`](../algorithms/level-offs.qmd) | 282,900 | 517,300 | 46.0 % low | none |  |  |  |  |  |  |  | no reference |
| [`xing-fl50`](../algorithms/flight-level-crossings.qmd) | 174,300 | 517,300 | 60.8 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-fl70`](../algorithms/flight-level-crossings.qmd) | 170,900 | 517,300 | 59.2 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-fl100`](../algorithms/flight-level-crossings.qmd) | 166,100 | 517,300 | 56.9 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-fl245`](../algorithms/flight-level-crossings.qmd) | 139,900 | 517,300 | 46.6 % low | none |  |  |  |  |  |  |  | no reference |
| [`xing-40nm`, inbound](../algorithms/distance-rings.qmd) | 90,910 | 331,100 | 79.1 % partial | airport records | 40 NM crossing time | 56,400 | 89.5 % high | 50,830 | 98.5 % within 60 s | 2 s | 9 s | good |
| [`xing-40nm`, outbound](../algorithms/distance-rings.qmd) | 97,120 | 363,000 | 76.6 % partial | airport records | 40 NM crossing time | 56,230 | 96.1 % high | 54,070 | 99.6 % within 60 s | 2 s | 6 s | good |
| [`xing-50nm`, inbound](../algorithms/distance-rings.qmd) | 86,360 | 331,100 | 75.8 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-50nm`, outbound](../algorithms/distance-rings.qmd) | 92,190 | 363,000 | 73.8 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-60nm`, inbound](../algorithms/distance-rings.qmd) | 83,070 | 331,100 | 73.5 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-60nm`, outbound](../algorithms/distance-rings.qmd) | 88,470 | 363,000 | 71.4 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-100nm`, inbound](../algorithms/distance-rings.qmd) | 74,890 | 331,100 | 67.0 % partial | airport records | 100 NM crossing time | 56,010 | 88.8 % high | 49,760 | 99.7 % within 60 s | 2 s | 7 s | good |
| [`xing-100nm`, outbound](../algorithms/distance-rings.qmd) | 79,170 | 363,000 | 64.6 % partial | airport records | 100 NM crossing time | 55,850 | 95.4 % high | 53,310 | 99.8 % within 60 s | 2 s | 6 s | good |
| [`xing-110nm`, inbound](../algorithms/distance-rings.qmd) | 73,430 | 331,100 | 65.8 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-110nm`, outbound](../algorithms/distance-rings.qmd) | 77,440 | 363,000 | 63.4 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-120nm`, inbound](../algorithms/distance-rings.qmd) | 72,040 | 331,100 | 64.6 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-120nm`, outbound](../algorithms/distance-rings.qmd) | 75,820 | 363,000 | 62.1 % partial | none |  |  |  |  |  |  |  | no reference |
| [`c40_arr` (flight list)](../algorithms/distance-rings.qmd) | 87,270 | 331,100 | 79.1 % partial | airport records | 40 NM crossing time | 56,400 | 89.5 % high | 50,500 | 99.1 % within 60 s | 2 s | 8 s | good |
| [`c40_dep` (flight list)](../algorithms/distance-rings.qmd) | 92,690 | 363,000 | 76.6 % partial | airport records | 40 NM crossing time | 56,230 | 96.1 % high | 54,020 | 99.6 % within 60 s | 2 s | 6 s | good |
| [`c50_arr` (flight list)](../algorithms/distance-rings.qmd) | 83,660 | 331,100 | 75.8 % partial | none |  |  |  |  |  |  |  | no reference |
| [`c50_dep` (flight list)](../algorithms/distance-rings.qmd) | 89,320 | 363,000 | 73.8 % partial | none |  |  |  |  |  |  |  | no reference |
| [`c60_arr` (flight list)](../algorithms/distance-rings.qmd) | 81,130 | 331,100 | 73.5 % partial | none |  |  |  |  |  |  |  | no reference |
| [`c60_dep` (flight list)](../algorithms/distance-rings.qmd) | 86,410 | 363,000 | 71.4 % partial | none |  |  |  |  |  |  |  | no reference |
| [`c100_arr` (flight list)](../algorithms/distance-rings.qmd) | 73,940 | 331,100 | 67.0 % partial | airport records | 100 NM crossing time | 56,010 | 88.8 % high | 49,730 | 99.7 % within 60 s | 2 s | 7 s | good |
| [`c100_dep` (flight list)](../algorithms/distance-rings.qmd) | 78,210 | 363,000 | 64.6 % partial | airport records | 100 NM crossing time | 55,850 | 95.4 % high | 53,270 | 99.8 % within 60 s | 2 s | 6 s | good |
| [`c110_arr` (flight list)](../algorithms/distance-rings.qmd) | 72,640 | 331,100 | 65.8 % partial | none |  |  |  |  |  |  |  | no reference |
| [`c110_dep` (flight list)](../algorithms/distance-rings.qmd) | 76,640 | 363,000 | 63.4 % partial | none |  |  |  |  |  |  |  | no reference |
| [`c120_arr` (flight list)](../algorithms/distance-rings.qmd) | 71,290 | 331,100 | 64.6 % partial | none |  |  |  |  |  |  |  | no reference |
| [`c120_dep` (flight list)](../algorithms/distance-rings.qmd) | 75,080 | 363,000 | 62.1 % partial | none |  |  |  |  |  |  |  | no reference |
| [`xing-fir`](../algorithms/fir-crossings.qmd) | 296,900 | 517,300 | 45.5 % low | none |  |  |  |  |  |  |  | no reference |
| [`holding-start`](../algorithms/holding.qmd) | 1,006 | 331,100 | 0.7 % low | none |  |  |  |  |  |  |  | no reference |
| [`holding-end`](../algorithms/holding.qmd) | 1,006 | 331,100 | 0.7 % low | none |  |  |  |  |  |  |  | no reference |
| [`holding_duration_s`](../algorithms/holding.qmd) | 1,006 | 517,300 | 0.5 % low | none |  |  |  |  |  |  |  | no reference |
| [`holding_distance_nm`](../algorithms/holding.qmd) | 1,006 | 517,300 | 0.5 % low | none |  |  |  |  |  |  |  | no reference |
| [`holding_circuits`](../algorithms/holding.qmd) | 1,006 | 517,300 | 0.5 % low | none |  |  |  |  |  |  |  | no reference |
| [`pms-leg-entry`](../algorithms/point-merge.qmd) | 617 | 331,100 | 0.6 % low | none |  |  |  |  |  |  |  | no reference |
| [`pms-leg-exit`](../algorithms/point-merge.qmd) | 617 | 331,100 | 0.6 % low | none |  |  |  |  |  |  |  | no reference |
| [`pms-merge-point`](../algorithms/point-merge.qmd) | 3,873 | 331,100 | 3.5 % low | none |  |  |  |  |  |  |  | no reference |
| [`pms_leg_time_s`](../algorithms/point-merge.qmd) | 617 | 517,300 | 0.4 % low | none |  |  |  |  |  |  |  | no reference |
| [`pms_leg_distance_nm`](../algorithms/point-merge.qmd) | 617 | 517,300 | 0.4 % low | none |  |  |  |  |  |  |  | no reference |
| [`pms_direct_to_distance_nm`](../algorithms/point-merge.qmd) | 617 | 517,300 | 0.4 % low | none |  |  |  |  |  |  |  | no reference |
| [`distance_flown_nm`](../algorithms/distance-flown.qmd) | 6,318,000 | 517,300 | 92.1 % high | Network Manager | route length | 96,390 | 96.1 % high | 92,630 | 80.0 % within 10 % of the route length |  |  | good |
| [`time_passed_s`](../algorithms/time-passed.qmd) | 6,366,000 | 517,300 | 100.0 % high | none |  |  |  |  |  |  |  | no reference |

: Every published item, the three days together. Shares in percent; times in seconds. An empty cell means not applicable. {.striped .hover}
