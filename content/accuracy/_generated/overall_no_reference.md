<!-- Written by tools/gen_accuracy_tables.py from assets/accuracy/; do not edit. -->

| Item | Values on 10 June | Values on 14 June | Values on 17 June | Available among OPDI's flights (three days) | What a reference would need to be |
|---|---|---|---|---|---|
| [`first_seen` (flight list)](../algorithms/first-last-seen.qmd) | 175,900 | 162,800 | 181,400 | 100.0 % high | none needed: they are the times of the first and last report received |
| [`last_seen` (flight list)](../algorithms/first-last-seen.qmd) | 175,900 | 162,800 | 181,400 | 100.0 % high | none needed: they are the times of the first and last report received |
| [`first_seen`](../algorithms/first-last-seen.qmd) | 175,000 | 161,900 | 180,500 | 99.5 % high | none needed: they are the times of the first and last report received |
| [`last_seen`](../algorithms/first-last-seen.qmd) | 175,000 | 161,900 | 180,500 | 99.5 % high | none needed: they are the times of the first and last report received |
| [`go-around`](../algorithms/go-around.qmd) | 686 | 289 | 605 | 0.4 % low | records of go-arounds from the airport or air traffic control |
| [`runway-crossing-entry`](../algorithms/runway-crossings.qmd) | 140 | 120 | 152 | 0.1 % low | records of runway crossings from the airport or air traffic control |
| [`runway-crossing-vacated`](../algorithms/runway-crossings.qmd) | 140 | 120 | 152 | 0.1 % low | records of runway crossings from the airport or air traffic control |
| [`entry-apron`](../algorithms/surface-events.qmd) | 65,360 | 60,420 | 65,730 | 26.4 % low | records of the taxi route or of surface movement radar |
| [`exit-apron`](../algorithms/surface-events.qmd) | 65,360 | 60,420 | 65,730 | 26.4 % low | records of the taxi route or of surface movement radar |
| [`entry-taxiway`](../algorithms/surface-events.qmd) | 588,400 | 561,300 | 589,400 | 38.5 % low | records of the taxi route or of surface movement radar |
| [`exit-taxiway`](../algorithms/surface-events.qmd) | 588,400 | 561,300 | 589,400 | 38.5 % low | records of the taxi route or of surface movement radar |
| [`entry-threshold`](../algorithms/surface-events.qmd) | 3,926 | 3,658 | 4,344 | 2.2 % low | records of the taxi route or of surface movement radar |
| [`exit-threshold`](../algorithms/surface-events.qmd) | 3,926 | 3,658 | 4,344 | 2.2 % low | records of the taxi route or of surface movement radar |
| [`entry-deicing_pad`](../algorithms/surface-events.qmd) | 689 | 548 | 694 | 0.2 % low | records of the taxi route or of surface movement radar |
| [`exit-deicing_pad`](../algorithms/surface-events.qmd) | 689 | 548 | 694 | 0.2 % low | records of the taxi route or of surface movement radar |
| [`entry-hangar`](../algorithms/surface-events.qmd) | 333 | 213 | 430 | 0.1 % low | records of the taxi route or of surface movement radar |
| [`exit-hangar`](../algorithms/surface-events.qmd) | 333 | 213 | 430 | 0.1 % low | records of the taxi route or of surface movement radar |
| [`top-of-climb`](../algorithms/tops.qmd) | 159,900 | 148,100 | 165,500 | 91.0 % high | a measured vertical profile of the flight |
| [`top-of-descent`](../algorithms/tops.qmd) | 159,900 | 148,100 | 165,500 | 91.0 % high | a measured vertical profile of the flight |
| [`level-start`](../algorithms/level-segments.qmd) | 541,700 | 484,500 | 565,700 | 81.1 % high | a measured vertical profile of the flight |
| [`level-end`](../algorithms/level-segments.qmd) | 541,700 | 484,500 | 565,700 | 81.1 % high | a measured vertical profile of the flight |
| [`level-off-climb-start`](../algorithms/level-offs.qmd) | 114,000 | 97,760 | 127,400 | 23.1 % low | a measured vertical profile of the flight |
| [`level-off-climb-end`](../algorithms/level-offs.qmd) | 114,000 | 97,760 | 127,400 | 23.1 % low | a measured vertical profile of the flight |
| [`level-off-descent-start`](../algorithms/level-offs.qmd) | 278,500 | 263,600 | 306,300 | 45.8 % low | a measured vertical profile of the flight |
| [`level-off-descent-end`](../algorithms/level-offs.qmd) | 278,500 | 263,600 | 306,300 | 45.8 % low | a measured vertical profile of the flight |
| [`xing-fl50`](../algorithms/flight-level-crossings.qmd) | 173,500 | 168,000 | 181,100 | 60.9 % partial | a measured vertical profile of the flight |
| [`xing-fl70`](../algorithms/flight-level-crossings.qmd) | 170,800 | 165,600 | 176,000 | 59.3 % partial | a measured vertical profile of the flight |
| [`xing-fl100`](../algorithms/flight-level-crossings.qmd) | 166,500 | 161,300 | 170,400 | 56.9 % partial | a measured vertical profile of the flight |
| [`xing-fl245`](../algorithms/flight-level-crossings.qmd) | 139,600 | 138,600 | 141,100 | 46.6 % low | a measured vertical profile of the flight |
| [`xing-50nm`, inbound](../algorithms/distance-rings.qmd) | 87,110 | 84,410 | 89,330 | 75.8 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-50nm`, outbound](../algorithms/distance-rings.qmd) | 93,480 | 89,680 | 95,240 | 73.9 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-60nm`, inbound](../algorithms/distance-rings.qmd) | 83,520 | 81,720 | 85,620 | 73.5 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-60nm`, outbound](../algorithms/distance-rings.qmd) | 89,360 | 86,660 | 91,110 | 71.5 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-110nm`, inbound](../algorithms/distance-rings.qmd) | 73,380 | 73,120 | 75,190 | 65.8 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-110nm`, outbound](../algorithms/distance-rings.qmd) | 77,640 | 76,960 | 79,180 | 63.3 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-120nm`, inbound](../algorithms/distance-rings.qmd) | 71,920 | 71,870 | 73,710 | 64.6 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-120nm`, outbound](../algorithms/distance-rings.qmd) | 75,900 | 75,420 | 77,560 | 62.0 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c50_arr` (flight list)](../algorithms/distance-rings.qmd) | 84,040 | 82,390 | 86,240 | 75.8 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c50_dep` (flight list)](../algorithms/distance-rings.qmd) | 90,340 | 87,510 | 92,170 | 73.9 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c60_arr` (flight list)](../algorithms/distance-rings.qmd) | 81,400 | 80,140 | 83,450 | 73.5 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c60_dep` (flight list)](../algorithms/distance-rings.qmd) | 87,240 | 85,010 | 88,910 | 71.5 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c110_arr` (flight list)](../algorithms/distance-rings.qmd) | 72,580 | 72,340 | 74,320 | 65.8 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c110_dep` (flight list)](../algorithms/distance-rings.qmd) | 76,900 | 76,220 | 78,360 | 63.3 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c120_arr` (flight list)](../algorithms/distance-rings.qmd) | 71,140 | 71,130 | 72,860 | 64.6 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c120_dep` (flight list)](../algorithms/distance-rings.qmd) | 75,220 | 74,740 | 76,780 | 62.0 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-fir`](../algorithms/fir-crossings.qmd) | 292,200 | 297,300 | 300,600 | 45.5 % low | records of the times each flight entered each FIR |
| [`holding-start`](../algorithms/holding.qmd) | 1,206 | 734 | 1,071 | 0.7 % low | records of holds from air traffic control |
| [`holding-end`](../algorithms/holding.qmd) | 1,206 | 734 | 1,071 | 0.7 % low | records of holds from air traffic control |
| [`holding_duration_s`](../algorithms/holding.qmd) | 1,206 | 734 | 1,071 | 0.5 % low | records of holds from air traffic control |
| [`holding_distance_nm`](../algorithms/holding.qmd) | 1,206 | 734 | 1,071 | 0.5 % low | records of holds from air traffic control |
| [`holding_circuits`](../algorithms/holding.qmd) | 1,206 | 734 | 1,071 | 0.5 % low | records of holds from air traffic control |
| [`pms-leg-entry`](../algorithms/point-merge.qmd) | 651 | 575 | 624 | 0.6 % low | records of the instructions given to each arrival |
| [`pms-leg-exit`](../algorithms/point-merge.qmd) | 651 | 575 | 624 | 0.6 % low | records of the instructions given to each arrival |
| [`pms-merge-point`](../algorithms/point-merge.qmd) | 4,179 | 3,415 | 4,014 | 3.5 % low | records of the instructions given to each arrival |
| [`pms_leg_time_s`](../algorithms/point-merge.qmd) | 651 | 575 | 624 | 0.4 % low | records of the instructions given to each arrival |
| [`pms_leg_distance_nm`](../algorithms/point-merge.qmd) | 651 | 575 | 624 | 0.4 % low | records of the instructions given to each arrival |
| [`pms_direct_to_distance_nm`](../algorithms/point-merge.qmd) | 651 | 575 | 624 | 0.4 % low | records of the instructions given to each arrival |
| [`time_passed_s`](../algorithms/time-passed.qmd) | 6,418,000 | 6,059,000 | 6,625,000 | 100.0 % high | none needed: it is the time between two report times |

: Items with no reference to compare with: how many there are each day. {.striped .hover}
