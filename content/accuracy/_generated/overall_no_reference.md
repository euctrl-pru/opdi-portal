<!-- Written by tools/gen_accuracy_tables.py from assets/accuracy/; do not edit. -->

| Item | Values on 10 June | Values on 14 June | Values on 17 June | Available among OPDI's flights (three days) | What a reference would need to be |
|---|---|---|---|---|---|
| [`first_seen` (flight list)](../algorithms/first-last-seen.qmd) | 175,000 | 161,900 | 180,500 | 100.0 % high | none needed: they are the times of the first and last report received |
| [`last_seen` (flight list)](../algorithms/first-last-seen.qmd) | 175,000 | 161,900 | 180,500 | 100.0 % high | none needed: they are the times of the first and last report received |
| [`first_seen`](../algorithms/first-last-seen.qmd) | 176,300 | 163,100 | 182,000 | 100.0 % high | none needed: they are the times of the first and last report received |
| [`last_seen`](../algorithms/first-last-seen.qmd) | 176,300 | 163,100 | 182,000 | 100.0 % high | none needed: they are the times of the first and last report received |
| [`go-around`](../algorithms/go-around.qmd) | 686 | 289 | 605 | 0.4 % low | records of go-arounds from the airport or air traffic control |
| [`runway-crossing-entry`](../algorithms/runway-crossings.qmd) | 140 | 120 | 152 | 0.1 % low | records of runway crossings from the airport or air traffic control |
| [`runway-crossing-vacated`](../algorithms/runway-crossings.qmd) | 140 | 120 | 152 | 0.1 % low | records of runway crossings from the airport or air traffic control |
| [`entry-apron`](../algorithms/surface-events.qmd) | 65,400 | 60,450 | 65,780 | 26.4 % low | records of the taxi route or of surface movement radar |
| [`exit-apron`](../algorithms/surface-events.qmd) | 65,400 | 60,450 | 65,780 | 26.4 % low | records of the taxi route or of surface movement radar |
| [`entry-taxiway`](../algorithms/surface-events.qmd) | 588,600 | 561,400 | 589,700 | 38.2 % low | records of the taxi route or of surface movement radar |
| [`exit-taxiway`](../algorithms/surface-events.qmd) | 588,600 | 561,400 | 589,700 | 38.2 % low | records of the taxi route or of surface movement radar |
| [`entry-threshold`](../algorithms/surface-events.qmd) | 3,926 | 3,659 | 4,346 | 2.2 % low | records of the taxi route or of surface movement radar |
| [`exit-threshold`](../algorithms/surface-events.qmd) | 3,926 | 3,659 | 4,346 | 2.2 % low | records of the taxi route or of surface movement radar |
| [`entry-deicing_pad`](../algorithms/surface-events.qmd) | 689 | 548 | 694 | 0.2 % low | records of the taxi route or of surface movement radar |
| [`exit-deicing_pad`](../algorithms/surface-events.qmd) | 689 | 548 | 694 | 0.2 % low | records of the taxi route or of surface movement radar |
| [`entry-hangar`](../algorithms/surface-events.qmd) | 333 | 213 | 430 | 0.1 % low | records of the taxi route or of surface movement radar |
| [`exit-hangar`](../algorithms/surface-events.qmd) | 333 | 213 | 430 | 0.1 % low | records of the taxi route or of surface movement radar |
| [`top-of-climb`](../algorithms/tops.qmd) | 160,200 | 148,300 | 165,800 | 91.5 % high | a measured vertical profile of the flight |
| [`top-of-descent`](../algorithms/tops.qmd) | 160,200 | 148,300 | 165,800 | 91.5 % high | a measured vertical profile of the flight |
| [`level-start`](../algorithms/level-segments.qmd) | 541,900 | 484,700 | 566,000 | 81.0 % high | a measured vertical profile of the flight |
| [`level-end`](../algorithms/level-segments.qmd) | 541,900 | 484,700 | 566,000 | 81.0 % high | a measured vertical profile of the flight |
| [`level-off-climb-start`](../algorithms/level-offs.qmd) | 114,000 | 97,810 | 127,400 | 23.2 % low | a measured vertical profile of the flight |
| [`level-off-climb-end`](../algorithms/level-offs.qmd) | 114,000 | 97,810 | 127,400 | 23.2 % low | a measured vertical profile of the flight |
| [`level-off-descent-start`](../algorithms/level-offs.qmd) | 278,600 | 263,700 | 306,500 | 46.0 % low | a measured vertical profile of the flight |
| [`level-off-descent-end`](../algorithms/level-offs.qmd) | 278,600 | 263,700 | 306,500 | 46.0 % low | a measured vertical profile of the flight |
| [`xing-fl50`](../algorithms/flight-level-crossings.qmd) | 173,600 | 168,100 | 181,200 | 60.8 % partial | a measured vertical profile of the flight |
| [`xing-fl70`](../algorithms/flight-level-crossings.qmd) | 170,900 | 165,700 | 176,000 | 59.2 % partial | a measured vertical profile of the flight |
| [`xing-fl100`](../algorithms/flight-level-crossings.qmd) | 166,600 | 161,300 | 170,500 | 56.9 % partial | a measured vertical profile of the flight |
| [`xing-fl245`](../algorithms/flight-level-crossings.qmd) | 139,700 | 138,700 | 141,200 | 46.6 % low | a measured vertical profile of the flight |
| [`xing-50nm`, inbound](../algorithms/distance-rings.qmd) | 86,530 | 83,800 | 88,740 | 75.8 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-50nm`, outbound](../algorithms/distance-rings.qmd) | 92,880 | 89,060 | 94,620 | 73.8 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-60nm`, inbound](../algorithms/distance-rings.qmd) | 82,990 | 81,140 | 85,090 | 73.5 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-60nm`, outbound](../algorithms/distance-rings.qmd) | 88,810 | 86,070 | 90,540 | 71.4 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-110nm`, inbound](../algorithms/distance-rings.qmd) | 72,940 | 72,610 | 74,730 | 65.8 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-110nm`, outbound](../algorithms/distance-rings.qmd) | 77,180 | 76,440 | 78,700 | 63.4 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-120nm`, inbound](../algorithms/distance-rings.qmd) | 71,490 | 71,370 | 73,250 | 64.6 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-120nm`, outbound](../algorithms/distance-rings.qmd) | 75,450 | 74,910 | 77,100 | 62.1 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c50_arr` (flight list)](../algorithms/distance-rings.qmd) | 83,500 | 81,800 | 85,670 | 75.8 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c50_dep` (flight list)](../algorithms/distance-rings.qmd) | 89,670 | 86,820 | 91,460 | 73.8 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c60_arr` (flight list)](../algorithms/distance-rings.qmd) | 80,890 | 79,590 | 82,910 | 73.5 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c60_dep` (flight list)](../algorithms/distance-rings.qmd) | 86,620 | 84,370 | 88,250 | 71.4 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c110_arr` (flight list)](../algorithms/distance-rings.qmd) | 72,160 | 71,870 | 73,890 | 65.8 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c110_dep` (flight list)](../algorithms/distance-rings.qmd) | 76,420 | 75,680 | 77,830 | 63.4 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c120_arr` (flight list)](../algorithms/distance-rings.qmd) | 70,750 | 70,670 | 72,440 | 64.6 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`c120_dep` (flight list)](../algorithms/distance-rings.qmd) | 74,750 | 74,210 | 76,290 | 62.1 % partial | crossing times of this circle; the airport records give only the 40 and 100 NM circles |
| [`xing-fir`](../algorithms/fir-crossings.qmd) | 292,400 | 297,500 | 300,900 | 45.5 % low | records of the times each flight entered each FIR |
| [`holding-start`](../algorithms/holding.qmd) | 1,207 | 736 | 1,074 | 0.7 % low | records of holds from air traffic control |
| [`holding-end`](../algorithms/holding.qmd) | 1,207 | 736 | 1,074 | 0.7 % low | records of holds from air traffic control |
| [`holding_duration_s`](../algorithms/holding.qmd) | 1,207 | 736 | 1,074 | 0.5 % low | records of holds from air traffic control |
| [`holding_distance_nm`](../algorithms/holding.qmd) | 1,207 | 736 | 1,074 | 0.5 % low | records of holds from air traffic control |
| [`holding_circuits`](../algorithms/holding.qmd) | 1,207 | 736 | 1,074 | 0.5 % low | records of holds from air traffic control |
| [`pms-leg-entry`](../algorithms/point-merge.qmd) | 651 | 575 | 624 | 0.6 % low | records of the instructions given to each arrival |
| [`pms-leg-exit`](../algorithms/point-merge.qmd) | 651 | 575 | 624 | 0.6 % low | records of the instructions given to each arrival |
| [`pms-merge-point`](../algorithms/point-merge.qmd) | 4,181 | 3,418 | 4,020 | 3.5 % low | records of the instructions given to each arrival |
| [`pms_leg_time_s`](../algorithms/point-merge.qmd) | 651 | 575 | 624 | 0.4 % low | records of the instructions given to each arrival |
| [`pms_leg_distance_nm`](../algorithms/point-merge.qmd) | 651 | 575 | 624 | 0.4 % low | records of the instructions given to each arrival |
| [`pms_direct_to_distance_nm`](../algorithms/point-merge.qmd) | 651 | 575 | 624 | 0.4 % low | records of the instructions given to each arrival |
| [`time_passed_s`](../algorithms/time-passed.qmd) | 6,417,000 | 6,057,000 | 6,624,000 | 100.0 % high | none needed: it is the time between two report times |

: Items with no reference to compare with: how many there are each day. {.striped .hover}
