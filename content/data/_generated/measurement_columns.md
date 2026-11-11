<!-- Written by tools/gen_data_dictionary.py from src/opdi/output/schema.py in the opdi repository. Do not edit by hand. -->

| Column | Type | Unit | Description |
|---|---|---|---|
| `distance_flown_nm` | double | NM | Distance flown from the start of the track to this event, along the received positions, in nautical miles (NM, 1 NM = 1,852 m). |
| `time_passed_s` | double | s | Time from the first position of the track to this event, in seconds. |
| `holding_duration_s` | double | s | Time spent in a holding pattern, in seconds. A holding pattern is a loop flown while the aircraft waits for permission to continue. It is set only on holding-end events and is NULL on every other event. |
| `holding_distance_nm` | double | NM | Distance flown in the holding pattern, in nautical miles (NM). It is set only on holding-end events and is NULL on every other event. |
| `holding_circuits` | double | count | Number of loops flown in the holding pattern: the total turn divided by 360 degrees, to one decimal. It is set only on holding-end events and is NULL on every other event. |
| `pms_leg_time_s` | double | s | Time spent on a point merge leg, in seconds. A point merge system is an arrival procedure in which aircraft fly along an arc, called a leg, at a fixed distance from a merge point until they are sent directly to it. It is set only on pms-leg-exit events and is NULL on every other event. |
| `pms_leg_distance_nm` | double | NM | Distance flown along the point merge leg, in nautical miles (NM). It is set only on pms-leg-exit events and is NULL on every other event. |
| `pms_direct_to_distance_nm` | double | NM | Distance from the aircraft to the merge point when it left the leg, in nautical miles (NM). It is set only on pms-leg-exit events and is NULL on every other event. |
