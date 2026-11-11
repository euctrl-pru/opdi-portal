<!-- Written by tools/gen_data_dictionary.py from src/opdi/output/schema.py in the opdi repository. Do not edit by hand. -->

| Column | Type | Unit | Description |
|---|---|---|---|
| `flight_id` | string |  | Unique identifier of the flight. A flight is one track: the sequence of position reports of one aircraft that belong together. |
| `icao24` | string |  | ICAO 24-bit address of the aircraft's transponder, written as six hexadecimal characters. It normally stays with one aircraft. |
| `flt_id` | string |  | Call sign broadcast by the aircraft, for example DLH123. |
| `dof` | date | UTC | Date of the flight: the day on which it was first seen. A day's files hold the flights of that date. |
| `adep` | string |  | Departure airport, as its four-letter ICAO code. 'OOA' (out of area) when the track began at the edge of the covered area, so the flight departed from somewhere outside it. This occurs only in data built with European coverage. NULL when no departure airport could be identified, which includes every overflight. |
| `ades` | string |  | Destination airport, as its four-letter ICAO code. 'OOA' (out of area) when the track ended at the edge of the covered area, so the flight continued to somewhere outside it. This occurs only in data built with European coverage. NULL when no destination airport could be identified, which includes every overflight. |
| `adep_p` | string |  | Other airports near the start of the track that the flight could have left from, as a comma-separated list of ICAO codes, nearest first. |
| `ades_p` | string |  | Other airports near the end of the track that the flight could have gone to, as a comma-separated list of ICAO codes, nearest first. |
| `adep_source` | string |  | How the departure airport was decided. 'aerodrome': adep names the airport near which the track began, found from the position and height of its first reports. 'runway_event': adep was taken from a take-off detected on one of that airport's runways, which takes precedence over the first method. 'out_of_area': adep is 'OOA'. 'undetermined': no airport could be identified and adep is NULL, as for an overflight. |
| `ades_source` | string |  | How the destination airport was decided, with the same four values as adep_source: 'aerodrome' (found from the last reports of the track), 'runway_event' (taken from a landing detected on one of the airport's runways), 'out_of_area' (ades is 'OOA') and 'undetermined' (ades is NULL). |
| `registration` | string |  | Registration of the aircraft (its tail number), from the OpenSky aircraft database. NULL when the aircraft is not in that database. |
| `model` | string |  | Aircraft model, from the OpenSky aircraft database. |
| `typecode` | string |  | ICAO aircraft type designator, for example A320, from the OpenSky aircraft database. |
| `icao_aircraft_class` | string |  | ICAO aircraft description, for example L2J for a landplane with two jet engines, from the OpenSky aircraft database. |
| `icao_operator` | string |  | Three-letter ICAO code of the aircraft operator, from the OpenSky aircraft database. |
| `first_seen` | timestamp | UTC | Time of the first position report of the flight received by the OpenSky Network. |
| `last_seen` | timestamp | UTC | Time of the last position report of the flight received by the OpenSky Network. |
| `atot` | timestamp | UTC | Actual take-off time: when the aircraft left the runway at the departure airport. Taken from the flight's earliest ATOT event at that airport. NULL when no take-off was detected. |
| `aldt` | timestamp | UTC | Actual landing time: when the aircraft touched down at the destination airport. Taken from the flight's latest ALDT event at that airport. NULL when no landing was detected. |
| `aobt` | timestamp | UTC | Actual off-block time: when the aircraft started to move away from its parking position. Taken from the flight's earliest AOBT event. NULL when it was not detected, which is common because it needs reception on the ground. |
| `aibt` | timestamp | UTC | Actual in-block time: when the aircraft stopped at its parking position after landing. Taken from the flight's latest AIBT event. NULL when it was not detected, which is common because it needs reception on the ground. |
| `rwy_dep` | string |  | Runway used for take-off, for example 25R. Taken from the same event as atot. |
| `rwy_dep_bearing_deg` | double | degrees | True bearing of the centre line of the take-off runway, in degrees clockwise from north. It describes the runway, not the heading of the aircraft. |
| `rwy_arr` | string |  | Runway used for landing, for example 07L. Taken from the same event as aldt. |
| `rwy_arr_bearing_deg` | double | degrees | True bearing of the centre line of the landing runway, in degrees clockwise from north. It describes the runway, not the heading of the aircraft. |
| `stnd_dep` | string |  | Parking position (stand) the flight left from, with the name it has in OpenStreetMap. NULL when it was not detected. |
| `stnd_arr` | string |  | Parking position (stand) the flight arrived at, with the name it has in OpenStreetMap. NULL when it was not detected. |
| `c40_arr` | timestamp | UTC | Time when the arriving flight last crossed, inbound, a circle of 40 nautical miles (NM) radius around the destination airport. NULL when no such crossing was detected. |
| `c50_arr` | timestamp | UTC | Time when the arriving flight last crossed, inbound, a circle of 50 nautical miles (NM) radius around the destination airport. NULL when no such crossing was detected. |
| `c60_arr` | timestamp | UTC | Time when the arriving flight last crossed, inbound, a circle of 60 nautical miles (NM) radius around the destination airport. NULL when no such crossing was detected. |
| `c100_arr` | timestamp | UTC | Time when the arriving flight last crossed, inbound, a circle of 100 nautical miles (NM) radius around the destination airport. NULL when no such crossing was detected. |
| `c110_arr` | timestamp | UTC | Time when the arriving flight last crossed, inbound, a circle of 110 nautical miles (NM) radius around the destination airport. NULL when no such crossing was detected. |
| `c120_arr` | timestamp | UTC | Time when the arriving flight last crossed, inbound, a circle of 120 nautical miles (NM) radius around the destination airport. NULL when no such crossing was detected. |
| `c40_dep` | timestamp | UTC | Time when the departing flight first crossed, outbound, a circle of 40 nautical miles (NM) radius around the departure airport. NULL when no such crossing was detected. |
| `c50_dep` | timestamp | UTC | Time when the departing flight first crossed, outbound, a circle of 50 nautical miles (NM) radius around the departure airport. NULL when no such crossing was detected. |
| `c60_dep` | timestamp | UTC | Time when the departing flight first crossed, outbound, a circle of 60 nautical miles (NM) radius around the departure airport. NULL when no such crossing was detected. |
| `c100_dep` | timestamp | UTC | Time when the departing flight first crossed, outbound, a circle of 100 nautical miles (NM) radius around the departure airport. NULL when no such crossing was detected. |
| `c110_dep` | timestamp | UTC | Time when the departing flight first crossed, outbound, a circle of 110 nautical miles (NM) radius around the departure airport. NULL when no such crossing was detected. |
| `c120_dep` | timestamp | UTC | Time when the departing flight first crossed, outbound, a circle of 120 nautical miles (NM) radius around the departure airport. NULL when no such crossing was detected. |
| `track_duration_min` | double | min | Time between first_seen and last_seen, in minutes. Very short tracks are often pieces of a flight seen on the ground rather than whole flights. No row is removed on this value, so users can choose their own limit. |
| `superseded_dep` | boolean |  | True when this row is a piece of a departure that is already counted on another row: it has no take-off, and the same aircraft departs again from the same airport within 3 hours (10,800 s). False otherwise. |
| `superseded_arr` | boolean |  | True when this row is a piece of an arrival that is already counted on another row: it has no landing, and the same aircraft arrived at the same airport within the 3 hours (10,800 s) before. False otherwise. |
| `movement_dep` | boolean |  | True when this row counts as a departure: the row names an airport, it is not a fragment counted on another row, and there is evidence that the aircraft flew (a detected runway time, an airport at the other end, or a crossing of one of the distance circles). Use this column to count departures. |
| `movement_arr` | boolean |  | True when this row counts as an arrival: the row names an airport, it is not a fragment counted on another row, and there is evidence that the aircraft flew (a detected runway time, an airport at the other end, or a crossing of one of the distance circles). Use this column to count arrivals. |
| `version` | string |  | Version of the method that built this flight row, for example v5.0.0. It changes only when that method changes. |
| `release` | string |  | Name of the data release the row belongs to (v2.0.0). It is the same on every row of a release. |
