<!-- Written by tools/gen_data_dictionary.py from src/opdi/output/schema.py in the opdi repository. Do not edit by hand. -->

| Column | Type | Unit | Description |
|---|---|---|---|
| `flight_id` | string |  | Unique identifier of the flight. Usually it is the identifier of its track (the position reports of one aircraft under one call sign). A track that lands, stops and takes off again holds several flights: <track>-L1, <track>-L2 and so on, in time order. Removing that ending gives the identifier of the track. |
| `icao24` | string |  | ICAO 24-bit address of the aircraft's transponder, written as six hexadecimal characters. It normally stays with one aircraft. |
| `flt_id` | string |  | Call sign broadcast by the aircraft, for example DLH123. |
| `adep` | string |  | Departure airport, as its four-letter ICAO code. 'OOA' (out of area) when the track began at the edge of the covered area, so the flight departed from somewhere outside it. This occurs only in data built with European coverage. NULL when no departure airport could be identified, which includes every overflight. |
| `ades` | string |  | Destination airport, as its four-letter ICAO code. 'OOA' (out of area) when the track ended at the edge of the covered area, so the flight continued to somewhere outside it. This occurs only in data built with European coverage. NULL when no destination airport could be identified, which includes every overflight. |
| `first_seen` | timestamp | UTC | Time of the first position report of the flight received by the OpenSky Network. For a flight that is not the first of its track, the time at which the track was divided: the in-block time after the previous landing, else the midpoint of the stop. |
| `last_seen` | timestamp | UTC | Time of the last position report of the flight received by the OpenSky Network. For a flight that is not the last of its track, the time at which the track was divided (see first_seen). |
| `dof` | date | UTC | Date of the flight: the day on which its track was first seen. A day's files hold the flights of that date. The flights of a track that holds several share the date of the track, also a flight that started after midnight. |
| `file` | string |  | Path of the flight list file that holds the flight, relative to the published folder, for example flight_list/flight_list_20260614.parquet. |
