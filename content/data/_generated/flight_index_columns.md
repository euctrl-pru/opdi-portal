<!-- Written by tools/gen_data_dictionary.py from src/opdi/output/schema.py in the opdi repository. Do not edit by hand. -->

| Column | Type | Unit | Description |
|---|---|---|---|
| `flight_id` | string |  | Unique identifier of the flight. A flight is one track: the sequence of position reports of one aircraft that belong together. |
| `icao24` | string |  | ICAO 24-bit address of the aircraft's transponder, written as six hexadecimal characters. It normally stays with one aircraft. |
| `flt_id` | string |  | Call sign broadcast by the aircraft, for example DLH123. |
| `adep` | string |  | Departure airport, as its four-letter ICAO code. 'OOA' (out of area) when the track began at the edge of the covered area, so the flight departed from somewhere outside it. This occurs only in data built with European coverage. NULL when no departure airport could be identified, which includes every overflight. |
| `ades` | string |  | Destination airport, as its four-letter ICAO code. 'OOA' (out of area) when the track ended at the edge of the covered area, so the flight continued to somewhere outside it. This occurs only in data built with European coverage. NULL when no destination airport could be identified, which includes every overflight. |
| `first_seen` | timestamp | UTC | Time of the first position report of the flight received by the OpenSky Network. |
| `last_seen` | timestamp | UTC | Time of the last position report of the flight received by the OpenSky Network. |
| `dof` | date | UTC | Date of the flight: the day on which it was first seen. A day's files hold the flights of that date. |
| `file` | string |  | Path of the flight list file that holds the flight, relative to the published folder, for example flight_list/flight_list_20260614.parquet. |
