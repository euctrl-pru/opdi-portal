<!-- Written by tools/gen_data_dictionary.py from src/opdi/output/schema.py in the opdi repository. Do not edit by hand. -->

| Column | Type | Unit | Description |
|---|---|---|---|
| `event_id` | string |  | Identifier of the event, written <flight_id>-E<nnnn>: nnnn counts the events of the flight in time order, with at least four digits, so the ids of a flight sort in time order. An event whose flight is not in the flight list keeps the pipeline's own identifier. It identifies the event within one release. |
| `flight_id` | string |  | Identifier of the flight the event belongs to. It matches flight_id in the flight list. On a track that holds several flights, an event belongs to the flight during which it happened. The taxi-in after a landing belongs to the arriving flight up to the in-block time. |
| `type` | string |  | Event type, for example ATOT, level-start or xing-40nm. The data dictionary lists every type. |
| `event_time` | timestamp | UTC | Time at which the event happened. |
| `longitude` | double | degrees | Longitude of the aircraft at the event, in degrees (WGS 84, positive east). Rounded to 5 decimals. |
| `latitude` | double | degrees | Latitude of the aircraft at the event, in degrees (WGS 84, positive north). Rounded to 5 decimals. |
| `altitude_ft` | double | ft | Altitude of the aircraft at the event, in feet, from the barometric altitude the aircraft reports. For a flight-level crossing it is the flight level times 100. NULL for events that have no altitude. Rounded to whole feet. |
| `h3_res_5` | string |  | Cell of the H3 hexagonal grid at resolution 5 that contains the event position. Cells cover about 250 km2 each, so the column can be used to select events in a region quickly. |
| `source` | string |  | Source of the position reports, OSN for the OpenSky Network. |
| `version` | string |  | Version of the method that produced this event type, for example events_v0.5.0. Different event types can have different versions. |
| `info` | string |  | Extra details about the event, as JSON text, for example the runway or the airport. The keys depend on the event type and are listed in the data dictionary. Numbers in it with more than three decimals are cut to three decimals. |
| `flight_version` | string |  | Version of the method that built the flight's row in the flight list (the version column of the flight list). |
| `release` | string |  | Name of the data release the row belongs to (v2.0.0). It is the same on every row of a release. |
