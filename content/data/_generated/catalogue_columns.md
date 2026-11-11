<!-- Written by tools/gen_data_dictionary.py from src/opdi/output/schema.py in the opdi repository. Do not edit by hand. -->

| Column | Type | Unit | Description |
|---|---|---|---|
| `day` | date | UTC | Day the row describes. |
| `flights` | bigint | count | Number of rows in the day's flight list file. |
| `events` | bigint | count | Number of rows in the day's flight events file. |
| `first_event` | timestamp | UTC | Time of the earliest event in the day's flight events file. NULL when the file is empty. |
| `last_event` | timestamp | UTC | Time of the latest event in the day's flight events file. It can fall on the next day, because a flight keeps all its events in the file of the day it started. NULL when the file is empty. |
| `flight_list_bytes` | bigint | bytes | Size of the day's flight list file. |
| `flight_events_bytes` | bigint | bytes | Size of the day's flight events file. |
| `release` | string |  | Name of the data release (v2.0.0). |
| `flight_list_version` | string |  | Version of the flight list method used for the day. When several were used, they are listed separated by commas. |
| `event_versions` | array<string> |  | Versions of the event methods found in the day's flight events file. |
| `commit` | string |  | Identifier of the version of the OPDI software (the git commit) that published the day. |
| `published_at` | timestamp | UTC | Time when the day was published. |
