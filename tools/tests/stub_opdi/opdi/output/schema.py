"""A small stand-in for opdi/output/schema.py, used by the tests only."""
from dataclasses import dataclass


@dataclass(frozen=True)
class Column:
    name: str
    dtype: str
    unit: str
    description: str
    source: str
    group: str


FLIGHT_LIST = (
    Column("flight_id", "string", "", "Identifier of the flight.", "ID", "identity"),
    Column("adep", "string", "", "Departure airport | ICAO code.", "ADEP", "airports"),
    Column("release", "string", "", "Data release.", "release", "release"),
)
EVENT_COLUMNS = (
    Column("event_id", "string", "", "Identifier of the event.", "id", "event"),
    Column("flight_id", "string", "", "Identifier of the flight.", "flight_id", "identity"),
)
MEASUREMENT_COLUMNS = (
    Column("distance_flown_nm", "double", "NM", "Distance flown since the start of the track.",
           "Distance flown (NM)", "measurement"),
)
FLIGHT_EVENTS = EVENT_COLUMNS + MEASUREMENT_COLUMNS + (FLIGHT_LIST[1],) + (
    Column("flight_version", "string", "", "Version of the flight list method.", "VERSION", "release"),
    FLIGHT_LIST[-1],
)
