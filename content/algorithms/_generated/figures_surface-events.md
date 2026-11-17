<!-- Written by tools/gen_item_figures.py from assets/accuracy/item_accuracy.csv; do not edit. -->

| What | Kind | Value | Out of | Label | Compared with |
|---|---|---|---|---|---|
| entry-taxiway events, per day | count | 579,900 |  |  | OPDI flight events |
| entry-parking_position events, per day | count | 152,300 |  |  | OPDI flight events |
| entry-parking_position within 60 s of the recorded in-block (nearest per record) | accuracy | 79.0 % | 16,660 | fair | airport operator records (Europe) |
| exit-parking_position within 60 s of the recorded off-block (nearest per record) | accuracy | 32.0 % | 17,780 | limited | airport operator records (Europe) |

: Three days together (10, 14 and 17 June 2026). "Out of" is the number of flights, records or compared values the share is computed over, to four significant figures. {.striped}

Per airport: every item at the [100 busiest European airports](../accuracy/europe.qmd#all-items) and the [100 busiest airports worldwide](../accuracy/world.qmd#all-items), as files to download.
