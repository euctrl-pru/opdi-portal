<!-- Written by tools/gen_item_figures.py from assets/accuracy/item_accuracy.csv; do not edit. -->

| What | Kind | Value | Out of | Label | Compared with |
|---|---|---|---|---|---|
| Departures with an AOBT | availability | 17.5 % | 363,000 | low | OPDI flight events |
| Recorded off-blocks with an AOBT | availability | 35.0 % | 57,340 | low | airport operator records (Europe) |
| AOBT within 60 s of the record | accuracy | 40.3 % | 20,080 | limited | airport operator records (Europe) |
| AOBT: median difference from the record | accuracy | 99 s | 20,080 |  | airport operator records (Europe) |
| Arrivals with an AIBT | availability | 19.2 % | 331,100 | low | OPDI flight events |
| Recorded in-blocks with an AIBT | availability | 33.4 % | 57,550 | low | airport operator records (Europe) |
| AIBT within 60 s of the record | accuracy | 72.0 % | 19,230 | fair | airport operator records (Europe) |
| AIBT: median difference from the record | accuracy | 34 s | 19,230 |  | airport operator records (Europe) |
| AOBT naming the recorded stand | accuracy | 61.9 % | 14,740 | fair | airport operator records (Europe) |
| AIBT naming the recorded stand | accuracy | 74.1 % | 14,220 | fair | airport operator records (Europe) |

: Three days together (10, 14 and 17 June 2026). "Out of" is the number of flights, records or compared values the share is computed over, to four significant figures. {.striped}

Per airport: at the 100 busiest European airports ([off-block times](../accuracy/europe.qmd#off-block), [in-block times](../accuracy/europe.qmd#in-block)) and at the 100 busiest airports worldwide ([off-block times](../accuracy/world.qmd#off-block), [in-block times](../accuracy/world.qmd#in-block)).
