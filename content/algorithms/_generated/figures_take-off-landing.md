<!-- Written by tools/gen_item_figures.py from assets/accuracy/item_accuracy.csv; do not edit. -->

| What | Kind | Value | Out of | Label | Compared with |
|---|---|---|---|---|---|
| Departures with an ATOT | availability | 50.8 % | 363,000 | partial | OPDI flight events |
| Recorded take-offs with an ATOT | availability | 80.8 % | 57,340 | high | airport operator records (Europe) |
| ATOT within 60 s of the record | accuracy | 93.2 % | 46,350 | good | airport operator records (Europe) |
| ATOT: median difference from the record | accuracy | 21 s | 46,350 |  | airport operator records (Europe) |
| ATOT: 90th percentile of the difference | accuracy | 56 s | 46,350 |  | airport operator records (Europe) |
| Arrivals with an ALDT | availability | 65.3 % | 331,100 | partial | OPDI flight events |
| Recorded landings with an ALDT | availability | 81.7 % | 57,550 | high | airport operator records (Europe) |
| ALDT within 60 s of the record | accuracy | 94.5 % | 47,010 | good | airport operator records (Europe) |
| ALDT: median difference from the record | accuracy | 18 s | 47,010 |  | airport operator records (Europe) |
| ALDT: 90th percentile of the difference | accuracy | 51 s | 47,010 |  | airport operator records (Europe) |
| ATOT from the runway pass (method acdm) within 60 s | accuracy | 94.3 % | 27,580 | good | airport operator records (Europe) |
| ATOT from the runway window (method legacy) within 60 s | accuracy | 91.5 % | 18,770 | good | airport operator records (Europe) |
| ALDT from the runway pass (method acdm) within 60 s | accuracy | 97.1 % | 33,760 | good | airport operator records (Europe) |
| ALDT from the runway window (method legacy) within 60 s | accuracy | 87.9 % | 13,260 | good | airport operator records (Europe) |
| ATOT within 60 s of Network Manager, at airports without records | accuracy | 85.5 % | 10,340 | good | Network Manager flight records (Europe) |
| ALDT within 60 s of Network Manager, at airports without records | accuracy | 83.1 % | 13,110 | good | Network Manager flight records (Europe) |
| Take-off runway equal to the record | accuracy | 97.0 % | 46,350 | good | airport operator records (Europe) |
| Landing runway equal to the record | accuracy | 96.6 % | 47,010 | good | airport operator records (Europe) |

: Three days together (10, 14 and 17 June 2026). "Out of" is the number of flights, records or compared values the share is computed over, to four significant figures. {.striped}

Per airport: at the 100 busiest European airports ([take-off times](../accuracy/europe.qmd#take-off), [landing times](../accuracy/europe.qmd#landing)) and at the 100 busiest airports worldwide ([take-off times](../accuracy/world.qmd#take-off), [landing times](../accuracy/world.qmd#landing)).
