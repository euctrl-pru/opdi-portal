library('eurocontrol')
library(tidyverse)
df <- eurocontrol::flights_tidy(
  wef = '2024-07-01',
  til = '2024-07-02',
  include_sensitive = TRUE,
  include_military = TRUE,
  include_head = TRUE) %>%
  collect()

df %>%
  filter(ID %in% c(273722160, 273727675)) %>%
  View()

?eurocontrol::flights_tidy
