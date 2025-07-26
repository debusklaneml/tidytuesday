# new tidytuesday 2025.07.20

if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, tidylog, here, pins, janitor, tidytuesdayR)

tt_data_obj <- tidytuesdayR::tt_load(
  2025, week = 26
) 

weekly_prices <- tt_data_obj$weekly_gas_prices

weekly_diesel_prices <- tt_data_obj$weekly_gas_prices |> 
  filter(fuel == 'diesel') 

weekly_gas_prices <- tt_data_obj$weekly_gas_prices |> 
  filter(fuel == 'gasoline')

ggplot() + 
  geom_point(aes(x = date, y = price, color = 'red'), data = weekly_diesel_prices) +
  geom_point(aes(x = date, y = price), data = weekly_gas_prices)

weekly_prices |> 
  ggplot(aes(x = date, y = price)) + 
  geom_point() + 
  facet_wrap(~fuel)

# Let's inspect how much cheaper gas is than diesel

comparison <- weekly_prices |> 
  # filter(formulation == 'reformulated') |> 
  select(date, fuel, price) |> 
  group_by(date, fuel) |> 
  slice_max(fuel, with_ties = FALSE) |> 
  ungroup() |> 
  pivot_wider(
    id_cols = date,
    names_from = fuel,
    values_from = price
  ) |> 
  filter(!is.na(diesel)) |> 
  mutate(how_much_cheaper_is_gas = gasoline - diesel) 

comparison |> 
  ggplot(aes(x = date, y = how_much_cheaper_is_gas)) + 
  geom_line() + 
  geom_hline(yintercept = 0, color = 'red') + 
  ggtitle("How much cheaper has gasoline been than diesel?") + 
  theme_minimal() + 
  ylab('Gasoline - Diesel') + 
  theme(axis.title.x = element_blank())

