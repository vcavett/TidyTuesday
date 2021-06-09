library(tidyverse)
library(tidytuesdayR)

data <- tidytuesdayR::tt_load(2021, week = 24)
stocked <- data$stocked
fishing <- data$fishing

fishing %>% 
	mutate(species = str_to_title(species)) %>% 
	filter(grepl("Salmon", species)==TRUE) %>% 
	filter(grand_total > 0) %>% 
	mutate(decade = substr(year, start = 1, stop = 3),
				 decade = paste(decade, "0's", sep = "")) %>% 
	ggplot() +
	aes(species, grand_total, fill = decade) +
	geom_col(drop = TRUE, 
					 position = position_stack(reverse = TRUE)) +
	scale_y_log10() +
	facet_wrap(~region, scales = "free", drop = TRUE) +
	coord_flip() +
	theme_bw()

fishing %>% 
	mutate(species = str_to_title(species)) %>% 
	filter(grepl("Pacific Salmon", species)==TRUE) %>% 
	filter(grepl("Canada", region)==FALSE) %>% 
	filter(grepl("MI", region) == TRUE) %>% 
	mutate(decade = substr(year, start = 1, stop = 3),
				 decade = paste(decade, "0's", sep = "")) %>% 
	ggplot() +
	aes(year, grand_total, color = region) +
	geom_point() +
	theme_bw()
