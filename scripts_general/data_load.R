# LOADING DATASETS FOR ALL STUDIES

# study 1 -----
d1 <- read_csv("../data/sense_spirit_study1.csv") %>%
  mutate(country = factor(country, levels = levels_country),
         site = factor(site, levels = levels_site),
         religion = factor(religion, levels = levels_religion),
         researcher = factor(researcher, levels = levels_researcher))

contrasts(d1$country) <- contrasts_country
contrasts(d1$site) <- contrasts_site
contrasts(d1$religion) <- contrasts_religion


# study 2 -----
d2 <- read_csv("../data/sense_spirit_study2.csv") %>%
  mutate(country = factor(country, levels = levels_country),
         religion = factor(religion, levels = c("charismatic", "general population")))

contrasts(d2$country) <- contrasts_country
contrasts(d2$religion) <- contrasts_religion


# study 3 -----
d3 <- read_csv("../data/sense_spirit_study3.csv") %>%
  mutate(country = factor(country, levels = levels_country))

contrasts(d3$country) <- contrasts_country


# study 4 -----
d4 <- read_csv("../data/sense_spirit_study4.csv") %>%
  mutate(country = factor(country, levels = levels_country))

contrasts(d4$country) <- contrasts_country

