# levels
levels_country <- c("US", "Ghana", "Thailand", "China", "Vanuatu")
levels_site <- c("urban", "rural")
levels_religion <- c("charismatic", "local")
levels_researcher <- c("JBrahinsky", "Jdulin", "VDzokoto",
                       "Faulino", "Emng", "Rsmith")

# contrasts (effect-coding)
contrasts_country <- cbind("_gh" = c(-1, 1, 0, 0, 0),
                           "_th" = c(-1, 0, 1, 0, 0),
                           "_ch" = c(-1, 0, 0, 1, 0),
                           "_vt" = c(-1, 0, 0, 0, 1))
contrasts_site <- cbind("_rural" = c(-1, 1))
contrasts_religion <- cbind("_char" = c(1, -1))
