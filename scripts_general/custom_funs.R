# REGRESSION -----

# function for modifying stargazer tables (modifying interaction symbol)
sub_interact_fun <- function(str) {
  str <- gsub("\\`", "", str) # for `Porosity Vignettes`, etc.
  str <- gsub("\\.\\:", "\\. × ", str) # for country
  str <- gsub("\\:Cou", " × Cou", str)
  str <- gsub("\\:Sit", " × Sit", str)
  str <- gsub("\\:Rel", " × Rel", str)
  str <- gsub("\\:Pop", " × Pop", str)
  str <- gsub("\\:Por", " × Por", str)
  str <- gsub("\\:Abs", " × Abs", str)
  str <- gsub("\\:Gen", " × Gen", str)
  str <- gsub("\\:Age", " × Age", str)
  str <- gsub("\\:Edu", " × Edu", str)
  str <- gsub("\\:Pre", " × Pre", str)
  return(str)
}

# function for printing out rsq values for lme4 models
rsq_table_fun <- function(model_list) {
  tab <- rsquared(modelList = model_list) %>%
    mutate_at(vars(Marginal, Conditional), ~ round(., 2)) %>%
    rownames_to_column("model") %>%
    mutate(model = paste0("(", model, ")")) %>%
    select(model, Marginal, Conditional) %>%
    gather(`R-squared Type`, rsq, -model) %>%
    spread(model, rsq) %>%
    arrange(desc(`R-squared Type`)) %>%
    kable() %>%
    kable_styling()
  
  return(tab)
}


# SCORING AND RELIABILITY -----

# function for calculating Cronbach's alpha
alpha_fun <- function(df, which_vars, which_country, which_keys = NULL,
                      which_use = NULL){
  
  if (which_country != "ALL") {
    df0 <- df %>% filter(country == which_country)
  } else {
    df0 <- df
  }
  
  df0 <- df0 %>% select(!!which_vars)
  
  res <- psych::alpha(df0, keys = which_keys, use = "pairwise")
  res_alpha <- res$total["raw_alpha"] %>% as.numeric()
  
  return(res_alpha)  
}


# function for scoring scales after omitting items
score_fun <- function(df, var_omit = NA, 
                      var_group = c("country", "subject_id")){
  
  if (!is.na(var_omit)) {
    df0 <- df %>% select(-!!var_omit)
  } else {
    df0 <- df
  }
  
  df0 <- df0 %>%
    gather(question, response, -!!var_group) %>%
    group_by_at(var_group) %>%
    summarise(score = mean(response, na.rm = T)) %>%
    ungroup()
  
  return(df0)
  
}


# function for getting ICC stat
icc_fun <- function(df, var_name = NA, 
                    var1 = "response", var2 = "recoded",
                    which_model = "oneway", which_type = "consistency",
                    which_unit = "single") {
  
  df0 <- df %>%
    filter(question == var_name) %>%
    select_at(c(var1, var2))
  
  res <- irr::icc(df0, model = which_model, type = which_type, unit = which_unit)
  
  icc <- res$value
  
  return(icc)
  
}


