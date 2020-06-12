deprive <- function (raw_score){
  raw_score <- raw_score * 4
  cats_score <- ordered(cut(raw_score ,breaks=c(0,1.001,2.001,3.001,4.001),
                            labels=factor(c("Most deprived","Deprived","Somewhat deprived","Least deprived")),right = FALSE))
  return(cats_score)}


# Initialise easy summary function

summarise_dim_score <- function (score, df = data, factorise = TRUE, by_province = FALSE) {
  if (factorise) {
    df$fact_score <- deprive(pull(data[score]))
  } else {
    df$fact_Score <- pull(data[score])
  }
  
  if (!by_province) {
    df %>%
      select(fact_score, Gender_ind) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(fact_score, Gender_ind) %>%
      count() %>%
      group_by(Gender_ind) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = fact_score, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      scale_x_discrete(limits = rev(levels(data$fact_score))) +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 2), vjust = -1))
  } else if (by_province) {
    df %>%
      select(fact_score, Gender_ind, Province_ind) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(fact_score, Gender_ind, Province_ind) %>%
      count() %>%
      group_by(Gender_ind, Province_ind) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = fact_score, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      scale_x_discrete(limits = rev(levels(data$fact_score))) +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 2), vjust = -1)) +
      facet_grid(cols = vars(Province_ind))
  }
}


# Initialise item score summary
summarise_item <- function(x, recode_string, by_province = FALSE) {
  if(class(recode_string) == "character") {
    data$x_recode <- car::recode(pull(data[,x]), recode_string)
  } else {
    data$x_recode <- pull(data[,x])
  }
  
  if(!by_province) {
    data %>%
      select(x_recode, Gender_ind) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(x_recode, Gender_ind) %>%
      count() %>%
      group_by(Gender_ind) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = x_recode, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 2), vjust = -1))
  } else if (by_province) {
    data %>%
      select(x_recode, Gender_ind, Province_ind) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(x_recode, Gender_ind, Province_ind) %>%
      count() %>%
      group_by(Gender_ind, Province_ind) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = x_recode, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 2), vjust = -1)) +
      facet_grid(cols = vars(Province_ind))
  }
}



summarise_dim_scores <- function (score1, score2, df = data, factorise = TRUE, by_province = FALSE) {
  if (factorise) {
    df$fact_score1 <- deprive(pull(data[score1]))
  } else {
    df$fact_score1 <- pull(data[score1])
  }
  
  if (factorise) {
    df$fact_score2 <- deprive(pull(data[score2]))
  } else {
    df$fact_Score2 <- pull(data[score2])
  }
  
  if (!by_province) {
    df %>%
      select(fact_score1, Gender_ind, fact_score2) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(fact_score1, fact_score2, Gender_ind) %>%
      count() %>%
      group_by(Gender_ind, fact_score2) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = fact_score1, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      scale_x_discrete(limits = rev(levels(data$fact_score))) +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 2), vjust = -1)) +
      facet_grid(cols = vars(fact_score2))
    
  } else if (by_province) {
    df %>%
      select(fact_score1, fact_score2, Gender_ind, Province_ind) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(fact_score1, fact_score2, Gender_ind, Province_ind) %>%
      count() %>%
      group_by(Gender_ind, Province_ind, fact_score2) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = fact_score1, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      scale_x_discrete(limits = rev(levels(data$fact_score))) +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 2), vjust = -1)) +
      facet_grid(cols = vars(Province_ind), rows = vars(fact_score2))
  }
}



# Initialise item score summary
summarise_items <- function(x, y, recode_string1 = "", recode_string2 = "", by_province = FALSE) {
  if(class(recode_string1) == "character" & length(recode_string1) > 0) {
    data$x_recode <- car::recode(pull(data[,x]), recode_string1)
  } else {
    data$x_recode <- pull(data[,x])
  }
  
  if(class(recode_string2) == "character" & length(recode_string2) > 0) {
    data$y_recode <- car::recode(pull(data[,y]), recode_string2)
  } else {
    data$y_recode <- pull(data[,y])
  }
  
  if(!by_province) {
    data %>%
      select(x_recode, y_recode, Gender_ind) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(x_recode, y_recode, Gender_ind) %>%
      count() %>%
      group_by(Gender_ind, y_recode) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = x_recode, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 2), vjust = -1)) +
      facet_grid(cols = vars(y_recode))
  } else if (by_province) {
    data %>%
      select(x_recode, y_recode, Gender_ind, Province_ind) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(x_recode, y_recode, Gender_ind, Province_ind) %>%
      count() %>%
      group_by(Gender_ind, y_recode, Province_ind) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = x_recode, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 2), vjust = -1)) +
      facet_grid(cols = vars(Province_ind), rows = vars(y_recode))
  }
}

creat_rank_data <- function(data) {
  rank_data <- data %>%
    select(Ranking_1, Gender_ind, Age_bracket_ind, Province_ind, Disabled_ind) %>%
    mutate(split = str_split(.$'Ranking_1', '\\|')) %>%
    unnest(split)
  
  rank_data$split <- factor(rank_data$split, levels = paste0(1:15))
  return(rank_data)
}


