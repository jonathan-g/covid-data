library(tidyverse)
library(magrittr)
library(lubridate)
library(slider)

us_cases <- read_csv("data/hopkins/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
us_deaths <- read_csv("data/hopkins/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

process_data <- function(src, var = c("cases", "deaths")) {
  var <- match.arg(var)
  svar <- ensym(var)
  new_var <- str_c("new_", var)

  df <- src %>%
    select(state = Province_State, matches("[0-9]+/[0-9]+/[0-9]+")) %>%
    pivot_longer(-state, names_to = "date", values_to = var) %>%
    mutate(date = mdy(date)) %>%
    group_by(state, date) %>% summarize_all(sum) %>% ungroup() %>%
    arrange(state, date) %>%
    group_by(state) %>%
    mutate(!!new_var := !!svar - lag(!!svar)) %>%
    ungroup()
  invisible(df)
}

process_county_data <- function(src, var = c("cases", "deaths")) {
  var <- match.arg(var)
  svar <- ensym(var)
  new_var <- str_c("new_", var)

  df <- src %>%
    select(county = Admin2, state = Province_State, matches("[0-9]+/[0-9]+/[0-9]+")) %>%
    pivot_longer(c(-state, -county), names_to = "date", values_to = var) %>%
    mutate(date = mdy(date)) %>%
    group_by(state, county, date) %>% summarize_all(sum) %>% ungroup() %>%
    arrange(state, county, date) %>%
    group_by(state, county) %>%
    mutate(!!new_var := !!svar - lag(!!svar)) %>%
    ungroup()
  invisible(df)
}


us_covid <- process_data(us_cases, "cases") %>%
  full_join(process_data(us_deaths, "deaths"), by = c("state", "date")) %>%
  arrange(state, date)

us_covid_county <- process_county_data(us_cases, "cases") %>%
  full_join(process_county_data(us_deaths, "deaths"),
            by = c("state", "county", "date")) %>%
  arrange(state, county, date)

plot_time_series <- function(state, var = c("cases", "deaths"),
                             county = NULL,
                             filter_len = 7,
                             filter_align = c("center", "right", "left"),
                             clamp_zero = FALSE) {
  var <- match.arg(var)
  filter_align <- match.arg(filter_align)

  svar <- ensym(var)
  nvar <- str_c("new_", var)
  snvar <- ensym(nvar)

  state <- str_to_title(state)
  if (! is.null(county)) {
    county <- str_to_title(county)
    loc <- str_c(county, " County, ", state)
  } else {
    loc <- state
  }

  if (filter_len < 2) {
    filter_len <- NA
  }
  if (! is.na(filter_len)) {
    if (filter_align == "center") {
      before = as.integer(ceiling((filter_len - 1) / 2))
      after = as.integer(floor((filter_len - 1) / 2))
    } else if (filter_align == "right") {
      before = filter_len - 1
      after = 0
    } else if (filter_align == "left") {
      before = 0
      after = filter_len - 1
    }
  }

  if (var == "cases") {
    lab_var <- str_to_sentence(str_c("New", var, sep = " "))
    title_var <- str_c("New COVID-19", var, sep = " ")
  } else {
    lab_var <- str_to_sentence(str_c("Daily", var, sep = " "))
    title_var <- str_c("COVID-19", var, sep = " ")
  }


  title_str <- str_c(title_var, "in", loc, "by day",
                     sep = " ")

  filtered_var <- str_c("smooth_", var)
  sf_var <- ensym(filtered_var)

  raw_label <- "Raw count"
  sf_label <- str_c(filter_len, "-day average")

  if (is.null(county)) {
    df <- us_covid %>% filter(state == !!state)
  } else {
    df <- us_covid_county %>% filter(state == !!state, county == !!county)
  }

  if (clamp_zero) {
    df <- df %>% mutate(new_deaths = pmax(0, new_deaths),
                        new_cases = pmax(0, new_cases))
  }

  if (! is.na(filter_len)) {
    df <- df %>%
      mutate(!!sf_var := slide_dbl(!!snvar, mean, .before = before,
                                   .after = after, .complete = TRUE)) %>%
      select(date, !!snvar, !!sf_var) %>%
      pivot_longer(-date, names_to = "filter", values_to = "count") %>%
      mutate(filter = ordered(filter, levels = c(nvar, filtered_var),
                              labels = c(!!raw_label, !!sf_label)))
    mapping <- aes(x = date, y = count, color = filter, size = filter)
    col_scale <- scale_color_manual(values = set_names(c(alpha("darkblue", 0.3),
                                                         "darkred"),
                                                       c(raw_label, sf_label)),
                                    name = NULL)
    size_scale <- scale_size_manual(values = set_names(c(0.1, 1),
                                                       c(raw_label, sf_label)),
                                    name = NULL)
    theme_extra <- theme(legend.position = c(0.01, 0.99),
                         legend.justification = c(0, 1))
    guide_extra <- guides(colour = guide_legend(override.aes = list(alpha = 1)))
  } else {
    mapping <- aes(x = date, y = !!snvar)
    col_scale <- scale_color_manual(values = "darkblue", name = NULL)
    size_scale <- scale_size_manual(values = 1, name = NULL)
    theme_extra <- theme()
    guide_extra <- guides(color = "none", size = "none")
  }

  # plot_df <<- df
  # plot_mapping <<- mapping
  # plot_col_scale <<- col_scale
  # plot_size_scale <<- size_scale
  # plot_guide_extra <<- guide_extra
  # plot_title_str <<- title_str
  # plot_lab_str <<- lab_var
  # plot_theme <<- theme_extra

  p <- ggplot(df, mapping = mapping) +
    geom_line(na.rm = TRUE) +
    col_scale + size_scale +
    guide_extra +
    labs(x = "Date", y = lab_var, title = title_str) +
    theme_bw() +
    theme_extra
  p
}
