library(tidyverse)
library(magrittr)
library(lubridate)
library(dtplyr)
library(slider)

initialize_git_tokens <- function() {
  git_keyring <- "git_access"
  if (keyring::keyring_is_locked(git_keyring)) {
    keyring::keyring_unlock(git_keyring)
  }
  Sys.setenv(GITHUB_PAT = keyring::key_get("GITHUB_PAT", keyring = git_keyring))
  Sys.setenv(JG_GITLAB_PAT = keyring::key_get("jg-gitlab", "jonathan",
                                              keyring = git_keyring))
}

update_repos <- function() {
  github_cred <- git2r::cred_token("GITHUB_PAT")
  if (! dir.exists("data/hopkins")) {
    git2r::clone("https://github.com/CSSEGISandData/COVID-19.git",
                 "data/hopkins", progress = FALSE, credentials = github_cred)
  } else {
    git2r::pull(git2r::repository("data/hopkins/"), credentials = github_cred)
  }
  if (! dir.exists("data/nytimes")) {
    git2r::clone("https://github.com/nytimes/covid-19-data.git",
                 "data/nytimes", progress = FALSE, credentials = github_cred)

  } else {
    git2r::pull(git2r::repository("data/nytimes/"), credentials = github_cred)
  }
}

load_data <- function(init_globals = FALSE, quiet = FALSE) {
  if (! quiet) message("Loading data ...")
  us_cases <- read_csv("data/hopkins/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  us_deaths <- read_csv("data/hopkins/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
  if (! quiet) message("... done loading data ")

  process_data <- function(src, var = c("cases", "deaths")) {
    var <- match.arg(var)
    svar <- ensym(var)
    new_var <- str_c("new_", var)

    if (! quiet) message("Processing national data...")
    df <- src %>%
      select(state = Province_State,
             matches("[0-9]+/[0-9]+/[0-9]+")) %>%
      mutate(state = factor(state)) %>%
      pivot_longer(-state, names_to = "date", values_to = var) %>%
      lazy_dt() %>%
      mutate(date = mdy(date)) %>%
      group_by(state, date) %>%
      summarize_all(~sum(., na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(state, date) %>%
      group_by(state) %>%
      mutate(!!new_var := !!svar - lag(!!svar)) %>%
      ungroup() %>%
      as_tibble()
    if (! quiet) message("... done processing national data.")
    invisible(df)
  }

  process_county_data <- function(src, var = c("cases", "deaths")) {
    var <- match.arg(var)
    svar <- ensym(var)
    new_var <- str_c("new_", var)

    if (! quiet) message("Processing county data...(",
                         str_c(dim(src), collapse = ", "), ")")
    df <- src %>%
      select(county = Admin2, state = Province_State, GEOID = FIPS,
             matches("[0-9]+/[0-9]+/[0-9]+")) %>%
      mutate_at(vars("state", "county"), factor) %>%
      pivot_longer(c(-state, -county, -GEOID),
                   names_to = "date", values_to = var) %>%
      mutate(date = mdy(date),
             GEOID = GEOID %>% as.integer() %>% sprintf("%05d", .) %>%
               factor()) %>%
      lazy_dt() %>%
      group_by(state, county, GEOID, date) %>%
      summarize_all(~sum(., na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(state, county, GEOID, date) %>%
      group_by(state, county, GEOID) %>%
      mutate(!!new_var := !!svar - lag(!!svar)) %>%
      ungroup() %>%
      as_tibble()
    if (! quiet) message("... done processing county data.")
    invisible(df)
  }

  if (! quiet) message("Preparing US data...")
  cases <- process_data(us_cases, "cases")
  deaths <- process_data(us_deaths, "deaths")
  us_covid <- full_join(cases, deaths, by = c("state", "date")) %>%
    arrange(state, date)
  if (! quiet) message("Done preparing US data.")


  if (! quiet) message("Preparing county data...")
  county_cases <- process_county_data(us_cases, "cases")
  county_deaths <- process_county_data(us_deaths, "deaths")
  us_covid_county <- full_join(county_cases, county_deaths,
                               by = c("state", "county", "GEOID", "date")) %>%
    arrange(state, county, GEOID, date)
  if (! quiet) message("Done preparing county data.")

  if (init_globals) {
    assign("us_covid", us_covid, envir = globalenv())
    assign("us_covid_county", us_covid_county, envir = globalenv())
  }
  invisible(list(us_covid = us_covid, us_covid_county = us_covid_county))
}

startup <- function() {
  initialize_git_tokens()
  update_repos()
  load_data(TRUE)
}

check_state <- function(state) {
  if (missing(state) || is.null(state) || is.na(state) ||
      length(state) == 1 && str_to_upper(state) == "USA") {
    state <- "USA"
  } else {
    abbs <- ! str_to_title(state) %in% state.name &
      str_to_upper(state) %in% state.abb
    if (any(abbs)) {
      state[abbs] <- map_chr(str_to_upper(state[abbs]),
                             ~state.name[state.abb == .x])
    }
  }
  state
}

oxford_c <- function(x) {
  if (length(x) > 2) {
    res <- str_c(head(x, -1), collapse = ", ") %>%
      str_c(tail(x, 1), sep = ", and ")
  } else {
    res <- str_c(x, collapse = " and ")
  }
  res
}


select_data <- function(state = NULL, county = NULL, complement = FALSE) {
  state <- check_state(state)
  if (length(state) == 1 && str_to_upper(state) == "USA") {
    df <- us_covid
    attr(df, "loc") <- "the United States"
    return(invisible(df))
  }

  if (length(state) > 2) {
    state_names <- state.abb[state.name %in% state]
  } else {
    state_names <- state
  }
  state_names <- sort(state_names)
  if (! is.null(county)) {
    county <- str_to_title(county) %>% sort()
    if (length(state_names) > 1) {
      loc <- str_c(county, " County, ", state_names) %>%
        oxford_c()
    } else {
      if (length(county > 2)) {
        state_names <- state.abb[state.name %in% state] %>% sort()
      }
      loc <- oxford_c(county) %>%
        str_c(ifelse(length(county) > 1, "Counties", "County"), sep = " ") %>%
        str_c(state_names, sep = ", ")
    }
  } else {
    loc <- oxford_c(state_names)
  }
  if (complement) {
    loc <- str_c("Except ", loc)
  }

  if (! all(state %in% us_covid$state)) {
    stop("Bad state names: ", str_c(setdiff(state, us_covid$state),
                                    collapse = ", "))
  }

  if (is.null(county)) {
    if (complement) {
      df <- us_covid %>% filter(! state %in% !!state)
    } else {
      df <- us_covid %>% filter(state %in% !!state)
    }
  } else {
    if (complement) {
      df <- us_covid_county %>% filter(! state %in% !!state)
    } else {
      df <- us_covid_county %>% filter(state %in% !!state)
    }
    counties <- df %>% pull(county) %>% unique() %>% sort()
    if (! all(county %in% counties)) {
      stop("Bad county names: ", str_c(setdiff(county, counties),
                                       collapse = ", "))
    }
    df <- df %>% filter(county %in% !!county)
  }
  attr(df, "loc") <- loc
  invisible(df)
}

rural_data <- function(state = NULL, urban = FALSE) {
  state <- check_state(state)
  if (! exists(".rural_counties", envir = globalenv()) ||
      ! is.environment(globalenv()$.rural_counties)) {
    .rural_counties <- new.env(parent = globalenv())
    assign(".rural_counties", .rural_counties, envir = globalenv())
  }
  if (! exists("rural_counties", envir = .rural_counties)) {
    rural_counties <- tidycensus::get_decennial("county",
                                                c("P002001", "P002002", "P002005"),
                                                cache_table = TRUE,
                                                cache = TRUE,
                                                year = 2010) %>%
      pivot_wider(names_from = "variable", values_from = "value") %>%
      filter(P002005 > P002002) %>% dplyr::pull(GEOID)
    assign("rural_counties", rural_counties, envir = .rural_counties)
  }
  rural_counties <- get("rural_counties", envir = .rural_counties)
  if (urban) {
    df <- us_covid_county %>% filter(! GEOID %in% rural_counties)
  } else {
    df <- us_covid_county %>% filter(GEOID %in% rural_counties)
  }
  loc <- "rural counties"
  if (length(state) != 1 || str_to_upper(state) != "USA") {
    state <- str_to_title(state)
    df <- df %>% filter(state %in% !!state)
    if (length(state) > 1) {
      state <- state.abb[state.name %in% state]
    }
    loc <- str_c(loc, " in ", str_c(state, collapse = ", "))
  }
  attr(df, "loc") <- loc
  invisible(df)
}

summarize_weekly <- function(df) {
  last_day <- wday(tail(df$date, 1))
  delta <- 7 - last_day
  df <- df %>% mutate(wday = (wday(date) + delta - 1) %% 7 + 1,
                      wdate = date - days(wday - 3) + hours(12)) %>%
    group_by(wdate) %>%
    summarize(cases = sum(cases, na.rm = TRUE),
              new_cases = sum(new_cases, na.rm = TRUE),
              deaths = sum(deaths, na.rm = TRUE),
              new_deaths = sum(new_deaths, na.rm = TRUE),
              days = n(),
              end_date = max(date),
              .groups = "drop") %>%
    mutate(wday = wday(wdate), end_wday = wday(end_date)) %>%
    rename(date = wdate)

}

plot_time_series <- function(df, var = c("cases", "deaths"),
                             type = c("bar", "line"),
                             filter_len = 7,
                             align = c("right", "center", "left"),
                             weekly = FALSE,
                             clamp_zero = TRUE, loc = NULL) {
  var <- match.arg(var)
  filter_align <- match.arg(align)
  type <- match.arg(type)

  if (weekly && ! is.null(filter_len) && ! is.na(filter_len) &&
      filter_len <= 7) {
    filter_len <- NA
  }

  svar <- ensym(var)
  nvar <- str_c("new_", var)
  snvar <- ensym(nvar)

  if (is.null(loc) && ("loc" %in% names(attributes(df)))) {
    loc <- attr(df, "loc", TRUE)
  }

  filtered_var <- str_c("smooth_", var)
  sf_var <- ensym(filtered_var)

  raw_label <- "Raw count"
  sf_label <- str_c(filter_len, "-day average")

  if (clamp_zero) {
    df <- df %>% mutate(new_deaths = pmax(0, new_deaths),
                        new_cases = pmax(0, new_cases))
  }

  if (var == "cases") {
    lab_var <- str_to_sentence(str_c("New", var, sep = " "))
    title_var <- str_c("New COVID-19", var, sep = " ")
  } else {
    lab_var <- str_to_sentence(str_c(ifelse(weekly, "Weekly", "Daily"),
                                     var, sep = " "))
    title_var <- str_c("COVID-19", var, sep = " ")
  }

  if (is.null(loc) || loc == "") {
    loc <- ""
  } else {
    loc <- str_c(" in ", loc)
  }
  title_str <- str_c(title_var, loc, " by ", ifelse(weekly, "week", "day"))

  df <- df %>% group_by(date) %>%
    summarize(across(c(cases, deaths, new_cases, new_deaths),
                     ~sum(., na.rm = TRUE)),
              .groups = "drop")

  if (weekly) {
    df <- summarize_weekly(df)
  }

  df <- df %>% mutate(date = as_datetime(date))

  if (! is.na(filter_len) && filter_len < 2) {
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

  if (! is.na(filter_len)) {
    df <- df %>%
      mutate(!!sf_var := slide_dbl(!!snvar, mean, .before = before,
                                   .after = after, .complete = TRUE)) %>%
      select(date, !!snvar, !!sf_var)

    if (type == "line") {
      df <- df %>%
        pivot_longer(-date, names_to = "filter", values_to = "count") %>%
        mutate(filter = ordered(filter, levels = c(nvar, filtered_var),
                                labels = c(!!raw_label, !!sf_label)))
      mapping <- aes(x = date, y = count, color = filter, size = filter)
      col_scale <- scale_color_manual(values = set_names(c(alpha("darkblue", 0.3),
                                                           "darkred"),
                                                         c(raw_label, sf_label)),
                                      name = NULL)
      fil_scale <- scale_fill_manual(values = set_names(c(NA, NA),
                                                        c(raw_label, sf_label)),
                                     name = NULL)
      size_scale <- scale_size_manual(values = set_names(c(0.1, 1),
                                                         c(raw_label, sf_label)),
                                      name = NULL)
      guide_extra <- guides(colour = guide_legend(override.aes = list(alpha = 1)))
    } else {
      mapping <- aes(x = date, y = !!snvar)
      col_scale <- scale_color_manual(values = set_names(c(alpha("darkblue", 0.2),
                                                           "darkred"),
                                                         c(raw_label, sf_label)),
                                      name = NULL)
      fil_scale <- scale_fill_manual(values = set_names(c(alpha("darkblue", 0.1),
                                                          NA),
                                                        c(raw_label, sf_label)),
                                     name = NULL)
      size_scale <- scale_size_manual(values = set_names(c(0, 1),
                                                         c(raw_label, sf_label)),
                                      name = NULL)
      guide_extra <- guides()
    }

    theme_extra <- theme(legend.position = c(0.01, 0.99),
                         legend.justification = c(0, 1))
  } else {
    mapping <- aes(x = date, y = !!snvar)
    col_scale <- scale_color_manual(values = "darkblue", name = NULL)
    fil_scale <- scale_fill_manual(values = "darkblue", name = NULL)
    size_scale <- scale_size_manual(values = 1, name = NULL)
    theme_extra <- theme()
    guide_extra <- guides(color = "none", size = "none", fill = "none")
  }

  plot_df <<- df
  plot_mapping <<- mapping
  plot_col_scale <<- col_scale
  plot_size_scale <<- size_scale
  plot_guide_extra <<- guide_extra
  plot_title_str <<- title_str
  plot_lab_str <<- lab_var
  plot_theme <<- theme_extra
  #

  p <- ggplot(df, mapping = mapping)
  if (type == "line") {
    p <- p +
      geom_line(na.rm = TRUE)
  } else {
    if (weekly) {
      p <- p +
        geom_col(aes(fill = !!raw_label, color = !!raw_label,
                     size = !!raw_label),
                 width = as.numeric(days(6)),
                 na.rm = TRUE)
    } else {
      p <- p +
        geom_col(aes(fill = !!raw_label, color = !!raw_label,
                     size = !!raw_label),
                 na.rm = TRUE)
    }
    if (! is.na(filter_len)) {
      p <- p +
        geom_line(aes(y = !!sf_var, color = !!sf_label, fill = !!sf_label,
                      size = !!sf_label),
                  na.rm = TRUE)
    }
  }
  p <- p +
    col_scale + size_scale
  if (type == "bar") {
    p <- p + fil_scale
  }

  p <- p +
    guide_extra +
    labs(x = "Date", y = lab_var, title = title_str) +
    theme_bw() +
    theme_extra
  p <- adjust_scale(p, day = ifelse(weekly, tail(df$end_wday,1), 1))
  p
}

adjust_scale <- function(p, start = ymd("2020-03-01"), day = 1, skip = 1) {
  if (is.character(start))
    start <- ymd(start)
  start <- as_datetime(start)
  end <- ymd("2020-12-31") %>% as_datetime()
  b <- seq(start, end, by = "day") %>%
    keep(~wday(.x) == day)
  if (skip < 1) {
    mb <- NULL
  } else {
    mb <- b
  }
  b <- b %>% keep(., (seq_along(.) - 1) %% (skip + 1) == 0)
  if (! is.null(mb)) {
    mb <- setdiff(mb, b) %>% as_datetime()
  }

  plot_breaks <<- b
  plot_minor_breaks <<- mb

  p + scale_x_datetime(limits = c(start, NA),
                       breaks = b, minor_breaks = mb,
                       date_labels = "%b %d")
}
