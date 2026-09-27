library(tidyverse)
library(yaml)

base_raw <- "https://raw.githubusercontent.com/Duosmium/duosmium/main/data/"

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


index <- yaml::read_yaml(
  paste0(base_raw, "recents.yaml")
) |>
  unlist()


# Pennsylvania state + regional tournaments
pa_files <- index[
  str_detect(index, "_PA_(states|.*regional)_")
]


# Extra invitationals to include
patterns <- c(
  "tiger",
  "barons",
  "dick_smith",
  "pitt",
  "birdso",
  "georgia",
  "berks_county",
  "umbc",
  "umd",
  "bavf"
)


extra_files <- index[
  str_detect(
    index,
    regex(
      paste(patterns, collapse = "|"),
      ignore_case = TRUE
    )
  )
]


candidate_files <- unique(
  c(pa_files, extra_files)
)


message(
  "Checking ",
  length(candidate_files),
  " candidate files"
)

message(
  "  PA state/regional: ",
  length(pa_files)
)

message(
  "  Extra invitationals: ",
  length(extra_files)
)

normalize_yaml_rows <- function(x) {

  if (is.null(x) || length(x) == 0) {
    return(tibble())
  }

  x |>
    map(~ {
      if ("suffix" %in% names(.x)) {
        .x$suffix <- as.character(.x$suffix)
      }

      .x
    }) |>
    bind_rows()
}


get_team_results <- function(fname) {

  url <- paste0(
    base_raw,
    "results/",
    fname
  )

  yml <- tryCatch(

  suppressWarnings(
    yaml::read_yaml(url)
  ),

  error = function(e) {

    message(
      "MISSING/UNREADABLE: ",
      fname
    )

    NULL
  }
)


  if (is.null(yml)) {
    return(NULL)
  }


  if (is.null(yml$Teams)) {

    message(
      "NO TEAMS DATA: ",
      fname
    )

    return(NULL)
  }

  tryCatch({

    # Teams
    teams <- normalize_yaml_rows(
      yml$Teams
    )


    our_team <- teams |>
      filter(
        school == "Central York High School"
      )

    if (nrow(our_team) == 0) {
      return(NULL)
    }


    team_num <- our_team$number[1]

    events <- normalize_yaml_rows(
      yml$Events
    )

    trial_events <- events$name[
      !is.na(events$trial) &
        events$trial
    ]


    scored_events <- setdiff(
      events$name,
      trial_events
    )

    placings <- normalize_yaml_rows(
      yml$Placings
    ) |>
      filter(
        event %in% scored_events
      )

    totals <- placings |>
      group_by(team) |>
      summarize(
        points = sum(
          place,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      arrange(points) |>
      mutate(
        rank = row_number()
      )


    our_rank <- totals$rank[
      totals$team == team_num
    ][1]

    medal_cut <- yml$Tournament$medals %||% 0

    trophy_cut <- yml$Tournament$trophies %||% 0


    our_medals <- placings |>
      filter(
        team == team_num,
        place >= 1,
        place <= medal_cut
      ) |>
      nrow()

    tibble(

      file = fname,

      tournament =
        yml$Tournament$`short name` %||%
        yml$Tournament$name %||%
        paste(
          yml$Tournament$state,
          yml$Tournament$level
        ),

      level =
        yml$Tournament$level,

      year =
        yml$Tournament$year,

      division =
        yml$Tournament$division,

      rank =
        our_rank,

      n_teams =
        nrow(totals),

      trophy =
        !is.na(our_rank) &
        our_rank <= trophy_cut,

      event_medals =
        our_medals
    )


  }, error = function(e) {

    message(
      "ERROR PROCESSING: ",
      fname,
      " -> ",
      conditionMessage(e)
    )

    NULL
  })
}


results <- map(
  candidate_files,
  get_team_results
) |>
  compact() |>
  bind_rows() |>
  arrange(year)


dir.create(
  "data",
  showWarnings = FALSE
)


write.csv(
  results,
  "data/team-stats.csv",
  row.names = FALSE
)

message(
  "\n",
  nrow(results),
  " tournament results found for CYHS"
)

message(
  "Saved to data/team-stats.csv"
)