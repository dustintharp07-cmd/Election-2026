library(dplyr)
library(readr)
library(sf)
library(stringr)
library(tidyr)

data_dir <- "data/north-carolina"
raw_dir <- file.path(data_dir, "raw")
processed_dir <- file.path(data_dir, "processed")
geography_file <- file.path(data_dir, "geography", "nc_voting_precincts.geojson")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

read_results <- function(path, year) {
  if (!file.exists(path)) {
    stop("Missing official election results file: ", path)
  }

  members <- unzip(path, list = TRUE)$Name
  result_file <- members[grepl("\\.txt$", members, ignore.case = TRUE)]
  if (length(result_file) != 1L) {
    stop("Expected one results text file in ", path, "; found ", length(result_file))
  }

  read_tsv(
    unz(path, result_file),
    col_select = all_of(c(
      "County", "Precinct", "Contest Type", "Contest Name",
      "Choice", "Choice Party", "Total Votes", "Real Precinct"
    )),
    col_types = cols(
      .default = col_character(),
      `Total Votes` = col_double()
    ),
    name_repair = "minimal",
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    transmute(
      year = year,
      county = str_to_upper(str_trim(County)),
      precinct = str_to_upper(str_trim(Precinct)),
      contest_type = str_trim(`Contest Type`),
      contest_name = str_trim(`Contest Name`),
      candidate = str_trim(Choice),
      party = str_to_upper(str_trim(`Choice Party`)),
      votes = `Total Votes`,
      real_precinct = str_to_upper(str_trim(`Real Precinct`))
    )
}

results_2024 <- read_results(
  file.path(raw_dir, "results_pct_20241105.zip"),
  2024
)
results_2026 <- read_results(
  file.path(raw_dir, "results_pct_20261103.zip"),
  2026
)

us_senate <- results_2026 |>
  filter(contest_name == "US SENATE", !is.na(candidate), candidate != "") |>
  group_by(candidate, party) |>
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") |>
  mutate(share = votes / sum(votes)) |>
  arrange(desc(votes), candidate)

if (nrow(us_senate) == 0L) {
  stop("No U.S. Senate candidate rows were found in the 2026 results file.")
}

write_csv(
  us_senate,
  file.path(processed_dir, "us_senate_2026_statewide.csv")
)

precinct_candidates <- results_2026 |>
  filter(
    contest_name == "US SENATE",
    real_precinct == "Y",
    !is.na(candidate),
    candidate != ""
  ) |>
  group_by(county, precinct, candidate, party) |>
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop")

write_csv(
  precinct_candidates,
  file.path(processed_dir, "us_senate_2026_precincts.csv")
)

precinct_leaders <- precinct_candidates |>
  group_by(county, precinct) |>
  arrange(desc(votes), candidate, .by_group = TRUE) |>
  summarise(
    leader = first(candidate),
    party = first(party),
    leader_votes = first(votes),
    runner_up_votes = nth(votes, 2, default = 0),
    total_votes = sum(votes),
    .groups = "drop"
  ) |>
  mutate(
    leader_share = leader_votes / total_votes,
    margin = leader_votes - runner_up_votes
  )

if (!file.exists(geography_file)) {
  stop("Missing official precinct boundary file: ", geography_file)
}

precinct_boundaries <- st_read(geography_file, quiet = TRUE) |>
  mutate(
    join_county = str_to_upper(str_trim(county_nam)),
    join_precinct = str_to_upper(str_trim(prec_id))
  )

precinct_map <- precinct_boundaries |>
  inner_join(
    precinct_leaders,
    by = c("join_county" = "county", "join_precinct" = "precinct")
  ) |>
  select(
    county = county_nam,
    precinct = prec_id,
    leader,
    party,
    leader_votes,
    total_votes,
    leader_share,
    margin,
    geometry
  )

if (nrow(precinct_map) == 0L) {
  stop("No election precincts matched the official boundary layer.")
}

st_write(
  precinct_map,
  file.path(processed_dir, "us_senate_2026_precinct_map.geojson"),
  driver = "GeoJSON",
  delete_dsn = TRUE,
  quiet = TRUE
)

county_results <- results_2026 |>
  filter(contest_name == "US SENATE", !is.na(candidate), candidate != "") |>
  group_by(county, candidate, party) |>
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") |>
  group_by(county) |>
  arrange(desc(votes), candidate, .by_group = TRUE) |>
  summarise(
    leader = first(candidate),
    party = first(party),
    leader_votes = first(votes),
    runner_up_votes = nth(votes, 2, default = 0),
    total_votes = sum(votes),
    .groups = "drop"
  ) |>
  mutate(
    leader_share = leader_votes / total_votes,
    margin = leader_votes - runner_up_votes
  ) |>
  arrange(county)

write_csv(
  county_results,
  file.path(processed_dir, "us_senate_2026_county.csv")
)

state_senate_county <- bind_rows(results_2024, results_2026) |>
  filter(
    str_starts(contest_name, "NC STATE SENATE DISTRICT "),
    party %in% c("DEM", "REP")
  ) |>
  group_by(year, county, party) |>
  summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = c(year, party),
    values_from = votes,
    values_fill = 0,
    names_glue = "{year}_{party}"
  )

comparison <- state_senate_county |>
  mutate(
    dem_share_2024 = `2024_DEM` / (`2024_DEM` + `2024_REP`),
    dem_share_2026 = `2026_DEM` / (`2026_DEM` + `2026_REP`),
    change_pp = (dem_share_2026 - dem_share_2024) * 100
  ) |>
  transmute(
    county,
    dem_votes_2024 = `2024_DEM`,
    rep_votes_2024 = `2024_REP`,
    dem_share_2024,
    dem_votes_2026 = `2026_DEM`,
    rep_votes_2026 = `2026_REP`,
    dem_share_2026,
    change_pp
  ) |>
  arrange(county)

if (nrow(comparison) != 100L) {
  stop("Expected 100 counties in the State Senate comparison; found ", nrow(comparison))
}

write_csv(
  comparison,
  file.path(processed_dir, "state_senate_county_comparison.csv")
)

cat(
  "Processed 2026 U.S. Senate results for ",
  nrow(county_results), " counties and ",
  nrow(precinct_candidates |> distinct(county, precinct)),
  " real precincts; matched ", nrow(precinct_map),
  " precinct boundaries. Wrote outputs to ", processed_dir, ".\n",
  sep = ""
)
