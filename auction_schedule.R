# =============================================================================
# Schedule Formation Model: Auction-Based Collegiate Athletic Conference Scheduling
# Version: 0.1.0
# Author: Art Steinmetz
# Created: 2025-02-01
# =============================================================================
#
# This script implements an iterative auction-based model to create a seasonal
# sports schedule for a universe of 20 schools. Schools bid on away game types
# based on opponent strength and travel requirements.
#
# =============================================================================

# -----------------------------------------------------------------------------
# Load Required Libraries
# -----------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lpSolve)

# Set seed for reproducibility
set.seed(42)

# =============================================================================
# SECTION 1: AGENTS AND SEASON PARAMETERS
# =============================================================================

# Number of schools in the conference
n_schools <- 20

# Total games each school must play in a season
total_games <- 12

# Exact number of home and away games each school must have
# With 12 games total, each school plays exactly 6 home and 6 away
exact_home_games <- 6
exact_away_games <- 6

# Initial budget for each school (in tokens)
initial_budget <- 100

# -----------------------------------------------------------------------------
# Travel Cost and Time Constants
# -----------------------------------------------------------------------------
# Home games cost zero for travel
home_travel_cost <- 0

# Bus away game cost (flat rate)
bus_travel_cost <- 1500

# Plane away game cost (flat rate)
plane_travel_cost <- 7500

# Plane travel time is constant (hours) regardless of distance
plane_travel_time <- 5

plane_use_threshold <- 5 # hours by bus to trigger plane use
# Create the set of schools with IDs
schools <- tibble(
  school_id = 1:n_schools,
  school_name = paste0("School_", LETTERS[1:n_schools])
)

cat(
  "Created",
  n_schools,
  "schools with",
  total_games,
  "games each (",
  exact_home_games,
  "home,",
  exact_away_games,
  "away).\n"
)

# =============================================================================
# SECTION 2: STRENGTH AND STRENGTH BANDS
# =============================================================================

# Assign random strength scores (1, 2, or 3) to each school
# 1 = weak, 2 = moderate, 3 = strong
schools <- schools |>
  mutate(
    # Strength score: randomly assigned 1, 2, or 3
    strength = sample(1:3, n_schools, replace = TRUE)
  ) |>
  arrange(desc(strength))

# Function to calculate match strength between two schools
# Schools prefer evenly matched opponents (same strength score)
# Numeric code: 3 = equal match (diff = 0), 2 = close match (diff = 1), 1 = mismatched (diff = 2)
calculate_strength_match <- function(strength_i, strength_j) {
  diff <- abs(strength_i - strength_j)
  case_when(
    diff == 0 ~ 3L, # Equal match (same strength)
    diff == 1 ~ 2L, # Close match
    diff == 2 ~ 1L  # Mismatched
  )
}

# Create all ordered pairs of schools for band calculation
school_pairs <- expand_grid(
  school_i = schools$school_id,
  school_j = schools$school_id
) |>
  filter(school_i != school_j) |>
  left_join(
    schools |> select(school_id, strength_i = strength),
    by = c("school_i" = "school_id")
  ) |>
  left_join(
    schools |> select(school_id, strength_j = strength),
    by = c("school_j" = "school_id")
  ) |>
  mutate(
    # compute numeric code first then convert to an ordered factor
    .strength_match_code = calculate_strength_match(strength_i, strength_j),
    strength_match = factor(
      .strength_match_code,
      levels = c(1L, 2L, 3L),
      labels = c("mismatched", "close match", "equal match"),
      ordered = TRUE
    )
  ) |>
  select(-.strength_match_code)

# Visualize the distribution of school strengths
ggplot(schools, aes(x = reorder(school_name, strength), y = strength)) +
  geom_col(aes(fill = as.factor(strength))) +
  scale_fill_brewer(palette = "Set2", name = "Strength") +
  coord_flip() +
  labs(
    title = "School Strength Scores",
    subtitle = "Randomly assigned: 1 = weak, 2 = moderate, 3 = strong",
    x = "School",
    y = "Strength Score"
  ) +
  theme_minimal()

cat("\nStrength score distribution:\n")
schools |>
  count(strength) |>
  mutate(
    strength_description = case_when(
      strength == 1 ~ "Weak",
      strength == 2 ~ "Moderate",
      strength == 3 ~ "Strong"
    )
  ) |>
  print()

cat("\nMatch strength distribution:\n")
school_pairs |>
  count(strength_match) |>
  mutate(
    band_description = case_when(
      strength_match == "mismatched" ~ "Mismatched (diff = 2)",
      strength_match == "close match" ~ "Close (diff = 1)",
      strength_match == "equal match" ~ "Evenly matched (diff = 0)"
    )
  ) |>
  print()

# =============================================================================
# SECTION 3: TRAVEL TECHNOLOGY
# =============================================================================

# Generate geographic locations for schools (using random coordinates)
# Coordinates represent approximate positions on a map (e.g., US colleges)
schools <- schools |>
  mutate(
    # Latitude and longitude (simplified grid representing eastern continental US)
    lat = runif(n_schools, min = 30, max = 45),
    lon = runif(n_schools, min = -80, max = -70)
  )

# Visualize school locations on a map grid
ggplot(schools, aes(x = lon, y = lat)) +
  geom_point(aes(color = as.factor(strength), size = 3)) +
  geom_text(aes(label = school_name), hjust = -0.1, vjust = 0.5, size = 3) +
  scale_color_brewer(palette = "Set2", name = "Strength") +
  guides(size = "none") +
  labs(
    title = "School Locations",
    subtitle = "Geographic distribution of conference schools",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  ) +
  coord_fixed(ratio = 1.3) + # Approximate aspect ratio for US map
  expand_limits(x = c(-85, -65), y = c(25, 50))

cat("\nSchool locations:\n")
schools |>
  select(school_name, strength, lat, lon) |>
  print()

# Function to calculate bus travel time (in hours) based on distance
# Plane travel time is constant (defined above)
calculate_bus_travel_time <- function(lat_i, lon_i, lat_j, lon_j) {
  # Calculate Euclidean distance (simplified)
  distance <- sqrt((lat_i - lat_j)^2 + (lon_i - lon_j)^2)
  # Convert to approximate travel time (scaling factor)
  # Roughly 1 unit = 50 miles, average speed = 50 mph for bus consideration
  travel_time <- distance * 0.5 # Results in 0-15 hour range
  round(travel_time, 1)
}

# Function to get actual travel time based on travel class
# Bus: calculated from distance, Plane: constant time
calculate_actual_travel_time <- function(bus_time, travel_class) {
  if_else(travel_class == "B", bus_time, plane_travel_time)
}

# Function to calculate travel cost based on travel class
# Uses fixed costs: Bus = $1,500, Plane = $7,500
calculate_travel_cost <- function(travel_class) {
  if_else(travel_class == "B", bus_travel_cost, plane_travel_cost)
}

# Add travel information to school pairs
school_pairs <- school_pairs |>
  left_join(
    schools |> select(school_id, lat_i = lat, lon_i = lon),
    by = c("school_i" = "school_id")
  ) |>
  left_join(
    schools |> select(school_id, lat_j = lat, lon_j = lon),
    by = c("school_j" = "school_id")
  ) |>
  mutate(
    # Calculate bus travel time based on distance
    bus_travel_time = calculate_bus_travel_time(lat_i, lon_i, lat_j, lon_j),
    # Travel class: B = Bus (<=5 hours by bus), P = Plane (>5 hours by bus)
    travel_class = if_else(bus_travel_time <= 5, "B", "P"),
    # Actual travel time: bus time for bus trips, constant for plane trips
    travel_time = calculate_actual_travel_time(bus_travel_time, travel_class),
    # Travel cost: fixed costs based on travel class
    travel_cost = calculate_travel_cost(travel_class)
  )

# Visualize travel time distribution
ggplot(school_pairs, aes(x = travel_time, fill = travel_class)) +
  geom_histogram(binwidth = 0.5, color = "white") +
  scale_fill_manual(
    values = c("B" = "forestgreen", "P" = "steelblue"),
    labels = c("B" = "Bus (≤5 hrs)", "P" = "Plane (>5 hrs)")
  ) +
  labs(
    title = "Distribution of Travel Times Between Schools",
    subtitle = "Travel class determines transportation method",
    x = "Travel Time (hours)",
    y = "Count",
    fill = "Travel Class"
  ) +
  theme_minimal()

cat("\nTravel class distribution:\n")
school_pairs |>
  count(travel_class) |>
  mutate(
    class_description = case_when(
      travel_class == "B" ~ "Bus (≤5 hours)",
      travel_class == "P" ~ "Plane (>5 hours)"
    )
  ) |>
  print()

# =============================================================================
# SECTION 4: DEFINE MATCH VARIABLES AND GAME TYPES
# =============================================================================

# Define away-game types: k = (strength_match, travel_class)
# This creates 6 possible game types: (mismatched,B), (mismatched,P), (close match,B), ...
game_types <- expand_grid(
  strength_match = factor(
    c("mismatched", "close match", "equal match"),
    levels = c("mismatched", "close match", "equal match"),
    ordered = TRUE
  ),
  travel_class = c("B", "P")
) |>
  mutate(
    game_type_id = row_number(),
    type_label = paste0("Match_", as.character(strength_match), "_", travel_class)
  )

cat("\nGame Types (k = strength_match, travel_class):\n")
print(game_types)

# Assign game type to each school pair
school_pairs <- school_pairs |>
  left_join(
    game_types,
    by = c("strength_band", "travel_class")
  )

# Visualize the distribution of game types
ggplot(school_pairs, aes(x = type_label, fill = as.factor(strength_band))) +
  geom_bar() +
  scale_fill_brewer(
    palette = "Set2",
    labels = c("1" = "Mismatched", "2" = "Moderate", "3" = "Evenly Matched")
  ) +
  labs(
    title = "Distribution of Away Game Types",
    subtitle = "By strength band and travel class",
    x = "Game Type",
    y = "Count",
    fill = "Strength Band"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# SECTION 5: SCHOOL PREFERENCES AND BIDDING LANGUAGE
# =============================================================================

# Each school has preferences over game types
# Preferences are expressed as disutilities (negative values = less desirable)
#
# Key preference factors:
# - Evenly matched games (band 2-3) are preferred to mismatched (band 1)
# - Bus travel (B) is preferred to plane travel (P) due to lower cost/fatigue
# - Each school has some random variation in preferences

generate_school_preferences <- function(school_id, game_types) {
  # Base disutilities for each game type
  # Schools prefer evenly matched opponents (band 3)
  # More negative = less desirable
  # we assume all schools have the same base disutilities for simplicity
  # but this may not be true.  Some schools may have smaller budgets
  # have a strong preference for bus travel, be willing to take longer rides, etc.
  base_disutility <- tribble(
    ~strength_band , ~travel_class , ~base_value ,
                 1 , "B"           ,         -20 , # Mismatched, bus - LEAST preferred
                 1 , "P"           ,         -30 , # Mismatched, plane - LEAST preferred
                 2 , "B"           ,          -8 , # Close match, bus
                 2 , "P"           ,         -15 , # Close match, plane
                 3 , "B"           ,          -3 , # Evenly matched, bus - MOST preferred!
                 3 , "P"           ,         -10 # Evenly matched, plane - preferred
  )

  # Add random variation for this school
  school_prefs <- game_types |>
    left_join(base_disutility, by = c("strength_band", "travel_class")) |>
    mutate(
      school_id = school_id,
      # Add random variation (±3 tokens)
      disutility = base_value + round(rnorm(n(), 0, 2), 1),
      # Each school is willing to accept more evenly matched games
      # Band 3 (evenly matched) is most desired
      max_quantity = case_when(
        strength_band == 3 ~ sample(4:6, n(), replace = TRUE), # Prefer evenly matched
        strength_band == 2 ~ sample(2:4, n(), replace = TRUE), # Accept close matches
        TRUE ~ sample(0:2, n(), replace = TRUE) # Few mismatched
      )
    ) |>
    select(school_id, game_type_id, type_label, disutility, max_quantity)
  return(school_prefs)
}

# Generate preferences for all schools
school_preferences <- map_dfr(
  schools$school_id,
  ~ generate_school_preferences(.x, game_types)
)

# Visualize average preferences by game type
avg_preferences <- school_preferences |>
  group_by(type_label, game_type_id) |>
  summarise(
    avg_disutility = mean(disutility),
    avg_max_quantity = mean(max_quantity),
    .groups = "drop"
  )

ggplot(
  avg_preferences,
  aes(x = reorder(type_label, avg_disutility), y = avg_disutility)
) +
  geom_col(aes(fill = avg_disutility)) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  coord_flip() +
  labs(
    title = "Average School Preferences by Game Type",
    subtitle = "Less negative = more preferred",
    x = "Game Type",
    y = "Average Disutility (tokens)",
    fill = "Disutility"
  ) +
  theme_minimal()

# =============================================================================
# SECTION 6: INITIALIZE AUCTION STATE
# =============================================================================

# Initialize prices for each game type (start at 0)
prices <- game_types |>
  mutate(price = 0)

# Initialize budgets for each school
budgets <- tibble(
  school_id = 1:n_schools,
  budget = initial_budget
)

# Initialize the schedule matrix (all zeros = no games scheduled)
# x_{ij} = 1 means school i plays AWAY at school j
schedule_matrix <- matrix(
  0,
  nrow = n_schools,
  ncol = n_schools,
  dimnames = list(
    paste0("School_", 1:n_schools),
    paste0("School_", 1:n_schools)
  )
)

# Track iteration history for analysis
iteration_history <- tibble(
  iteration = integer(),
  game_type_id = integer(),
  type_label = character(),
  price = numeric(),
  demand = numeric(),
  supply = numeric(),
  excess_demand = numeric()
)

cat("\n=== AUCTION INITIALIZED ===\n")
cat("Initial budgets:", initial_budget, "tokens per school\n")
cat("Initial prices: 0 for all game types\n")

# =============================================================================
# SECTION 7: HELPER FUNCTIONS FOR AUCTION
# =============================================================================

# Count current games for each school
count_school_games <- function(schedule) {
  tibble(
    school_id = 1:n_schools,
    away_games = rowSums(schedule),
    home_games = colSums(schedule),
    total_games = away_games + home_games
  )
}

# Check feasibility constraints
check_feasibility <- function(schedule) {
  game_counts <- count_school_games(schedule)

  constraints <- list(
    # (a) Total games constraint
    total_games_ok = all(game_counts$total_games <= total_games),

    # (b) Minimum home games constraint
    min_home_ok = all(game_counts$home_games >= 0), # Will check at end

    # (c) Pairwise match cap (max 1 meeting between any pair)
    pairwise_ok = all(schedule + t(schedule) <= 1)
  )

  constraints$all_ok <- all(unlist(constraints))
  constraints$game_counts <- game_counts

  return(constraints)
}

# Calculate demand for each school given current prices
# VALUE = how much better a game is compared to the worst possible game
# Schools demand games where value >= price (willing to pay for value)
calculate_demand <- function(
  school_id,
  current_prices,
  current_budget,
  current_schedule,
  school_prefs
) {
  # Get this school's preferences
  prefs <- school_prefs |>
    filter(school_id == !!school_id) |>
    left_join(
      current_prices |> select(game_type_id, price),
      by = "game_type_id"
    )

  # Count current away games for this school
  current_away <- sum(current_schedule[school_id, ])
  remaining_away_needed <- exact_away_games - current_away
  remaining_away_needed <- max(0, remaining_away_needed)

  # Calculate VALUE = how much better this game is than the worst option
  # The worst game has disutility = -30 (Band1_P)
  # value = disutility - worst_disutility
  # Band3_B: value = -3 - (-30) = 27 (most valuable, highest WTP)
  # Band1_P: value = -30 - (-30) = 0 (least valuable, lowest WTP)
  worst_disutility <- -30 # The minimum (worst) disutility across all game types
  prefs <- prefs |>
    mutate(
      # Value = how much better than the worst game
      value = disutility - worst_disutility,
      # Net surplus = value - price (demand if positive)
      net_surplus = value - price,
      # Demand games where we get positive surplus, constrained by max_quantity
      demand = if_else(
        net_surplus >= 0,
        pmin(max_quantity, remaining_away_needed),
        0L
      )
    ) |>
    arrange(desc(value)) # Sort by desirability (highest value first)

  # Budget constraint: can only afford so many away games at current prices
  prefs <- prefs |>
    mutate(
      cumulative_cost = cumsum(demand * price),
      affordable = cumulative_cost <= current_budget
    )

  return(prefs)
}

# =============================================================================
# SECTION 8: ITERATIVE AUCTION PROCESS WITH LP OPTIMIZATION
# =============================================================================

# Solve the clearing problem using linear programming (lpSolve)
# This finds the optimal assignment of away games given current demands and prices
# max_games_per_round: limit total games per iteration to create scarcity
solve_clearing_problem <- function(
  demands_by_school,
  current_prices,
  current_schedule,
  school_pairs_df,
  max_games_per_round = Inf
) {
  # Get the list of potential matches (ordered pairs where i plays away at j)
  # Only include matches that haven't played yet (max 1 meeting per pair)
  potential_matches <- school_pairs_df |>
    mutate(
      current_meetings = map2_dbl(
        school_i,
        school_j,
        ~ current_schedule[.x, .y] + current_schedule[.y, .x]
      ),
      available = current_meetings < 1
    ) |>
    filter(available) |>
    select(school_i, school_j, game_type_id, type_label)

  if (nrow(potential_matches) == 0) {
    return(current_schedule)
  }

  # Number of decision variables (one for each potential away game)
  n_vars <- nrow(potential_matches)

  # Get price and utility for each potential match
  potential_matches <- potential_matches |>
    left_join(
      current_prices |> select(game_type_id, price),
      by = "game_type_id"
    ) |>
    left_join(
      demands_by_school |>
        select(school_id, game_type_id, value) |>
        distinct(),
      by = c("school_i" = "school_id", "game_type_id")
    ) |>
    mutate(value = replace_na(value, 0))

  # Objective: maximize total value (social welfare)
  # This prioritizes scheduling games that schools want most (high value = low disutility)
  # Games with high value (Band3_B = 27) will be prioritized over low value (Band1_P = 0)
  objective <- potential_matches$value

  # Constraints setup
  # We need constraints for:
  # 1. Each school plays exactly G total games (home + away)
  # 2. Each school has at least H_min home games
  # 3. Each school's away games don't exceed their demand
  # 4. Binary constraints (handled by lpSolve)

  # Count current games
  current_away <- rowSums(current_schedule)
  current_home <- colSums(current_schedule)
  current_total <- current_away + current_home

  # Calculate remaining games needed for each school
  away_games_remaining <- pmax(0, exact_away_games - current_away)
  home_games_remaining <- pmax(0, exact_home_games - current_home)
  games_needed <- total_games - current_total

  # Build constraint matrix
  constraints_list <- list()
  directions_list <- list()
  rhs_list <- list()

  # Constraint: Each unordered pair can have at most 1 game scheduled

  # This prevents both (i,j) and (j,i) from being selected in the same iteration
  unique_pairs <- potential_matches |>
    mutate(
      pair_key = paste(
        pmin(school_i, school_j),
        pmax(school_i, school_j),
        sep = "_"
      )
    ) |>
    distinct(pair_key) |>
    pull(pair_key)

  for (pk in unique_pairs) {
    pair_schools <- as.integer(strsplit(pk, "_")[[1]])
    i <- pair_schools[1]
    j <- pair_schools[2]
    # Constraint: x_{i,j} + x_{j,i} <= 1
    constraint_row <- as.numeric(
      (potential_matches$school_i == i & potential_matches$school_j == j) |
        (potential_matches$school_i == j & potential_matches$school_j == i)
    )
    if (sum(constraint_row) > 0) {
      constraints_list <- append(constraints_list, list(constraint_row))
      directions_list <- append(directions_list, "<=")
      rhs_list <- append(rhs_list, 1)
    }
  }

  # Constraint: Each school's away games <= remaining away capacity (exactly 6 away total)
  for (i in 1:n_schools) {
    constraint_row <- as.numeric(potential_matches$school_i == i)
    if (sum(constraint_row) > 0) {
      constraints_list <- append(constraints_list, list(constraint_row))
      directions_list <- append(directions_list, "<=")
      rhs_list <- append(rhs_list, away_games_remaining[i])
    }
  }

  # Constraint: Each school's home games <= remaining home capacity (exactly 6 home total)
  for (i in 1:n_schools) {
    constraint_row <- as.numeric(potential_matches$school_j == i)
    if (sum(constraint_row) > 0) {
      constraints_list <- append(constraints_list, list(constraint_row))
      directions_list <- append(directions_list, "<=")
      rhs_list <- append(rhs_list, home_games_remaining[i])
    }
  }

  # Constraint: Total games for each school <= games_needed
  for (i in 1:n_schools) {
    # Games where i is away + games where i is home
    away_row <- as.numeric(potential_matches$school_i == i)
    home_row <- as.numeric(potential_matches$school_j == i)
    constraint_row <- away_row + home_row
    if (sum(constraint_row) > 0) {
      constraints_list <- append(constraints_list, list(constraint_row))
      directions_list <- append(directions_list, "<=")
      rhs_list <- append(rhs_list, games_needed[i])
    }
  }

  # Constraint: Demand limits by game type per school
  for (i in 1:n_schools) {
    school_demand <- demands_by_school |>
      filter(school_id == i)

    for (gt in unique(potential_matches$game_type_id)) {
      demand_qty <- school_demand |>
        filter(game_type_id == gt) |>
        pull(demand)

      if (length(demand_qty) == 0) {
        demand_qty <- 0
      }

      constraint_row <- as.numeric(
        potential_matches$school_i == i &
          potential_matches$game_type_id == gt
      )
      if (sum(constraint_row) > 0 && demand_qty >= 0) {
        constraints_list <- append(constraints_list, list(constraint_row))
        directions_list <- append(directions_list, "<=")
        rhs_list <- append(rhs_list, max(1, demand_qty)) # At least 1 to allow progress
      }
    }
  }

  # Constraint: Total games scheduled this round <= max_games_per_round
  # This creates scarcity and drives up prices for desirable game types
  if (is.finite(max_games_per_round) && max_games_per_round > 0) {
    constraint_row <- rep(1, nrow(potential_matches))
    constraints_list <- append(constraints_list, list(constraint_row))
    directions_list <- append(directions_list, "<=")
    rhs_list <- append(rhs_list, max_games_per_round)
  }

  if (length(constraints_list) == 0) {
    return(current_schedule)
  }

  # Convert to matrix
  constraint_matrix <- do.call(rbind, constraints_list)
  directions <- unlist(directions_list)
  rhs <- unlist(rhs_list)

  # Solve the LP (as integer program for binary variables)
  solution <- tryCatch(
    {
      lp(
        direction = "max",
        objective.in = objective,
        const.mat = constraint_matrix,
        const.dir = directions,
        const.rhs = rhs,
        all.bin = TRUE # Binary decision variables
      )
    },
    error = function(e) {
      return(NULL)
    }
  )

  if (is.null(solution) || solution$status != 0) {
    # LP failed, return unchanged schedule
    return(current_schedule)
  }

  # Update schedule based on solution
  new_schedule <- current_schedule
  selected <- which(solution$solution == 1)

  for (idx in selected) {
    i <- potential_matches$school_i[idx]
    j <- potential_matches$school_j[idx]
    new_schedule[i, j] <- new_schedule[i, j] + 1
  }

  return(new_schedule)
}
# ===============================================
# Main auction loop
# ===============================================
run_auction <- function(
  max_iterations = 100,
  price_increment = 2,
  games_per_round = 20, # Limit games per iteration to create scarcity
  verbose = TRUE
) {
  # Reset auction state
  current_prices <- game_types |> mutate(price = 0)
  current_budgets <- tibble(school_id = 1:n_schools, budget = initial_budget)
  current_schedule <- matrix(0, nrow = n_schools, ncol = n_schools)

  iteration_log <- tibble()

  for (iter in 1:max_iterations) {
    if (verbose && iter %% 10 == 1) {
      cat("\n--- Iteration", iter, "---\n")
    }

    # -------------------------------------------------------------------------
    # Step 1: Demand Determination
    # -------------------------------------------------------------------------
    # Each school calculates their demand based on current prices and budgets

    all_demands <- map_dfr(1:n_schools, function(sid) {
      calculate_demand(
        school_id = sid,
        current_prices = current_prices,
        current_budget = current_budgets$budget[sid],
        current_schedule = current_schedule,
        school_prefs = school_preferences
      ) |>
        mutate(school_id = sid)
    })

    # Aggregate demand by game type
    aggregate_demand <- all_demands |>
      group_by(game_type_id, type_label) |>
      summarise(
        total_demand = sum(demand),
        .groups = "drop"
      )

    # -------------------------------------------------------------------------
    # Step 2: Clearing Problem - Solve using LP optimization
    # -------------------------------------------------------------------------

    previous_schedule <- current_schedule

    current_schedule <- solve_clearing_problem(
      demands_by_school = all_demands,
      current_prices = current_prices,
      current_schedule = current_schedule,
      school_pairs_df = school_pairs,
      max_games_per_round = games_per_round
    )

    games_scheduled_this_round <- sum(current_schedule) - sum(previous_schedule)

    # Update budgets based on new games scheduled
    for (i in 1:n_schools) {
      for (j in 1:n_schools) {
        if (i != j) {
          new_games <- current_schedule[i, j] - previous_schedule[i, j]
          if (new_games > 0) {
            game_type <- school_pairs |>
              filter(school_i == i, school_j == j) |>
              pull(game_type_id)

            if (length(game_type) > 0) {
              price <- current_prices$price[
                current_prices$game_type_id == game_type[1]
              ]
              current_budgets$budget[i] <- current_budgets$budget[i] -
                price * new_games
            }
          }
        }
      }
    }

    # -------------------------------------------------------------------------
    # Step 3: Calculate supply and update prices
    # -------------------------------------------------------------------------

    # Calculate games scheduled THIS ROUND by type
    games_this_round_by_type <- map_dfr(1:n_schools, function(i) {
      map_dfr(1:n_schools, function(j) {
        new_games <- current_schedule[i, j] - previous_schedule[i, j]
        if (i != j && new_games > 0) {
          school_pairs |>
            filter(school_i == i, school_j == j) |>
            select(game_type_id) |>
            mutate(count = new_games)
        } else {
          tibble()
        }
      })
    })

    # Handle empty tibble case
    if (nrow(games_this_round_by_type) == 0) {
      games_this_round_by_type <- tibble(
        game_type_id = integer(),
        scheduled_this_round = integer()
      )
    } else {
      games_this_round_by_type <- games_this_round_by_type |>
        group_by(game_type_id) |>
        summarise(scheduled_this_round = sum(count), .groups = "drop")
    }

    # Calculate excess demand = demand this round - games scheduled this round
    # If demand > scheduled, price should increase (schools wanted more than they got)
    price_updates <- aggregate_demand |>
      left_join(games_this_round_by_type, by = "game_type_id") |>
      mutate(
        scheduled_this_round = replace_na(scheduled_this_round, 0),
        excess_demand = total_demand - scheduled_this_round
      )

    # Update prices where there's excess demand
    current_prices <- current_prices |>
      left_join(
        price_updates |> select(game_type_id, excess_demand),
        by = "game_type_id"
      ) |>
      mutate(
        excess_demand = replace_na(excess_demand, 0),
        price = if_else(excess_demand > 0, price + price_increment, price)
      ) |>
      select(-excess_demand)

    # Log this iteration
    iteration_log <- bind_rows(
      iteration_log,
      price_updates |>
        mutate(iteration = iter) |>
        left_join(
          current_prices |> select(game_type_id, price),
          by = "game_type_id"
        )
    )

    # -------------------------------------------------------------------------
    # Step 4: Check termination conditions
    # -------------------------------------------------------------------------

    game_counts <- count_school_games(current_schedule)

    all_complete <- all(game_counts$total_games == total_games)
    no_excess <- all(price_updates$excess_demand <= 0)

    if (all_complete) {
      if (verbose) {
        cat("\n=== AUCTION COMPLETE: All schedules filled ===\n")
      }
      break
    }

    if (games_scheduled_this_round == 0 && iter > 20) {
      # If no progress, try greedy fill with relaxed constraints
      if (verbose) {
        cat("No progress, attempting to fill remaining games...\n")
      }

      # Try greedy fill with strict constraint first (max 1 meeting per pair)
      # If that fails, relax to allow up to 2 meetings per pair
      for (max_meetings in c(1, 2)) {
        games_before_greedy <- sum(current_schedule)

        for (i in 1:n_schools) {
          i_total <- sum(current_schedule[i, ]) + sum(current_schedule[, i])
          if (i_total >= total_games) {
            next
          }

          for (j in 1:n_schools) {
            if (i == j) {
              next
            }

            j_total <- sum(current_schedule[j, ]) + sum(current_schedule[, j])
            if (j_total >= total_games) {
              next
            }

            # Check pairwise constraint with current max_meetings threshold
            meetings <- current_schedule[i, j] + current_schedule[j, i]
            if (meetings >= max_meetings) {
              next
            }

            # Add a game if i needs more away games and j needs more home games
            i_away <- sum(current_schedule[i, ])
            j_home <- sum(current_schedule[, j])
            if (i_away < exact_away_games && j_home < exact_home_games) {
              current_schedule[i, j] <- current_schedule[i, j] + 1
              games_scheduled_this_round <- games_scheduled_this_round + 1
              if (max_meetings == 2 && verbose) {
                cat(
                  "  Relaxed constraint: added game",
                  schools$school_name[i],
                  "@",
                  schools$school_name[j],
                  "(2nd meeting)\n"
                )
              }
            }

            i_total <- sum(current_schedule[i, ]) + sum(current_schedule[, i])
            if (i_total >= total_games) break
          }
        }

        # If we made progress with strict constraint, don't relax further
        if (sum(current_schedule) > games_before_greedy) {
          break
        }
      }
    }

    if (verbose && iter %% 10 == 0) {
      cat(
        "Games scheduled:",
        sum(current_schedule),
        "/",
        n_schools * total_games / 2,
        "\n"
      )
      cat(
        "Schools complete:",
        sum(game_counts$total_games == total_games),
        "/",
        n_schools,
        "\n"
      )
    }
  }

  # Return results
  list(
    schedule = current_schedule,
    final_prices = current_prices,
    final_budgets = current_budgets,
    iteration_log = iteration_log,
    iterations_used = iter
  )
}

# Run the auction
cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("STARTING AUCTION PROCESS\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")

auction_results <- run_auction(
  max_iterations = 100,
  price_increment = 2,
  games_per_round = 20, # Limit games per round to create scarcity
  verbose = TRUE
)

# =============================================================================
# SECTION 9: ANALYZE AND VISUALIZE RESULTS
# =============================================================================

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("AUCTION RESULTS ANALYSIS\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")

# Extract final schedule
final_schedule <- auction_results$schedule

# Calculate game counts for each school
game_summary <- count_school_games(final_schedule) |>
  left_join(
    schools |> select(school_id, school_name, strength),
    by = "school_id"
  )

cat("\nGame counts per school:\n")
print(game_summary)

# Check constraints
cat("\n--- Constraint Verification ---\n")
cat("Total games per school (should be", total_games, "):\n")
cat("  Min:", min(game_summary$total_games), "\n")
cat("  Max:", max(game_summary$total_games), "\n")
cat("  All equal to 12:", all(game_summary$total_games == total_games), "\n")

cat("\nHome games per school (should be exactly", exact_home_games, "):\n")
cat("  Min:", min(game_summary$home_games), "\n")
cat("  Max:", max(game_summary$home_games), "\n")
cat("  All equal to 6:", all(game_summary$home_games == exact_home_games), "\n")

cat("\nAway games per school (should be exactly", exact_away_games, "):\n")
cat("  Min:", min(game_summary$away_games), "\n")
cat("  Max:", max(game_summary$away_games), "\n")
cat("  All equal to 6:", all(game_summary$away_games == exact_away_games), "\n")

# Check pairwise cap
pairwise_meetings <- final_schedule + t(final_schedule)
cat("\nPairwise meetings (should be <= 1):\n")
cat("  Max meetings between any pair:", max(pairwise_meetings), "\n")
pairwise_constraint_met <- all(pairwise_meetings <= 1)
cat("  All <= 1:", pairwise_constraint_met, "\n")

# Report violations if any
if (!pairwise_constraint_met) {
  cat(
    "\n  *** CONSTRAINT VIOLATION: Some school pairs play more than once ***\n"
  )
  violations <- which(pairwise_meetings > 1, arr.ind = TRUE)
  violations <- violations[violations[, 1] < violations[, 2], , drop = FALSE] # Keep unique pairs
  cat("  Number of pairs with multiple meetings:", nrow(violations), "\n")
  cat("  Violating pairs:\n")
  for (v in seq_len(min(10, nrow(violations)))) {
    i <- violations[v, 1]
    j <- violations[v, 2]
    cat(
      "    ",
      schools$school_name[i],
      "vs",
      schools$school_name[j],
      ":",
      pairwise_meetings[i, j],
      "games\n"
    )
  }
  if (nrow(violations) > 10) {
    cat("    ... and", nrow(violations) - 10, "more pairs\n")
  }
}

# =============================================================================
# TRAVEL ANALYSIS: Total Hours and Costs per School
# =============================================================================

# Calculate travel statistics for each school based on their away games
travel_by_school <- map_dfr(1:n_schools, function(i) {
  # Get all away games for this school
  away_games_info <- map_dfr(1:n_schools, function(j) {
    if (i != j && final_schedule[i, j] > 0) {
      school_pairs |>
        filter(school_i == i, school_j == j) |>
        select(school_j, travel_class, travel_time, travel_cost) |>
        mutate(games = final_schedule[i, j])
    } else {
      tibble()
    }
  })

  if (nrow(away_games_info) == 0) {
    return(tibble(
      school_id = i,
      total_away_games = 0,
      bus_games = 0,
      plane_games = 0,
      total_travel_hours = 0,
      total_travel_cost = 0
    ))
  }

  tibble(
    school_id = i,
    total_away_games = sum(away_games_info$games),
    bus_games = sum(away_games_info$games[away_games_info$travel_class == "B"]),
    plane_games = sum(away_games_info$games[
      away_games_info$travel_class == "P"
    ]),
    total_travel_hours = sum(
      away_games_info$travel_time * away_games_info$games
    ),
    total_travel_cost = sum(away_games_info$travel_cost * away_games_info$games)
  )
}) |>
  left_join(
    schools |> select(school_id, school_name, strength),
    by = "school_id"
  )

cat("\n--- Travel Analysis by School ---\n")
cat(
  "Travel costs: Home = $0, Bus away = $",
  format(bus_travel_cost, big.mark = ","),
  ", Plane away = $",
  format(plane_travel_cost, big.mark = ","),
  "\n",
  sep = ""
)
cat("Plane travel time: ", plane_travel_time, " hours (constant)\n", sep = "")
print(
  travel_by_school |>
    select(
      school_name,
      total_away_games,
      bus_games,
      plane_games,
      total_travel_hours,
      total_travel_cost
    )
)

cat("\n--- Conference-Wide Travel Summary ---\n")
cat(
  "Total travel hours (all schools):",
  sum(travel_by_school$total_travel_hours),
  "hours\n"
)
cat(
  "Total travel cost (all schools): $",
  format(sum(travel_by_school$total_travel_cost), big.mark = ","),
  "\n",
  sep = ""
)
cat(
  "Average travel hours per school:",
  round(mean(travel_by_school$total_travel_hours), 1),
  "hours\n"
)
cat(
  "Average travel cost per school: $",
  format(round(mean(travel_by_school$total_travel_cost), 0), big.mark = ","),
  "\n",
  sep = ""
)

# Visualize total travel hours by school
ggplot(
  travel_by_school,
  aes(
    x = reorder(school_name, total_travel_hours),
    y = total_travel_hours,
    fill = total_travel_cost
  )
) +
  geom_col() +
  scale_fill_viridis_c(option = "plasma", labels = scales::dollar_format()) +
  coord_flip() +
  labs(
    title = "Total Travel Hours by School",
    subtitle = "Fewer hours is preferred",
    x = "School",
    y = "Total Travel Hours (Season)",
    fill = "Travel Cost"
  ) +
  theme_minimal()

# Visualize total travel cost by school
ggplot(
  travel_by_school,
  aes(
    x = reorder(school_name, total_travel_cost),
    y = total_travel_cost,
    fill = as.factor(strength)
  )
) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_brewer(palette = "Set2", name = "Strength") +
  coord_flip() +
  labs(
    title = "Total Travel Cost by School",
    subtitle = paste0(
      "Bus: $",
      format(bus_travel_cost, big.mark = ","),
      " | Plane: $",
      format(plane_travel_cost, big.mark = ",")
    ),
    x = "School",
    y = "Total Travel Cost ($)"
  ) +
  theme_minimal()

# Travel hours vs cost scatter
ggplot(
  travel_by_school,
  aes(
    x = total_travel_hours,
    y = total_travel_cost,
    color = as.factor(strength)
  )
) +
  geom_point(size = 4) +
  geom_text(aes(label = school_name), hjust = -0.1, vjust = 0.5, size = 3) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_brewer(palette = "Set2", name = "Strength") +
  labs(
    title = "Travel Hours vs Travel Cost by School",
    subtitle = "Schools prefer lower values on both axes",
    x = "Total Travel Hours",
    y = "Total Travel Cost ($)"
  ) +
  theme_minimal() +
  expand_limits(x = max(travel_by_school$total_travel_hours) * 1.2)

# -----------------------------------------------------------------------------
# Game Matchups Map: Lines between each pairwise game
# -----------------------------------------------------------------------------

# Create a dataframe of all games played with coordinates
game_connections <- map_dfr(1:n_schools, function(i) {
  map_dfr(1:n_schools, function(j) {
    if (i < j && (final_schedule[i, j] > 0 || final_schedule[j, i] > 0)) {
      # Get coordinates for both schools
      school_i_info <- schools |> filter(school_id == i)
      school_j_info <- schools |> filter(school_id == j)

      # Determine travel class for this pair
      pair_info <- school_pairs |>
        filter(school_i == i, school_j == j) |>
        slice(1)

      tibble(
        x_start = school_i_info$lon,
        y_start = school_i_info$lat,
        x_end = school_j_info$lon,
        y_end = school_j_info$lat,
        travel_class = pair_info$travel_class,
        distance = pair_info$distance_miles
      )
    } else {
      tibble()
    }
  })
})

# Plot the map with game connections
ggplot() +
  # Draw lines for each game matchup
  geom_segment(
    data = game_connections,
    aes(
      x = x_start,
      y = y_start,
      xend = x_end,
      yend = y_end,
      color = travel_class
    ),
    alpha = 0.6,
    linewidth = 0.8
  ) +
  # Plot school locations on top
  geom_point(
    data = schools,
    aes(x = lon, y = lat, shape = as.factor(strength)),
    size = 4,
    fill = "white",
    color = "black"
  ) +
  geom_text(
    data = schools,
    aes(x = lon, y = lat, label = gsub("School_", "", school_name)),
    size = 2.5,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c("B" = "steelblue", "P" = "firebrick"),
    labels = c("B" = "Bus", "P" = "Plane"),
    name = "Travel Mode"
  ) +
  scale_shape_manual(
    values = c("1" = 21, "2" = 22, "3" = 24),
    name = "Strength"
  ) +
  labs(
    title = "Conference Schedule Map",
    subtitle = paste(
      "Lines connect schools that play each other |",
      nrow(game_connections),
      "unique matchups"
    ),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  coord_fixed(ratio = 1.2)

# -----------------------------------------------------------------------------
# Single School Map Function: Shows home/away games with directional arrows
# -----------------------------------------------------------------------------

plot_school_schedule_map <- function(
  base_school_name = "School_D",
  schedule_matrix = final_schedule,
  schools_df = schools,
  school_pairs_df = school_pairs
) {
  # Get base school info from name
  base_school <- schools_df |> filter(school_name == base_school_name)

  if (nrow(base_school) == 0) {
    stop(paste("School not found:", base_school_name))
  }

  base_school_id <- base_school$school_id
  base_name <- base_school_name

  # Create dataframe for away games (arrows pointing FROM base TO opponent)
  away_games <- map_dfr(1:nrow(schools_df), function(j) {
    if (base_school_id != j && schedule_matrix[base_school_id, j] > 0) {
      opponent <- schools_df |> filter(school_id == j)
      pair_info <- school_pairs_df |>
        filter(school_i == base_school_id, school_j == j) |>
        slice(1)

      tibble(
        x_start = base_school$lon,
        y_start = base_school$lat,
        x_end = opponent$lon,
        y_end = opponent$lat,
        game_type = "Away",
        travel_class = pair_info$travel_class,
        opponent_name = opponent$school_name
      )
    } else {
      tibble()
    }
  })

  # Create dataframe for home games (arrows pointing FROM opponent TO base)
  home_games <- map_dfr(1:nrow(schools_df), function(j) {
    if (base_school_id != j && schedule_matrix[j, base_school_id] > 0) {
      opponent <- schools_df |> filter(school_id == j)
      pair_info <- school_pairs_df |>
        filter(school_i == j, school_j == base_school_id) |>
        slice(1)

      tibble(
        x_start = opponent$lon,
        y_start = opponent$lat,
        x_end = base_school$lon,
        y_end = base_school$lat,
        game_type = "Home",
        travel_class = pair_info$travel_class,
        opponent_name = opponent$school_name
      )
    } else {
      tibble()
    }
  })

  # Combine all games
  all_games <- bind_rows(away_games, home_games)

  # Get all opponents
  opponent_ids <- unique(c(
    which(schedule_matrix[base_school_id, ] > 0),
    which(schedule_matrix[, base_school_id] > 0)
  ))
  opponents <- schools_df |> filter(school_id %in% opponent_ids)

  # Create the plot
  ggplot() +
    # Draw arrows for away games (arrow at end, pointing to opponent)
    geom_segment(
      data = all_games |> filter(game_type == "Away"),
      aes(
        x = x_start,
        y = y_start,
        xend = x_end,
        yend = y_end,
        color = game_type,
        linetype = travel_class
      ),
      arrow = arrow(
        length = unit(0.15, "inches"),
        type = "closed",
        ends = "last"
      ),
      linewidth = 1,
      alpha = 0.7
    ) +
    # Draw arrows for home games (arrow at end, pointing to base school)
    geom_segment(
      data = all_games |> filter(game_type == "Home"),
      aes(
        x = x_start,
        y = y_start,
        xend = x_end,
        yend = y_end,
        color = game_type,
        linetype = travel_class
      ),
      arrow = arrow(
        length = unit(0.15, "inches"),
        type = "closed",
        ends = "last"
      ),
      linewidth = 1,
      alpha = 0.7
    ) +
    # Plot opponent locations (size by strength)
    geom_point(
      data = opponents,
      aes(x = lon, y = lat, size = strength),
      shape = 21,
      fill = "gray80",
      color = "black"
    ) +
    # Plot base school (larger, size by strength + bonus)
    geom_point(
      data = base_school,
      aes(x = lon, y = lat, size = strength + 3),
      shape = 21,
      fill = "gold",
      color = "black",
      stroke = 2
    ) +
    scale_size_continuous(
      range = c(3, 8),
      breaks = c(1, 2, 3),
      name = "Strength"
    ) +
    # Label opponents
    geom_text(
      data = opponents,
      aes(x = lon, y = lat, label = gsub("School_", "", school_name)),
      size = 2.5,
      fontface = "bold",
      vjust = -1.5
    ) +
    # Label base school
    geom_text(
      data = base_school,
      aes(x = lon, y = lat, label = gsub("School_", "", school_name)),
      size = 4,
      fontface = "bold",
      vjust = -1.8
    ) +
    scale_color_manual(
      values = c("Home" = "forestgreen", "Away" = "firebrick"),
      name = "Game Type"
    ) +
    scale_linetype_manual(
      values = c("B" = "solid", "P" = "dashed"),
      labels = c("B" = "Bus", "P" = "Plane"),
      name = "Travel Mode"
    ) +
    labs(
      title = paste("Schedule Map:", base_name),
      subtitle = paste(
        nrow(away_games),
        "away games (red arrows out) |",
        nrow(home_games),
        "home games (green arrows in)"
      ),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "right"
    ) +
    coord_fixed(ratio = 1.2)
}

# Example: Show schedule map for a school
# Call with no parameters for School_D, or specify a school name
plot_school_schedule_map() # Uses default: School_D
plot_school_schedule_map("School_A") # For a different school

# Analyze schedule by game type
schedule_by_type <- map_dfr(1:n_schools, function(i) {
  map_dfr(1:n_schools, function(j) {
    if (i != j && final_schedule[i, j] > 0) {
      school_pairs |>
        filter(school_i == i, school_j == j) |>
        select(
          school_i,
          school_j,
          strength_band,
          travel_class,
          game_type_id,
          type_label,
          travel_time,
          travel_cost
        ) |>
        mutate(games = final_schedule[i, j])
    } else {
      tibble()
    }
  })
})

cat("\n--- Schedule Composition by Game Type ---\n")
type_summary <- schedule_by_type |>
  group_by(type_label, strength_band, travel_class) |>
  summarise(
    total_games = sum(games),
    avg_travel_time = round(mean(travel_time), 1),
    avg_travel_cost = round(mean(travel_cost), 0),
    .groups = "drop"
  )
print(type_summary)

# Visualize game type distribution
ggplot(
  type_summary,
  aes(x = type_label, y = total_games, fill = as.factor(strength_band))
) +
  geom_col() +
  scale_fill_brewer(
    palette = "Set2",
    labels = c("1" = "Mismatched", "2" = "Moderate", "3" = "Evenly Matched")
  ) +
  labs(
    title = "Final Schedule: Games by Type",
    subtitle = "Distribution of away games across strength bands and travel classes",
    x = "Game Type",
    y = "Total Games",
    fill = "Strength Band"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize final prices
ggplot(
  auction_results$final_prices,
  aes(x = type_label, y = price, fill = price)
) +
  geom_col() +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Final Auction Prices by Game Type",
    subtitle = "Higher prices indicate higher demand",
    x = "Game Type",
    y = "Price (tokens)",
    fill = "Price"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Price evolution over iterations
if (nrow(auction_results$iteration_log) > 0) {
  price_evolution <- auction_results$iteration_log |>
    select(iteration, type_label, price) |>
    distinct()

  ggplot(price_evolution, aes(x = iteration, y = price, color = type_label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d(option = "turbo") +
    labs(
      title = "Price Evolution During Auction",
      subtitle = "How prices adjusted based on demand",
      x = "Iteration",
      y = "Price (tokens)",
      color = "Game Type"
    ) +
    theme_minimal()
}

# Final budget analysis
budget_summary <- auction_results$final_budgets |>
  left_join(
    schools |> select(school_id, school_name, strength),
    by = "school_id"
  ) |>
  mutate(spent = initial_budget - budget)

cat("\n--- Budget Summary ---\n")
cat("Initial budget per school:", initial_budget, "tokens\n")
cat("Average spent:", round(mean(budget_summary$spent), 1), "tokens\n")
cat("Min remaining:", min(budget_summary$budget), "tokens\n")
cat("Max remaining:", max(budget_summary$budget), "tokens\n")

ggplot(
  budget_summary,
  aes(x = reorder(school_name, spent), y = spent, fill = strength)
) +
  geom_col() +
  scale_fill_viridis_c(option = "plasma") +
  coord_flip() +
  labs(
    title = "Tokens Spent by School",
    subtitle = "Schools with more desirable away game opportunities spent more",
    x = "School",
    y = "Tokens Spent",
    fill = "Strength"
  ) +
  theme_minimal()

# =============================================================================
# SECTION 10: CREATE READABLE SCHEDULE OUTPUT
# =============================================================================

# Convert schedule matrix to a readable format
create_readable_schedule <- function(schedule, schools_df) {
  schedule_long <- expand_grid(
    home_id = 1:n_schools,
    away_id = 1:n_schools
  ) |>
    filter(home_id != away_id) |>
    mutate(
      games = map2_dbl(away_id, home_id, ~ schedule[.x, .y]) # away team in row, home in col
    ) |>
    filter(games > 0) |>
    left_join(
      schools_df |> select(school_id, away_team = school_name),
      by = c("away_id" = "school_id")
    ) |>
    left_join(
      schools_df |> select(school_id, home_team = school_name),
      by = c("home_id" = "school_id")
    ) |>
    left_join(
      school_pairs |>
        select(school_i, school_j, strength_band, travel_class, travel_time),
      by = c("away_id" = "school_i", "home_id" = "school_j")
    ) |>
    select(
      away_team,
      home_team,
      games,
      strength_band,
      travel_class,
      travel_time
    ) |>
    arrange(away_team, home_team)

  return(schedule_long)
}

readable_schedule <- create_readable_schedule(final_schedule, schools)

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("FINAL SCHEDULE\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("\nTotal games scheduled:", sum(final_schedule), "\n")
cat("(Each game appears once: away team @ home team)\n\n")

print(readable_schedule, n = 50)

# Summary statistics
cat("\n--- Summary Statistics ---\n")
cat("Total unique matchups:", nrow(readable_schedule), "\n")
cat("Games by strength band:\n")
readable_schedule |>
  group_by(strength_band) |>
  summarise(games = sum(games)) |>
  print()

cat("\nGames by travel class:\n")
readable_schedule |>
  group_by(travel_class) |>
  summarise(
    games = sum(games),
    avg_travel_time = round(mean(travel_time), 1)
  ) |>
  print()

# =============================================================================
# SECTION 11: DETAILED WEEKLY SCHEDULE BY SCHOOL
# =============================================================================

# Create a detailed weekly schedule for each school
# Shows week number, home/away status, opponent, relative strength, travel mode, distance, time

create_weekly_schedule <- function(schedule, schools_df, school_pairs_df) {
  # First, create a list of all games from the schedule matrix
  # Each game appears twice: once as home, once as away
  all_games <- tibble()

  for (i in 1:n_schools) {
    for (j in 1:n_schools) {
      if (i != j && schedule[i, j] > 0) {
        # i is away at j (i travels to j)
        # Get travel info from school_pairs (away school perspective)
        pair_info <- school_pairs_df |>
          filter(school_i == i, school_j == j) |>
          slice(1)

        if (nrow(pair_info) > 0) {
          # Calculate distance in miles from bus_travel_time
          # bus_travel_time = distance * 0.5, so distance = bus_travel_time * 2
          # Then convert from grid units to miles (1 unit ≈ 50 miles)
          distance_miles <- round(pair_info$bus_travel_time * 2 * 50, 0)

          # Add away game for school i
          all_games <- bind_rows(
            all_games,
            tibble(
              school_id = i,
              opponent_id = j,
              location = "away",
              travel_class = pair_info$travel_class,
              travel_time = pair_info$travel_time,
              distance_miles = distance_miles,
              strength_band = pair_info$strength_band
            )
          )

          # Add home game for school j
          all_games <- bind_rows(
            all_games,
            tibble(
              school_id = j,
              opponent_id = i,
              location = "home",
              travel_class = "home",
              travel_time = 0,
              distance_miles = 0,
              strength_band = pair_info$strength_band
            )
          )
        }
      }
    }
  }

  # Join school names and strengths
  all_games <- all_games |>
    left_join(
      schools_df |> select(school_id, school_name, strength),
      by = "school_id"
    ) |>
    left_join(
      schools_df |>
        select(
          school_id,
          opponent_name = school_name,
          opponent_strength = strength
        ),
      by = c("opponent_id" = "school_id")
    ) |>
    # Calculate relative strength description
    mutate(
      relative_strength = case_when(
        opponent_strength > strength ~ "stronger",
        opponent_strength < strength ~ "weaker",
        TRUE ~ "matched"
      ),
      # Travel mode description
      travel_mode = case_when(
        location == "home" ~ "home",
        travel_class == "B" ~ "bus",
        travel_class == "P" ~ "plane",
        TRUE ~ "unknown"
      )
    )

  # Assign week numbers within each school's schedule

  # Constraint: Schedule should alternate home and away games for each school
  # We use a greedy algorithm that strictly alternates when possible

  assign_weeks_alternating <- function(school_games) {
    n_games <- nrow(school_games)
    if (n_games == 0) {
      return(school_games |> mutate(week = integer()))
    }

    # Separate home and away games
    home_games <- school_games |> filter(location == "home")
    away_games <- school_games |> filter(location == "away")

    n_home <- nrow(home_games)
    n_away <- nrow(away_games)

    # Build schedule strictly alternating home and away
    scheduled <- tibble()
    home_idx <- 1
    away_idx <- 1

    # Determine starting location based on which type has more games
    # Start with the type that has more games to ensure better distribution
    last_was_home <- if (n_home >= n_away) FALSE else TRUE

    for (week in 1:n_games) {
      home_available <- home_idx <= n_home
      away_available <- away_idx <= n_away

      if (home_available && away_available) {
        # Both available - strictly alternate
        if (last_was_home) {
          # Schedule away
          scheduled <- bind_rows(
            scheduled,
            away_games[away_idx, ] |> mutate(week = week)
          )
          away_idx <- away_idx + 1
          last_was_home <- FALSE
        } else {
          # Schedule home
          scheduled <- bind_rows(
            scheduled,
            home_games[home_idx, ] |> mutate(week = week)
          )
          home_idx <- home_idx + 1
          last_was_home <- TRUE
        }
      } else if (home_available) {
        # Only home games left
        scheduled <- bind_rows(
          scheduled,
          home_games[home_idx, ] |> mutate(week = week)
        )
        home_idx <- home_idx + 1
        last_was_home <- TRUE
      } else if (away_available) {
        # Only away games left
        scheduled <- bind_rows(
          scheduled,
          away_games[away_idx, ] |> mutate(week = week)
        )
        away_idx <- away_idx + 1
        last_was_home <- FALSE
      }
    }

    return(scheduled)
  }

  # Apply the alternating constraint to each school's games
  weekly_schedule <- all_games |>
    group_by(school_id) |>
    group_modify(~ assign_weeks_alternating(.x)) |>
    ungroup() |>
    select(
      week,
      school = school_name,
      location,
      opponent = opponent_name,
      relative_strength,
      travel_mode,
      distance_miles,
      travel_time
    ) |>
    arrange(school, week)

  return(weekly_schedule)
}

weekly_schedule <- create_weekly_schedule(final_schedule, schools, school_pairs)

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("WEEKLY SCHEDULE BY SCHOOL\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("\nSchedule showing each school's 12-game season:\n")
cat("- Week: Game number in the season (1-12)\n")
cat("- Location: 'home' or 'away'\n")
cat("- Relative Strength: Opponent is 'stronger', 'weaker', or 'matched'\n")
cat("- Travel Mode: 'home', 'bus', or 'plane'\n")
cat("- Distance: Miles to travel (0 for home games)\n")
cat(
  "- Travel Time: Hours (",
  plane_travel_time,
  " hrs constant for plane)\n\n",
  sep = ""
)

# Print schedule for each school
for (school_name in unique(weekly_schedule$school)) {
  cat("\n--- ", school_name, " ---\n", sep = "")
  school_sched <- weekly_schedule |>
    filter(school == school_name) |>
    select(-school)
  print(school_sched, n = 12)
}

# Create a summary view
cat("\n\n--- Schedule Summary Table ---\n")
print(weekly_schedule, n = 60)

# =============================================================================
# SECTION 12: SCHEDULE HEATMAP VISUALIZATION
# =============================================================================

# Create a heatmap of the schedule
schedule_df <- as_tibble(final_schedule, rownames = "away_school") |>
  pivot_longer(-away_school, names_to = "home_school", values_to = "games") |>
  mutate(
    away_school = str_remove(away_school, "School_"),
    home_school = str_remove(home_school, "School_")
  )

ggplot(
  schedule_df,
  aes(x = home_school, y = away_school, fill = factor(games))
) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("0" = "gray95", "1" = "steelblue", "2" = "darkblue"),
    name = "Games"
  ) +
  labs(
    title = "Schedule Matrix Heatmap",
    subtitle = "Rows = Away team, Columns = Home team",
    x = "Home School",
    y = "Away School"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  coord_fixed()

cat("\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
cat("AUCTION SIMULATION COMPLETE\n")
cat("=" |> rep(60) |> paste(collapse = ""), "\n")
