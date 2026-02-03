---
title: "Schedule Formation Model"
id: "auctioneer"
version: "0.1.1"
created: "2025-02-01"
author: "Art Steinmetz"
---

# Purpose
Forming a collegiate athletic conference where an auction market is used to create the seasonal schedule for a given sport.

# Scope
Write an R script implementing the iterative auction model below and simulate an auction to create a sports schedule for 20 schools. Use tidyverse idioms and visualize results with ggplot2. Liberally comment code. Ask for clarification if any instruction is ambiguous.

# Output Preferences
- Tidyverse verbs (dplyr, purrr, tibble, etc.)
- lpSolve for the clearing LP
- ggplot2 for visualization
- Functions return useful objects (not only printed output)
- When destructive actions are suggested, include explicit warnings

# Specification of Iterative Auction-Based Schedule Formation Model
(This document records the model and the implementation decisions made in the codebase and the chat history.)

## 1. Agents and season parameters
- Set of schools: N = {1,...,n}, with n = 20.
- Total games per school: G = 12.
- Exact home games per school: H = 6 and exact away games per school: A = 6 (model enforces exactly 6 home and 6 away).
- Seasonal budgets (reset each season): B_i = B0 ∀ i ∈ N.
- Per-iteration scarcity: an upper bound on games scheduled per auction round, parameter `games_per_round` (default 20) to create iterative scarcity and meaningful price discovery.

## 2. Strengths and discrete strength bands
- Each school i has a discrete strength score s_i ∈ {1,2,3}, assigned randomly in the simulation (1 = weak, 2 = moderate, 3 = strong).
- Strength band for ordered pair (i,j) is defined by the absolute difference:
  - b_ij = 3 if |s_i - s_j| = 0 ("matched", most preferred)
  - b_ij = 2 if |s_i - s_j| = 1 ("close")
  - b_ij = 1 if |s_i - s_j| = 2 ("mismatched", least preferred)

## 3. Geography, travel technology, and travel constants
- For each ordered pair (i,j):
  - Travel time T_ij (hours).
  - Travel cost C_ij (dollars).
  - Travel class τ_ij:
    - B (bus) if T_ij ≤ 5
    - P (plane) if T_ij > 5
- Plane travel time is treated as a constant: plane travel time = 5 hours (default) for each plane trip when used in aggregated travel-hours metrics.
- Travel cost constants (flat rates used in the model):
  - home_travel_cost = 0
  - bus_travel_cost = 1500
  - plane_travel_cost = 7500
- Home games satisfy T_ii = 0 and C_ii = 0.

## 4. Match variables
- Binary match variables x_ij ∈ {0,1} for i ≠ j, where x_ij = 1 denotes school i plays away at school j.
- Home games are represented by x_ji = 1 for the same pairing.

## 5. Feasibility constraints
(a) Total games per school:
- For all i:
  sum_{j ≠ i} x_{ij} + sum_{j ≠ i} x_{ji} = G

(b) Exact home/away:
- For all i:
  sum_{j ≠ i} x_{ji} = H  (exactly 6 home games)
  sum_{j ≠ i} x_{ij} = A  (exactly 6 away games)

(c) Pairwise match cap (soft/hard):
- Primary cap (preferred): For all unordered pairs {i,j}, x_{ij} + x_{ji} ≤ 1 (no repeated pairings).
- Fallback cap (allowed if strict cap makes the schedule infeasible): allow x_{ij} + x_{ji} ≤ 2 for the specific pairs required to complete a feasible schedule. The implementation should:
  - Attempt to produce a feasible schedule with the strict ≤1 cap.
  - If infeasible (some schools cannot reach G games), allow selected pairs to increase cap to 2 where necessary, and report precisely which pairs were relaxed.

(d) Binary constraints:
- x_{ij} ∈ {0,1}

## 6. Game types (auctioned goods)
- Away-game types k = (b, τ) ∈ {1,2,3} × {B,P}.
- Each ordered away match (i,j) belongs to exactly one type k_{ij} determined by b_{ij} and τ_{ij}.
- Home games are not auctioned.

## 7. Preference / utility specification (value and disutility)
- Schools report per-type disutilities v_{i,k} ≤ 0 (negative numbers). Disutility captures preference, travel burden, and spending aversion.
- Disutility components include:
  - Strength band preference (schools prefer matched opponents; band 3 least disutility)
  - Travel time disutility (longer travel increases disutility)
  - Travel cost disutility (plane cost > bus cost > home)
- To convert disutilities into willingness-to-pay (value), use a monotone transformation that maps lower disutility to higher value:
  - Let worst_disutility = min_k v_{i,k} across all k (most negative).
  - Define value_{i,k} = v_{i,k} - worst_disutility (so the most-preferred type has the largest non-negative value).
  - This value_{i,k} is interpreted as maximum tokens the school is willing to spend for one unit of type k.
  - Implementation note: values are computed per-school and normalized as needed for budget units.

## 8. Iterative auction process (revised design)
Initialization:
- Prices p_k^{(0)} = 0 for all k.
- Budgets B_i^{(0)} = B0 for all i.
- Iteration counter t = 0.
- Per-round supply limit: at most `games_per_round` games are allocated in each iteration t (default 20) — this enforces scarcity and enables price discovery.

Iteration t:
Step 1 — Demand determination:
- Each school i determines integer demand d_{i,k}^{(t)} for each away-game type k by selecting up to its remaining away slots (≤ A remaining) to maximize:
  sum_k q_{i,k} * (value_{i,k} - p_k^{(t)})
  subject to budget constraint: sum_k q_{i,k} * p_k^{(t)} ≤ B_i^{(t)}
- Demand is bounded by feasible opponents of that type (geography/strength) and the remaining away slots.

Step 2 — Clearing problem (solved with lpSolve as an integer program):
- Decision variables: x_{ij} ∈ {0,1} for candidate ordered pairs (i ≠ j) considered in this round.
- Objective: maximize aggregate realized value (or equivalently revenue) in this round. The implementation uses the realized per-game value (sum_i sum_j x_{ij} * value_{i,k_{ij}}) or can maximize p_k × x_{ij} depending on the chosen bookkeeping; the code uses value to prioritize more-desirable matches while tracking price dynamics for payments.
- Constraints:
  - All feasibility constraints in Section 5 (exact home/away per season enforced cumulatively across iterations).
  - Per-school demand quota for the round: sum_{j ≠ i} x_{ij} ≤ sum_k d_{i,k}^{(t)} (limits away assignments to the school's demand this round).
  - Per-round total games: sum_{i≠j} x_{ij} ≤ games_per_round (scarcity).
  - Pairwise caps: attempt with x_{ij} + x_{ji} ≤ 1; solver will indicate infeasibility if no solution exists. If infeasible globally, the outer logic will permit selective relaxations up to ≤2 for specific pairs as a fallback (see below).
- Solver: lpSolve (binary/integer programming). The clearing step returns the selected x_{ij} for the round and realized allocations by type.

Step 3 — Price update:
- For each type k, compute excess demand for the round as:
  excess_k = total_demand_for_k_in_round - allocated_units_of_type_k_in_round
- If excess_k > 0 then p_k^{(t+1)} = p_k^{(t)} + ε (price increment), else p_k^{(t+1)} = p_k^{(t)}.
- ε is a small increment parameter (configurable in the script); per-round scarcity ensures price movements are informative.

Step 4 — Budget update:
- For each school i, reduce budget by payments for allocations in this round:
  B_i^{(t+1)} = B_i^{(t)} - sum_{j ≠ i} x_{ij} * p_{k_{ij}}^{(t)}
- Travel costs (bus/plane) are treated separately and tracked as season travel expenditures; they also affect school disutility/value in demand generation but are paid outside the token-budget accounting (the model tracks both tokens spent and travel dollars).

Step 5 — Cumulative assignment update:
- Update the cumulative schedule matrix with x_{ij} from this round.
- Update remaining home/away requirements per school.

Fallback for infeasibility (pairwise cap relaxation):
- The auction first attempts to find a feasible allocation respecting x_{ij} + x_{ji} ≤ 1.
- If, after the allowed number of iterations, some schools cannot reach G games due to pairwise constraints, the algorithm:
  1. Identifies the minimal set of pairs whose cap must be relaxed to 2 to allow completion.
  2. Re-runs greedy/LP fill with those pairs allowed to have up to 2 meetings.
  3. Records and reports which pairs were relaxed and why.
- The implementation prioritizes feasibility and reports any relaxation instances to the user.

Termination:
- The process terminates when:
  - All schools have exactly G games scheduled (and exact H home games each).
  - All hard feasibility constraints (binary, totals) are met under the applied pairwise caps (≤1 or selectively ≤2).
  - No excess demand remains for positive-price types, or a max-iteration limit is reached (with reporting).

## 9. Travel aggregation and preferences
- The script computes per-school and conference totals for:
  - Number of bus away games and plane away games.
  - Total travel hours over the season (bus travel time computed from distance; plane trips use the constant plane_travel_time = 5 hours).
  - Total travel dollars (per-game flat costs: home = 0, bus = 1500, plane = 7500).
- Travel hours and travel cost negatively affect school disutility (schools prefer lower travel hours and lower travel cost). These components are included when computing v_{i,k} and thus value_{i,k}.

## 10. Reporting
- Final outputs:
  - final_schedule: matrix x_{ij} for the season.
  - readable weekly schedule grouped by school (weeks 1..12), with columns:
    - week, school (base), "home"/"away", opponent, relative_strength ("weaker","stronger","matched"), travel_mode ("home","bus","plane"), distance_miles, travel_time_hours.
  - final_prices by game type k and iteration history of price evolution.
  - per-school travel hours and travel dollars, and budgets final state.
  - constraint reports:
    - Any pairwise cap relaxations performed (which pairs and why).
    - Any remaining infeasibilities if schedule couldn't be completed (with diagnostics).
- Visualizations:
  - Map of school locations.
  - Conference schedule map showing lines/arrows for each matchup (and a per-school arrowed map function).
  - Bar charts for travel hours and travel cost per school.
  - Price evolution plot over auction iterations.
  - Game-type distribution and counts.

## 11. Implementation notes
- Use lpSolve (or ompr + ROI with an integer solver) for the clearing LP. Default choice: lpSolve for portability.
- Ensure the script can be re-sourced cleanly (avoid creating duplicate columns on repeated joins).
- Default parameters (modifiable at top of script):
  - n = 20, G = 12, H = 6, A = 6, B0 = 100 (or user-specified), bus_travel_cost = 1500, plane_travel_cost = 7500, plane_travel_time = 5, games_per_round = 20, price_increment = 2, max_iterations = 500.
- Prefer dplyr |> pipe and ggplot2 visualizations. Use testable functions that return useful objects.

## 12. Interpretation
- The mechanism is an iterative, value-based ascending auction with typed away-games and hard scheduling feasibility.
- Final prices reflect both willingness-to-pay (driven by preferences for matched opponents and lower travel burden) and scarcity induced by per-round supply limits and geographic constraints.
- When strict pairwise caps make a feasible schedule impossible, the system relaxes caps minimally and transparently, reporting any such exceptions.

--- 

End of specification (version 0.1.1).