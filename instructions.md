
---
title: "Schedule Formation Model"
id: "auctioneer"
version: "0.1.0"
created: "2025-02-01"
author: "Art Steinmetz"
---

# Purpose
Forming a collegiate athletic conference where an auction market is used to  create the seasonal schedule for a given sport. 

# Scope
Your task is to write an R language script that implements the auction model below and then simulate an auction to create a sports schedule  in a single sport for a universe of 20 schools.  Use the model specification below.

# Output Preferences
Liberally comment the code with explanations for each step.  Code using the Tidyverse vernacular with Dplyr verbs, not base R.  Visualize important observations using ggplot2 graphics.  Ask for clarification where instructions are ambiguous.

# Specification of Iterative Auction-Based Schedule Formation Model

*(with strength bands, travel classes, budgets, and pairwise caps)*

## 1. Agents and season parameters

- **Set of schools**:
  \[
  N = \{1,\dots,n\}
  \]

- **Total games per school**:
  \[
  G = 12
  \]

- **Minimum home games per school**:
  \[
  H_{\min} = 5
  \]

- **Seasonal budgets (reset each season)**:
  \[
  B_i = B_0 \quad \forall i \in N
  \]

---

## 2. Strength and strength bands

Each school \( i \) has an exogenous strength index \( s_i \in \mathbb{R} \).

Let thresholds satisfy \( 0 < \Delta_1 < \Delta_2 \). For each ordered pair \( (i,j) \), define the strength band:

\[
 b_{ij} =
 \begin{cases}
 1 & |s_i - s_j| > \Delta_2 \\
 2 & \Delta_1 < |s_i - s_j| \le \Delta_2 \\
 3 & |s_i - s_j| \le \Delta_1
 \end{cases}
\]

Band **2** represents evenly matched opponents.

---

## 3. Travel technology

For each ordered pair \( (i,j) \):

- Travel time \( T_{ij} \)
- Travel cost \( C_{ij} \)

Define travel class:

\[
 \tau_{ij} =
 \begin{cases}
 B & T_{ij} \le 5 \quad \text{(bus)} \\
 P & T_{ij} > 5 \quad \text{(plane)}
 \end{cases}
\]

Home games satisfy:
\[
 T_{ii} = 0, \quad C_{ii} = 0
\]

---

## 4. Match variables

Let:
\[
 x_{ij} \in \{0,1\} \quad \text{for } i \neq j
\]

where \( x_{ij} = 1 \) indicates that school \( i \) plays **away** at school \( j \).

---

## 5. Feasibility constraints

### (a) Total games per school
For all \( i \):
\[
 \sum_{j \neq i} x_{ij} + \sum_{j \neq i} x_{ji} = G
\]

---

### (b) Minimum home games
For all \( i \):
\[
 \sum_{j \neq i} x_{ji} \ge H_{\min}
\]

---

### (c) Pairwise match cap
For all unordered pairs \( \{i,j\} \), \( i \neq j \):
\[
 x_{ij} + x_{ji} \le 2
\]

This allows at most two total meetings between any pair of schools.

---

### (d) Binary constraints
\[
 x_{ij} \in \{0,1\}
\]

---

## 6. Auctioned good types

Define **away-game types**:
\[
 k = (b, \tau) \in \{1,2,3\} \times \{B,P\}
\]

Each away match \( (i,j) \) belongs to exactly one type \( k_{ij} = (b_{ij}, \tau_{ij}) \).

Home games are not auctioned.

---

## 7. Bidding language

Each school \( i \) submits bids over away-game types:
\[
 \beta_i = \{(q_{i,k}, v_{i,k})\}_{k}
\]

Where:
- \( q_{i,k} \in \mathbb{Z}_{\ge 0} \): quantity of away games of type \( k \)
- \( v_{i,k} \le 0 \): per-unit disutility (in tokens)

Interpretation:
> School \( i \) is willing to accept up to \( q_{i,k} \) away games of type \( k \), paying \( |v_{i,k}| \) tokens per game.

---

## 8. Iterative auction process

### Initialization
- Prices \( p_k^{(0)} = 0 \) for all \( k \)
- Budgets \( B_i^{(0)} = B_0 \)
- Iteration counter \( t = 0 \)

---

### Iteration \( t \)

#### Step 1: Demand determination
Each school chooses demand:
\[
 d_{i,k}^{(t)} = \arg\max_{q_{i,k}} \sum_k q_{i,k}(v_{i,k} - p_k^{(t)})
\]

subject to:
\[
 \sum_k q_{i,k} p_k^{(t)} \le B_i^{(t)}
\]

---

#### Step 2: Clearing problem

The auctioneer solves:
\[
 \max_{x} \sum_{i \neq j} x_{ij} \cdot p_{k_{ij}}^{(t)}
\]

subject to:
- All feasibility constraints in Section 5
- For each school \( i \):
\[
 \sum_{j \neq i} x_{ij} \le \sum_k d_{i,k}^{(t)}
\]

---

#### Step 3: Price update
For each type \( k \):
\[
 p_k^{(t+1)} =
 \begin{cases}
 p_k^{(t)} + \epsilon & \text{if excess demand exists} \\
 p_k^{(t)} & \text{otherwise}
 \end{cases}
\]

---

#### Step 4: Budget update
Budgets evolve as:
\[
 B_i^{(t+1)} = B_i^{(t)} - \sum_{j \neq i} x_{ij} p_{k_{ij}}^{(t)}
\]

---

### Termination
The process terminates when:
- All schools have exactly \( G \) games
- All feasibility constraints are satisfied
- No excess demand remains

---

## 9. Mechanism properties

- At least \( H_{\min} = 5 \) home games per school
- Explicit preference for evenly matched (band 2) games
- Travel burden handled endogenously via pricing
- Pairwise match cap prevents over-repetition
- Budgets reset each season
- No aftermarket or renegotiation

---

## 10. Interpretation

This mechanism is formally equivalent to iterative combinatorial allocation models used in transport capacity, fair scheduling, and network-constrained markets, with typed goods and hard feasibility constraints.



