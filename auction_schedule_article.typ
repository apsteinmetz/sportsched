// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
)

#show: doc => article(
  title: [Auction-Based Collegiate Athletic Conference Scheduling],
  subtitle: [An Iterative Market Mechanism for Sports Schedule Formation],
  authors: (
    ( name: [Art Steinmetz],
      affiliation: [],
      email: [] ),
    ),
  date: [2026-02-06],
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)

= Abstract
<abstract>
This article presents an iterative auction-based model for forming collegiate athletic conference schedules. The mechanism treats away games as differentiated goods, categorized by opponent strength match and travel requirements. Schools bid on game types according to their preferences, and a market-clearing process determines the final schedule. The model enforces hard constraints including exact home/away game requirements and pairwise meeting limits, while prices emerge endogenously to reflect relative scarcity and desirability. The prices for game types are established early in the auction while many further iterations are required to optimize the schedule. Analysis of simulation results reveals how geographic isolation creates systematic disadvantages for remote schools, who must spend more tokens to acquire less desirable schedules. The auction framework provides a transparent, preference-respecting approach to the complex combinatorial problem of conference scheduling.

= Introduction
<introduction>
In the last few decades, collegiate athletic conferences have undergone significant realignment driven by financial incentives, media rights considerations, and competitive balance concerns. This is a particular problem for Division III conferences, where schools have limited travel budgets and and fewer peer competitors. Further, schools in a conference play each other in every sport, while schools have varying levels of strength by sport. Some conferences have made agreements to bring in outside schools to some to some sports but these are ad-hoc arrangements.

A typical Division III conference has ten schools. An alternative structure might allow highly fluid "conference" definitions where many more schools participate. Opponents could be different for each sport and different for each year. Such a structure would encompass a larger geographic area and allow more flexibility for schools to find appropriate opponents. However, the scheduling problem becomes much more complex. Travel time and cost also grow as the number of schools increases.

This article explores an such an approach: using an iterative auction mechanism to form conference schedules. In this framework, away games are treated as differentiated goods, and schools express their preferences through bidding behavior. The market mechanism then allocates games in a way that respects both preferences and hard constraints. While the literature contains many approaches to athletic schedule optimization, I did not see any specifically suggesting an auction market.

The auction-based approach offers several advantages:

+ #strong[Preference revelation];: Schools' bids reveal their true preferences over different types of games
+ #strong[Price discovery];: Equilibrium prices emerge that reflect the relative scarcity of desirable game types
+ #strong[Transparency];: The allocation process is governed by clear rules rather than opaque negotiations
+ #strong[Flexibility];: The mechanism can accommodate heterogeneous preferences across schools

Notably, the auction does not directly allocate specific opponents. Instead, schools bid on categories of games (e.g., "evenly matched bus games") and the clearing mechanism determines which specific opponents they receive based on geographic compatibility and mutual availability.

We implement and simulate this mechanism for a 20-school conference where each school plays 12 games per season (6 home, 6 away). The simulation demonstrates how the auction produces feasible schedules while generating meaningful price signals about game type desirability.

This paper is written as an Quarto embedded code notebook, so all the code necessary to reproduce and critique the results is visible. I believe this is the future of academic publishing. Quarto publishing, the Positron IDE, the R language were used. While I am an experienced R coder, Claude Opus 4.5 wrote most of the code. "The programming language of the future is English."

DISCLAIMER: This is a "toy" model, created by a data science hobbyist without domain expertise. There may be errors. I share this in hopes it might inspire some discussion around the future of athletic conferences and provide ideas for student data science explorations.

= Model Description
<model-description>
== Agents and Season Parameters
<agents-and-season-parameters>
The model considers a set of $N = 20$ schools that form an athletic conference. Each school must play exactly $G = 12$ games per season, split evenly between home and away:

- Home games: $H = 6$
- Away games: $A = 6$

Each school begins the season with a budget of $B_0 = 100$ tokens, used to bid on away games.

== School Strengths
<school-strengths>
Each school $i$ is assigned a discrete strength score $s_i in { 1 \, 2 \, 3 }$:

- $s_i = 1$: Weak
- $s_i = 2$: Moderate \
- $s_i = 3$: Strong

The strength match between two schools is defined by the absolute difference in their strength scores:

$ b_(i j) = cases(delim: "{", 3 & upright("if ") lr(|s_i - s_j|) = 0 upright(" (evenly matched)"), 2 & upright("if ") lr(|s_i - s_j|) = 1 upright(" (close match)"), 1 & upright("if ") lr(|s_i - s_j|) = 2 upright(" (mismatched)")) $

Schools prefer evenly matched opponents (band 3) over mismatched opponents (band 1).

== Geography and Travel
<geography-and-travel>
Each school has a geographic location characterized by latitude and longitude coordinates. For each ordered pair of schools $(i \, j)$, travel requirements are determined by:

- #strong[Distance];: Haversine distance in miles between school locations
- #strong[Bus travel time];: $T_(i j)^(b u s) = upright("distance")_(i j) \/ 60$ hours
- #strong[Travel class] $tau_(i j)$:
  - $B$ (bus) if $T_(i j)^(b u s) lt.eq 5$ hours
  - $P$ (plane) if $T_(i j)^(b u s) > 5$ hours

Travel costs are fixed rates:

#table(
  columns: 2,
  align: (auto,auto,),
  table.header([Travel Mode], [Cost],),
  table.hline(),
  [Home], [\$0],
  [Bus], [\$1,500],
  [Plane], [\$7,500],
)
Plane travel time is treated as constant at 5 hours regardless of distance.

== Game Types
<game-types>
Away games are categorized into 6 types based on the combination of strength match and travel class:

$ k = (upright("strength_match") \, upright("travel_class")) in { 1 \, 2 \, 3 } times { B \, P } $

Each ordered away match $(i \, j)$ belongs to exactly one game type determined by $b_(i j)$ and $tau_(i j)$.

== Preference Specification
<preference-specification>
Schools express preferences over game types through disutilities $v_(i \, k) lt.eq 0$. More negative values indicate less desirable game types. Disutility captures:

- Strength band preference (evenly matched preferred)
- Travel time burden
- Travel cost burden

To convert disutilities to willingness-to-pay (value):

$ upright("value")_(i \, k) = v_(i \, k) - min_k v_(i \, k) $

This ensures the most-preferred game type has the highest value and the least-preferred has value zero.

== Feasibility Constraints
<feasibility-constraints>
The final schedule must satisfy:

#strong[\(a) Total games per school:] $ sum_(j eq.not i) x_(i j) + sum_(j eq.not i) x_(j i) = G quad forall i $

#strong[\(b) Exact home/away split:] $ sum_(j eq.not i) x_(j i) = H quad forall i quad upright("(home games)") $ $ sum_(j eq.not i) x_(i j) = A quad forall i quad upright("(away games)") $

#strong[\(c) Pairwise meeting cap:] $ x_(i j) + x_(j i) lt.eq 1 quad forall i < j $

The constraint above prefers single meetings between any pair of schools. However, if a feasible schedule cannot be constructed with all single matchups, the algorithm relaxes this constraint to allow teams to play each other up to two times (one home, one away).

#strong[\(d) Binary decision variables:] $ x_(i j) in { 0 \, 1 } $

== Iterative Auction Process
<iterative-auction-process>
The auction proceeds iteratively:

+ #strong[Demand determination];: Each school determines integer demand for each game type given current prices and remaining budget
+ #strong[Clearing problem];: An LP solver finds the value-maximizing allocation respecting feasibility and demand constraints
+ #strong[Price update];: Prices increase for game types with excess demand
+ #strong[Budget update];: Schools pay for allocated games at current prices
+ #strong[Schedule update];: Cumulative schedule matrix is updated

The process terminates when all schools have complete schedules or a maximum iteration limit is reached.

=== The Bidder's Experience
<the-bidders-experience>
From a school's perspective, the auction unfolds as follows. At the start of each round, the school reviews the current prices for each of the six game types and its remaining token budget. The athletic director must decide: how many games of each type do we want at these prices?

Consider a typical school, "Overthinkon."” Their ideal schedule would include mostly evenly-matched bus games---nearby opponents of similar strength. But if prices for these premium games have risen to 15 tokens each while mismatched plane games remain at 2 tokens, the school faces a trade-off. Spending heavily on preferred games leaves fewer tokens for later rounds when competition for remaining slots intensifies.

At the same time, a nearby evenly matched opponent which is very desirable for Overthinkon will be seen by another school as a mismatched long-distance game and will not want to pay much for it.

Schools cannot directly bid on specific opponents. Instead, they express demand for #emph[categories] of games. If Overthinkon requests 3 evenly-matched bus games, the clearing mechanism determines which specific opponents they receive based on geographic compatibility and mutual availability. A school might request games against strong nearby teams but end up matched with whoever remains feasible given everyone else's requests.

This indirect allocation creates strategic uncertainty. Schools with many nearby peers can often satisfy their preferences, while geographically isolated schools find themselves price-takers---forced to accept whatever game types their location permits, regardless of willingness to pay.

= Auction Simulation
<auction-simulation>
Now we will implement and simulate the auction mechanism described above. The code is organized into sections corresponding to the model components. Code visibility can be toggled in the interactive version of this article.

== Section 1: Initialize Season Parameters
<section-1-initialize-season-parameters>
This section establishes the fundamental parameters that govern the auction simulation, including the number of schools, games per season, and budget allocations. The code is visible in the interactive version of this article.

The conference consists of 20 schools, each playing 12 games with an even split between home and away. Each school starts with 100 tokens to bid on away games.

== Section 2: Create Schools and Geographic Locations
<section-2-create-schools-and-geographic-locations>
Schools are created with randomly assigned strength scores and random geographic locations within a region approximating the eastern United States. The names are fictional.

#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-school-locations-map-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Map of conference school locations with state boundaries
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-school-locations-map>


#ref(<fig-school-locations-map>, supplement: [Figure]) shows the geographic distribution of conference schools with state boundaries for context.

== Section 3: Define Game Types and Preferences
<section-3-define-game-types-and-preferences>
Schools have preferences over different types of away games. Game types are defined by the combination of opponent strength match and travel requirements. In practice schools would reveal their preferernces through the bidding process. Here we assign somewhat realistic preferences to simulate the auction. Each school's preferences are generated with some random variation around base disutility values.

#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-preferences-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Average school preferences by game type
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-preferences>


Schools strongly prefer evenly matched bus games (low disutility) and dislike mismatched plane games (high disutility).

== Section 4: Create School Pairs with Travel Information
<section-4-create-school-pairs-with-travel-information>
This section computes the travel requirements between all pairs of schools, determining which games require plane travel versus bus travel. If the bus ride is longer than 5 hours, the game is classified as a plane game with a fixed travel time of 5 hours. The cost for a bus trip is \$1,500 while the cost for a plane trip is \$7,500. This is not intened be an accurate model of travel costs, just a simple way to differentiate travel types. Likewise, we use the straight-line distance rather than actual driving distance, which would not be realistic if we were using acutal schools.

#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-travel-distribution-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Distribution of travel times between schools
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-travel-distribution>


#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-game-type-distribution-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Distribution of away game types
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-game-type-distribution>


Some schools are more geographically isolated than others. The table below quantifies each school's isolation score, which reflects the average distance to its opponents weighted by match quality. Inevitably, schools located far from the conference core must travel longer distances to reach well-matched opponents.

#figure([
#table(
  columns: (12.63%, 9.47%, 18.95%, 18.95%, 12.63%, 10.53%, 16.84%),
  align: (left,right,right,right,right,right,right,),
  table.header([School], [Strength], [Avg Distance (mi)], [Weighted Avg (mi)], [Bus Options], [Equal+Bus], [Isolation Score],),
  table.hline(),
  [Blamora], [1], [693], [671], [3], [1], [150],
  [Indecisia], [2], [623], [621], [4], [0], [139],
  [Tangentia], [3], [537], [518], [3], [1], [116],
  [Snackratia], [1], [466], [512], [5], [0], [115],
  [Awkwardia], [3], [471], [504], [5], [1], [113],
  [Confusios], [1], [492], [478], [5], [1], [107],
  [Scrollos], [2], [460], [460], [2], [0], [103],
  [Misplacion], [3], [481], [456], [6], [3], [102],
  [Clutteron], [3], [417], [437], [5], [1], [98],
  [Mumbleon], [3], [455], [431], [7], [3], [97],
  [Spillonius], [2], [414], [423], [8], [1], [95],
  [Fidgetes], [2], [416], [420], [4], [1], [94],
  [Snackylios], [1], [386], [400], [5], [0], [90],
  [Napthena], [1], [380], [399], [7], [1], [89],
  [Oopsicles], [3], [413], [391], [8], [4], [87],
  [Procrastion], [2], [381], [379], [7], [1], [85],
  [Overthinkon], [1], [375], [378], [5], [1], [85],
  [Oopsida], [3], [378], [377], [5], [0], [84],
  [Dramathea], [3], [353], [345], [8], [3], [77],
  [Wifion], [2], [334], [338], [10], [3], [76],
)
], caption: figure.caption(
position: top, 
[
Geographic isolation scores by school
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-geographic-isolation>


The isolation score weights distance by opponent match quality---schools whose evenly-matched opponents are far away score higher than schools with nearby well-matched peers.

== Section 5: Define Auction Helper Functions
<section-5-define-auction-helper-functions>
The auction requires several helper functions for demand calculation, feasibility checking, and schedule updates.

== Section 6: LP-Based Clearing Mechanism
<section-6-lp-based-clearing-mechanism>
The heart of the auction is the clearing problem, solved as an integer linear program to find the value-maximizing allocation.

== Section 7: Run the Auction
<section-7-run-the-auction>
With all components in place, we execute the iterative auction to produce the conference schedule.

The auction completed in 100 iterations, producing a complete schedule for all 20 schools.

= Results
<results>
== Schedule Verification
<schedule-verification>
We first verify that the produced schedule satisfies all feasibility constraints.

#figure([
#table(
  columns: 4,
  align: (left,left,left,left,),
  table.header([Constraint], [Required], [Observed], [Satisfied],),
  table.hline(),
  [Total games per school], [= 12], [12-12], [TRUE],
  [Home games per school], [= 6], [6-6], [TRUE],
  [Away games per school], [= 6], [6-6], [TRUE],
  [Max pairwise meetings], [≤ 2], [2], [TRUE],
)
], caption: figure.caption(
position: top, 
[
Schedule constraint verification
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-constraints>


== Final Prices
<final-prices>
The auction produces prices for each game type that reflect relative demand and scarcity. Note that prices reach their terminal values quickly, often within the first 20 iterations. Beyond that, prices stabilize as schools exhaust their budgets.

#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-price-evolution-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Price evolution during the auction
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-price-evolution>


== Travel Analysis
<travel-analysis>
Geographic location significantly impacts school outcomes in terms of travel burden.

#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-travel-cost-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Total travel cost by school
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-travel-cost>


#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-travel-hours-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Total travel hours by school
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-travel-hours>


== Budget and Disutility Analysis
<budget-and-disutility-analysis>
We examine how token spending relates to schedule quality (total disutility).

#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-spent-vs-disutility-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Tokens spent versus total disutility
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-spent-vs-disutility>


== Geographic Isolation Effects
<geographic-isolation-effects>
A key finding is that geographic isolation systematically disadvantages schools.

#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-geographic-isolation-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Geographic isolation versus schedule quality
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-geographic-isolation>


== Conference Schedule Map
<conference-schedule-map>
#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-schedule-map-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Conference schedule map showing all matchups
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-schedule-map>


#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-single-school-schedule-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Example schedule for a single school
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-single-school-schedule>


#figure([
#table(
  columns: (4.95%, 11.88%, 8.91%, 13.86%, 11.88%, 16.83%, 13.86%, 17.82%),
  align: (right,left,left,left,left,left,left,left,),
  table.header([Week], [Opponent], [Location], [Opp. Strength], [Travel Mode], [Opp. Travel Mode], [Distance (mi)], [Travel Time (hrs)],),
  table.hline(),
  [1], [Procrastion], [Home], [2], [---], [Bus], [299], [5],
  [2], [Snackylios], [Away], [1], [Plane], [---], [394], [5],
  [3], [Clutteron], [Home], [3], [---], [Plane], [360], [5],
  [4], [Confusios], [Away], [1], [Plane], [---], [369], [5],
  [5], [Indecisia], [Home], [2], [---], [Plane], [602], [5],
  [6], [Awkwardia], [Away], [3], [Plane], [---], [361], [5],
  [7], [Fidgetes], [Home], [2], [---], [Bus], [81], [1.3],
  [8], [Blamora], [Away], [1], [Plane], [---], [626], [5],
  [9], [Napthena], [Home], [1], [---], [Bus], [137], [2.3],
  [10], [Mumbleon], [Away], [3], [Plane], [---], [349], [5],
  [11], [Wifion], [Home], [2], [---], [Bus], [163], [2.7],
  [12], [Snackratia], [Away], [1], [Plane], [---], [493], [5],
)
], caption: figure.caption(
position: top, 
[
Overthinkon's complete season schedule
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-overthinkion-schedule>


In this particular example, Overthinkon has received a package where all their away games are by plane, resulting in the highest travel cost among all schools. Let's look at how their utility budget might have resulted in this outcome.

#figure([
#table(
  columns: (31.43%, 17.14%, 14.29%, 15.71%, 21.43%),
  align: (left,right,right,right,left,),
  table.header([Game Type], [Overthinkon], [Conf. Avg], [Difference], [Interpretation],),
  table.hline(),
  [Match\_equal match\_Bus], [-3.1], [-2.3], [-0.8], [Similar],
  [Match\_equal match\_Fly], [-5.5], [-10.4], [4.9], [Less averse],
  [Match\_close match\_Bus], [-11.5], [-8.6], [-2.9], [More averse],
  [Match\_close match\_Fly], [-17.2], [-15.7], [-1.5], [More averse],
  [Match\_mismatched\_Bus], [-22.9], [-20.0], [-2.9], [More averse],
  [Match\_mismatched\_Fly], [-25.5], [-30.3], [4.8], [Less averse],
)
], caption: figure.caption(
position: top, 
[
Overthinkon's game type preferences compared to conference average
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-overthinkon-utility>


Schools with less aversion to plane travel (less negative disutility for plane game types - bigger travel budgets) would be more willing to bid on those games. The problem for Overthinkon is their well-matched opponents are all far away, forcing them to acquire plane games to get quality matchups.

== Schedule Heatmap
<schedule-heatmap>
Schools will not meet the same opponent multiple times in a season unless the schedule cannot be filled otherwise. The schedule heatmap below shows how many times each pair of schools meet during the season. In this run Snackylios and Figetes meet twice, while all other pairs meet either once or not at all. The liklihood of multiple meetings depends on the utility budgets and geographic distribution of schools.

#figure([
#box(image("auction_schedule_article_files/figure-typst/fig-schedule-heatmap-1.svg"))
], caption: figure.caption(
position: bottom, 
[
Schedule matrix heatmap
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-schedule-heatmap>


= Conclusion
<conclusion>
This simulation demonstrates that an iterative auction mechanism can successfully produce feasible conference schedules while respecting school preferences and hard constraints. Key findings include:

+ #strong[Successful schedule formation];: The auction consistently produces complete schedules satisfying all constraints (12 games per school, exactly 6 home and 6 away, at most two meetings per pair).

+ #strong[Meaningful price discovery];: Prices for desirable game types (evenly matched, bus travel) rise during the auction, reflecting genuine scarcity and preferences.

+ #strong[Geographic disadvantage];: Schools in geographically isolated locations face structural disadvantages. They must acquire more plane games regardless of preference, leading to:

  - Higher token expenditure
  - Worse schedule quality (higher total disutility)
  - This inverse correlation between spending and outcomes reflects the geographic constraints embedded in the game type structure.

+ #strong[Preference heterogeneity matters];: Schools with different strength profiles have different sets of opponents available for "evenly matched" games, affecting their ability to achieve preferred schedules.

The auction framework provides a transparent, rules-based approach to conference scheduling that could be adapted for real-world applications. Future extensions could incorporate additional constraints such as mandatory rivalries and dollar budgets for travel costs.

The key feature of this model is that auction prices reflect game #emph[types];, not individual matchups. Schools cannot escape their geographic constraints through bidding behavior alone---a school surrounded by distant opponents must acquire plane games regardless of their willingness to pay for bus games. This structural feature of the mechanism has important equity implications for conference design.
