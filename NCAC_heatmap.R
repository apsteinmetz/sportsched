# Load required libraries
library(tidyverse)

# -----------------------------
# 1. Define the data
# -----------------------------

ncac_strengths <- tribble(
  ~School         , ~Sport               ,
  "Allegheny"     , "Cross Country"      ,
  "Allegheny"     , "Track & Field"      ,
  "Allegheny"     , "Baseball"           ,
  "Allegheny"     , "Women's Basketball" ,

  "DePauw"        , "Women's Basketball" ,
  "DePauw"        , "Football"           ,
  "DePauw"        , "Women's Golf"       ,
  "DePauw"        , "Swimming & Diving"  ,

  "Denison"       , "Swimming & Diving"  ,
  "Denison"       , "Lacrosse"           ,
  "Denison"       , "Golf"               ,
  "Denison"       , "Track & Field"      ,
  "Denison"       , "Baseball"           ,

  "John Carroll"  , "Football"           ,
  "John Carroll"  , "Swimming & Diving"  ,

  "Kenyon"        , "Swimming & Diving"  ,
  "Kenyon"        , "Cross Country"      ,
  "Kenyon"        , "Track & Field"      ,
  "Kenyon"        , "Lacrosse"           ,

  "Oberlin"       , "Cross Country"      ,
  "Oberlin"       , "Track & Field"      ,
  "Oberlin"       , "Tennis"             ,
  "Oberlin"       , "Soccer"             ,

  "Ohio Wesleyan" , "Men's Soccer"       ,
  "Ohio Wesleyan" , "Track & Field"      ,
  "Ohio Wesleyan" , "Lacrosse"           ,
  "Ohio Wesleyan" , "Football"           ,

  "Wabash"        , "Wrestling"          ,
  "Wabash"        , "Football"           ,
  "Wabash"        , "Track & Field"      ,
  "Wabash"        , "Cross Country"      ,

  "Wittenberg"    , "Volleyball"         ,
  "Wittenberg"    , "Football"           ,
  "Wittenberg"    , "Men's Basketball"   ,

  "Wooster"       , "Baseball"           ,
  "Wooster"       , "Men's Basketball"   ,
  "Wooster"       , "Golf"               ,
  "Wooster"       , "Lacrosse"
)

# -----------------------------
# 2. Create a complete matrix of schools × sports
# -----------------------------

all_schools <- unique(ncac_strengths$School)
all_sports <- unique(ncac_strengths$Sport)

heatmap_df <- expand_grid(School = all_schools, Sport = all_sports) %>%
  mutate(
    Strong = if_else(
      paste(School, Sport) %in%
        paste(ncac_strengths$School, ncac_strengths$Sport),
      1,
      0
    )
  )

# -----------------------------
# 3. Plot the heatmap
# -----------------------------

ggplot(heatmap_df, aes(x = Sport, y = School, fill = Strong)) +
  geom_tile(color = "grey80") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "NCAC Strong Sports Heatmap",
    x = "Sport",
    y = "School",
    fill = "Strong Sport"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "none"
  )
