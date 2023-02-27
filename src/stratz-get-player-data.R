library(tidyverse)
library(httr)

url <- "https://api.stratz.com/api/v1/match/"
api_key <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJuYW1laWQiOiJodHRwczovL3N0ZWFtY29tbXVuaXR5LmNvbS9vcGVuaWQvaWQvNzY1NjExOTgxMjAxNDQ2MjIiLCJ1bmlxdWVfbmFtZSI6IkFsZGVtYXIgVmlnw6FyaW8iLCJTdWJqZWN0IjoiMjc1MjY3ZTYtNGYwNi00M2U2LWIyNDYtNzVjZjVlNDRjMjE4IiwiU3RlYW1JZCI6IjE1OTg3ODg5NCIsIm5iZiI6MTY3NzAxNTQ2NywiZXhwIjoxNzA4NTUxNDY3LCJpYXQiOjE2NzcwMTU0NjcsImlzcyI6Imh0dHBzOi8vYXBpLnN0cmF0ei5jb20ifQ.Z3lA1CmFlxL5G8Go1zoWdtTxZCUY0FOJSluHpfnpDg4"

match_id <- "7025968515"
query <- list(api_key = api_key)

response <- GET(paste0(url, match_id), add_headers = (.headers = c("Authorization" = api_key)))

content <- content(response)

content$players[[1]] %>%
    glimpse(max.level = 1)

content$players[[1]] %>%
    map(unlist) %>%
    reduce(bind_rows)
    map_dfr(unlist)
    unlist()

as_tibble(t(do.call(cbind, content$players))) %>%
    select(kills, deaths, last_hits, gold_per_min, towers_killed, roshans_killed, teamfight_participation, obs_placed, camps_stacked, rune_pickups, firstblood_claimed, stuns) %>%
    View()
