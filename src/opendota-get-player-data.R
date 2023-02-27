library(tidyverse)
library(httr)
library(googlesheets4)


gsheet_matches <- "https://docs.google.com/spreadsheets/d/1UMfZPGZCMBTQ8GOhjct0umpJiZp1kvOmKLN7m4SyXGU"
gsheet_players <- "https://docs.google.com/spreadsheets/d/1FMNj3I8UKFDvkThuNdDZM2OGjofZ_kQotTsNqV4y9nI"
gsheet_scores <- "https://docs.google.com/spreadsheets/d/14eKVVBPUZ86xEqRtrPLIvzIax3kz_ww5mr53QwiqDhw"

matches <- read_sheet(gsheet_matches)
players <- read_sheet(gsheet_players)


update_sheet_matches <- function(match_id) {
    matches <- read_sheet(gsheet_matches)

    matches <- matches %>%
        mutate(Updated = ifelse(MatchID == match_id, TRUE, Updated))

    write_sheet(matches, ss = gsheet_matches, "Matches")
}


update_sheet_scores <- function(scores, scores_sheet) {
    old_sheet <- read_sheet(scores_sheet)

    matches <- read_sheet(gsheet_matches)

    if(pull(filter(matches, MatchID == scores$match_id[1]), "Updated")) {
        return(FALSE)
    }

    update_sheet_matches(scores$match_id[1])

    write_sheet(bind_rows(old_sheet, scores), ss = scores_sheet, "Scores")
}


get_scores <- function(match_id, matches, matches_sheet) {
    url <- "https://api.opendota.com/api/matches/"

    response <- GET(paste0(url, match_id))
    content <- content(response)

    players <- tibble()
    for(i in 1:10) {
        players <- bind_rows(
            players,
            tibble(
                match_id = content$players[[i]]$match_id,
                account_id = content$players[[i]]$account_id,
                hero_id = content$players[[i]]$hero_id,
                isRadiant = content$players[[i]]$isRadiant,
                kills = content$players[[i]]$kills,
                deaths = content$players[[i]]$deaths,
                last_hits = content$players[[i]]$last_hits + content$players[[1]]$denies,
                gold_per_min = content$players[[i]]$gold_per_min,
                towers_killed = content$players[[i]]$towers_killed,
                roshans_killed = content$players[[i]]$roshans_killed,
                teamfight_participation = content$players[[i]]$teamfight_participation,
                obs_placed = content$players[[i]]$obs_placed,
                camps_stacked = content$players[[i]]$camps_stacked,
                rune_pickups = content$players[[i]]$rune_pickups,
                firstblood_claimed = content$players[[i]]$firstblood_claimed,
                stuns = content$players[[i]]$stuns
            )
        )
    }

    players <- players %>%
        select(match_id, account_id, hero_id, isRadiant, kills, deaths, last_hits, gold_per_min, towers_killed, roshans_killed, teamfight_participation, obs_placed, camps_stacked, rune_pickups, firstblood_claimed, stuns)

    # players <- as_tibble(t(do.call(cbind, content$players))) %>%
    #     select(match_id, account_id, hero_id, kills, deaths, last_hits, gold_per_min, towers_killed, roshans_killed, teamfight_participation, obs_placed, camps_stacked, rune_pickups, firstblood_claimed, stuns) %>%
    #     mutate_all(as.character) %>%
    #     mutate_all(as.numeric)

    write_csv(players, paste0("data/raw_", match_id, ".csv"))

    return(players)
}


get_heroes <- function() {
    url <- "https://api.opendota.com/api/heroes/"

    response <- GET(url)
    content <- content(response)

    heroes <- as_tibble(t(do.call(cbind, content))) %>%
        mutate_all(as.character)

    write_csv(heroes, paste0("data/heroes_", Sys.Date(), ".csv"))

    return(heroes)
}


url <- "https://api.opendota.com/api/matches/"
match_id <- 7028077898

response <- GET(paste0(url, match_id))
content <- content(response)

content$players[[3]]$rank_tier

    glimpse(max.level = 1) %>% View()
    as.data.frame() %>%View

# i <- 1

# bind_rows(tibble(
#     match_id = content$players[[i]]$match_id,
#     account_id = content$players[[i]]$account_id,
#     hero_id = content$players[[i]]$hero_id,
#     isRadiant = content$players[[i]]$isRadiant,
#     kills = content$players[[i]]$kills,
#     deaths = content$players[[i]]$deaths,
#     last_hits = content$players[[i]]$last_hits,
#     gold_per_min = content$players[[i]]$gold_per_min,
#     towers_killed = content$players[[i]]$towers_killed,
#     roshans_killed = content$players[[i]]$roshans_killed,
#     teamfight_participation = content$players[[i]]$teamfight_participation,
#     obs_placed = content$players[[i]]$obs_placed,
#     camps_stacked = content$players[[i]]$camps_stacked,
#     rune_pickups = content$players[[i]]$rune_pickups,
#     firstblood_claimed = content$players[[i]]$firstblood_claimed,
#     stuns = content$players[[i]]$stuns
# ), data.frame())


# as_tibble(t(do.call(cbind, content$players))) %>% View
#     select(match_id, account_id, hero_id, isRadiant, kills, deaths, last_hits, gold_per_min, towers_killed, roshans_killed, teamfight_participation, obs_placed, camps_stacked, rune_pickups, firstblood_claimed, stuns) %>%
#     mutate_all(~nth(.x, 1))
#     # mutate_all(as.numeric)

# content$players[[0]]$isRadiant

# get_scores(7028077898, matches, gsheet_matches) %>% View

for(match in filter(matches, !Updated)$MatchID) {
    players_results <- get_scores(match, matches, gsheet_matches)

    heroes <- read_csv("data/heroes_2023-02-22.csv")

    scores <- players_results %>%
            mutate(
                kills = kills * 0.3,
                deaths = 3 - deaths * 0.3,
                last_hits = last_hits * 0.003,
                gold_per_min = gold_per_min * 0.002,
                towers_killed = towers_killed * 1,
                roshans_killed = roshans_killed * 1,
                teamfight_participation = teamfight_participation * 3,
                obs_placed = obs_placed * 0.5,
                camps_stacked = camps_stacked * 0.5,
                rune_pickups = rune_pickups * 0.25,
                firstblood_claimed = firstblood_claimed * 4,
                stuns = stuns * 0.05
            ) %>%
            rowwise() %>%
            mutate(
                total = sum(c_across(kills:stuns))
            ) %>%
            select(hero_id, total)

    scores <- players_results %>%
        left_join(scores, c("hero_id" = "hero_id")) %>%
        left_join(select(players, PlayerID, Name), c("account_id" = "PlayerID")) %>%
        relocate(Name, .after = "account_id") %>%
        left_join(select(heroes, id, localized_name), c("hero_id" = "id")) %>%
        relocate(hero = localized_name, .after = "Name") %>%
        select(-hero_id)

    update_sheet_scores(scores, gsheet_scores)
}

# googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1FMNj3I8UKFDvkThuNdDZM2OGjofZ_kQotTsNqV4y9nI/edit#gid=0") %>%
#     select(PlayerID, Nome) %>%
#     inner_join(select(scores, account_id, total), c("PlayerID" = "account_id")) %>%View
