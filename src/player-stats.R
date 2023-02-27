library(tidyverse)
library(httr)
library(googlesheets4)
library(fmsb)


shannon <- function(x) {
    p_x <- as.vector(table(x)) / 20
    h <- -sum(p_x * log(p_x))

    return(h)
}


gmean <- function(...) {
    return(exp(mean(log(c(...)))))
}


gsheet_players <- "https://docs.google.com/spreadsheets/d/1FMNj3I8UKFDvkThuNdDZM2OGjofZ_kQotTsNqV4y9nI"

players <- read_sheet(gsheet_players)

heroes <- read_csv("data/heroes_2023-02-22.csv")

account_id <- players$PlayerID[1]
url <- "https://api.opendota.com/api/players/"

response <- GET(paste0(url, account_id, "/recentMatches"))
content <- content(response)

content[[1]]$match_id,

response2 <- GET("https://api.opendota.com/api/matches/7029610092")
content2 <- content(response2)

player <- tibble(
    account_id = content2$players[[1]]$account_id,
    hero_id = content2$players[[1]]$hero_id,
    kda = content2$players[[1]]$kda,
    hero_damage = content2$players[[1]]$hero_damage,
    gold_per_min = content2$players[[1]]$gold_per_min,
    net_worth = content2$players[[1]]$net_worth,
    last_hits = content2$players[[1]]$last_hits,
    xp_per_min = content2$players[[1]]$xp_per_min,
    tower_damage = content2$players[[1]]$tower_damage,
    kills_per_min = content2$players[[1]]$kills_per_min,
    hero_healing = content2$players[[1]]$hero_healing
)

player %>%
    left_join(select(heroes, id, roles), c("hero_id" = "id")) %>%
    mutate(
        sup_hero = as.integer("Support" %in% roles),
        .keep = "unused"
    ) %>%
    mutate(
        account_id = account_id,

        hero_id = hero_id,

        hero_damage = ifelse(50e3 - hero_damage < 0, 50e3, hero_damage) / 50e3,
        kda = ifelse(10 - kda < 0, 10, kda) / 10,

        gold_per_min = ifelse(700 - gold_per_min < 0, 700, gold_per_min) / 700,
        net_worth = ifelse(40e3 - net_worth < 0, 40e3, net_worth) / 40e3,
        last_hits = ifelse(700 - last_hits < 0, 700, last_hits) / 700,
        xp_per_min = ifelse(40e3 - xp_per_min < 0, 40e3, xp_per_min) / 40e3,

        tower_damage = ifelse(20000 - tower_damage < 0, 20000, tower_damage) / 20000,

        hero_healing_p = ifelse(20e3 - hero_healing < 0, 20e3, hero_healing) / 20e3,
        sup_hero_p = sum(sup_hero) / 20,
        .keep = "none"
    ) %>%
    summarise(
        account_id = as.character(account_id),
        versatility = shannon(hero_id) / shannon(1:20),
        fighting = gmean(mean(hero_damage), mean(kda)),
        farming = gmean(mean(gold_per_min), mean(net_worth), mean(last_hits), mean(xp_per_min)),
        pushing = gmean(mean(last_hits), mean(tower_damage)),
        support = gmean(mean(hero_healing_p), mean(sup_hero_p))
    ) %>%
    bind_rows(
        tibble(account_id = "max", versatility = 1, fighting = 1, farming = 1, pushing = 1, support = 1),
        tibble(account_id = "min", versatility = 0, fighting = 0, farming = 0, pushing = 0, support = 0),
        .
    ) %>%
    column_to_rownames("account_id") %>%
    radarchart(
        pcol = "orange",
        pfcol = alpha("orange", 0.5),
        plwd = 2
    )
    # pivot_longer(cols = c(versatility:support)) %>%
    radarchart()
    ggplot() +
    geom_line(aes(name, value, group = account_id)) +
    coord_polar()



gmean(1, 2)
