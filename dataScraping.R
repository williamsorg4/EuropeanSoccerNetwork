library(tidyverse)
library(rvest)
library(purrr)
library(httr)


# Scrape Data from Transfermarkt ---------------------------------------

## Leagues of interest
leagues <- c("premier-league", "bundesliga", "ligue-1", "laliga", "serie-a",
             "liga-portugal", "super-lig", "eredivisie", "jupiler-pro-league",
             "premier-liga")

leagueID <- c("GB1", "L1", "FR1", "ES1", "IT1",
              "PO1", "TR1", "NL1", "BE1", "RU1")


## Scrape loop
transfers.scraped.all <- tibble()

for (leagueNum in 1:10) {
  
  # Subset league info
  league <- leagues[leagueNum]
  lID <- leagueID[leagueNum]
  
  for (year in 2022:2025) {
    
    # Create link and get page
    link <- paste0("https://www.transfermarkt.us/", league,"/transfers/wettbewerb/", lID,"/plus/?saison_id=", year, "&s_w=&leihe=1&intern=0")
    print(link)
    
    Sys.sleep(runif(1, 10, 20))
    
    page <- read_html_live(link)

    Sys.sleep(runif(1, 10, 20))
    
    # All tables of in and out transfers by club
    INandOUT <- page %>%
      html_table()
    
    ## Remove irrelevant table
    INandOUT <- INandOUT[2:length(INandOUT)]
    
    
    # Get header team names and their links
    teams <- page %>%
      html_nodes("#tm-main a+ a") %>%
      html_text()
    
    teamlink <- page %>%
      html_nodes("#tm-main a+ a") %>%
      html_attr("href")
    
    # Get destination/prev team links from tables
    leftjoincol <- page %>% 
      html_nodes(".verein-flagge-transfer-cell")
    
    leftjoinlinks <- leftjoincol %>% 
      html_node("a") %>% 
      html_attr("href")

    
    # Collapse list of tables into one
    teams <- rep(teams, each = 2)
    teamlink <- rep(teamlink, each = 2)
    direction <- rep(c("IN", "OUT"), times = length(teams) / 2)


    INandOUT <- lapply(INandOUT, function(x) {
      names(x) <- make.names(names(x), unique = TRUE)
      x
    })


    df_list <- map2(INandOUT, seq_along(INandOUT), function(tbl, i) {
      tbl %>%
        mutate(
          team = teams[i],,
          teamlink = teamlink[i],
          direction = direction[i]
        )
    })


    transfersubset <- bind_rows(df_list)

    transfersubset$leftjoinlink <- leftjoinlinks
    transfersubset$year <- year
    transfersubset$league <- league

    transfers.scraped.all <- transfers.scraped.all %>%
      bind_rows(transfersubset)

  }
}


saveRDS(transfers.scraped.all, "transfers_scraped_all_raw.rds")



# Clean the data ------------------------------------------------

## Remove rows that were scraped multiple times
transfers.scraped.all <- transfers.scraped.all %>% 
  distinct()


## Fill in to and from based on direction of move
transfers.scraped.clean <- transfers.scraped.all %>% 
  mutate(player = ifelse(is.na(In), Out, In),
         from = ifelse(is.na(Left.1), team, Left.1),
         to = ifelse(is.na(Joined.1), team, Joined.1))

## Fill in to-from team ids
transfers.scraped.clean$teamid <- str_extract(transfers.scraped.clean$teamlink, "(?<=/verein/)\\d+")
transfers.scraped.clean$leftjoinid <- str_extract(transfers.scraped.clean$leftjoinlink, "(?<=/verein/)\\d+")

transfers.scraped.clean <- transfers.scraped.clean %>% 
  mutate(from.id = ifelse(is.na(Left.1), teamid, leftjoinid),
         to.id = ifelse(is.na(Joined.1), teamid, leftjoinid))



## Clean market value and translate to euros
transfers.scraped.clean <- transfers.scraped.clean %>% 
  mutate(Market.value = case_when(
    Market.value == "-" ~ NA,
    str_detect(Market.value, "m") ~ as.numeric(str_remove_all(Market.value, "[€m]")) * 1e6,
    str_detect(Market.value, "k") ~ as.numeric(str_remove_all(Market.value, "[€k]")) * 1e3
  ))


## Clean fees and transfer type
transfers.scraped.clean <- transfers.scraped.clean %>% 
  mutate(
    fee_type = case_when(
      Fee == "free transfer" ~ "free",
      Fee %in% c("?", "-", "") ~ "unknown",
      Fee == "loan transfer" ~ "loan",
      str_detect(Fee, "^End of loan") ~ "loan_end",
      str_detect(Fee, "^Loan fee:") ~ "loan_fee",
      str_detect(Fee, "^€") ~ "transfer",
      .default = "unknown"
      ),
    fee_clean = str_remove(Fee, ".*€"), 
    fee = case_when(
      fee_type == "free" ~ 0,
      str_detect(fee_clean, "m") ~ as.numeric(str_remove(fee_clean, "m")) * 1e6,
      str_detect(fee_clean, "k") ~ as.numeric(str_remove(fee_clean, "k")) * 1e3,
      !is.na(fee_clean) & fee_clean != Fee ~ as.numeric(fee_clean),
      .default = NA
    )
    )


## Clean names to remove first initial, last name -------
nameplaceholder <- transfers.scraped.clean$player %>% 
  str_split("\\. ")


nameplaceholder <- lapply(nameplaceholder, function (x) {
  if (length(x) == 1) {
    return(x)
  }
  else {
    x <- x[1:length(x) - 1]
    x <- paste0(x, collapse = "")
    x <- sub(".$", "", x)
  }
})


transfers.scraped.clean$player <- nameplaceholder %>% unlist()


## Team-league assignments
teamLeagues <- transfers.scraped.clean %>% 
  select(team, teamid, league) %>% 
  distinct()


## Filter down to relevant cols
transfers.scraped.clean <- transfers.scraped.clean %>% 
  select(from, from.id, to, to.id, player, Age, year, Position, Pos, 
         fee_type, fee, Market.value)


## Join league cols
transfers.scraped.clean <- transfers.scraped.clean %>% 
  left_join(teamLeagues %>% select(-team), 
            join_by(from.id == teamid)) %>% 
  rename(league.from = league) %>% 
  left_join(teamLeagues %>% select(-team), 
            join_by(to.id == teamid)) %>% 
  rename(league.to = league)

saveRDS(transfers.scraped.clean, "transfers_scraped_clean.rds")



# Filter down to euro top 10 network ---------------------------------------

transfers.euro <- transfers.scraped.clean %>% 
  filter(!is.na(league.from), !is.na(league.to))


## Remove duplicate moves 
transfers.euro <- transfers.euro %>% 
  select(-from, -to) %>% 
  distinct()


write_rds(transfers.euro, "transfers.euro.rds")



