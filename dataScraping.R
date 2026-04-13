library(tidyverse)
library(rvest)
library(purrr)
library(httr)
library(tidygeocoder)
library(sf)
library(mapview)


# Scrape Data from Transfermarkt ---------------------------------------

## Transfers -------------

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




## Get Team Stadium, Total Market Value -----------------------

teamlinks <- transfers.scraped.all$teamlink %>% 
  gsub("transfers", "startseite", .) %>% 
  gsub("/saison.*", "", .) %>% 
  paste0("https://www.transfermarkt.us", .) %>% 
  unique()

stadiums <- tibble()

teamlinks <- teamlinks[!teamlinks %in% stadiums$link]

for (link in teamlinks) {
  
  page <- read_html_live(link)
  
  Sys.sleep(runif(1, 20, 27))
  
  stadium <- page %>% 
    html_nodes(".data-header__items+ .data-header__items .data-header__label+ .data-header__label .data-header__content > a") %>% 
    html_text()
  
  # totMarketVal <- page %>% 
  #   html_nodes(".data-header__market-value-wrapper") %>% 
  #   html_text()
  
  totMarketVal <- NA
  
  stadiums <- stadiums %>% 
    bind_rows(tibble(link, stadium, totMarketVal))
  
}


saveRDS(stadiums, "stadiumsRaw.rds")



## Standings ---------------

standings <- tibble()

for (leagueNum in 1:10) {
  
  # Subset league info
  league <- leagues[leagueNum]
  lID <- leagueID[leagueNum]
  
  for (year in 2022:2025) {
    
    # Create link and get page
    link <- paste0("https://www.transfermarkt.us/", league,"/tabelle/wettbewerb/", lID,"?saison_id=", year)
    print(link)
    
    Sys.sleep(runif(1, 10, 20))
    
    page <- read_html_live(link)
    
    
    tabTemp <- page %>% 
      html_table() %>% 
      .[[2]]
    
    colnames(tabTemp) <- c("Place", "NA1", "Club", "G", "W", "D", "L", "Goals", "PM", "Pts")
    
    teamlink <- page %>% 
      html_nodes("#yw1 .no-border-links a[title]") %>% 
      html_attr("href")
    
    tabTemp$teamlink <- teamlink
    
    logo <- page %>% 
      html_nodes("#yw1 .no-border-rechts img") %>% 
      html_attr("src")
    
    tabTemp$logo <- logo
    
    standings <- standings %>% 
      bind_rows(tabTemp)
  }
}


# saveRDS(standings, "standingsraw.rds")



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


# saveRDS(teamLeagues, "teamLeagues.rds")

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


# saveRDS(transfers.scraped.clean, "transfers_scraped_clean.rds")


## Get locations of clubs ---------------------

stadiums$club <- gsub("https://www.transfermarkt.us/", "", stadiums$link) %>% 
  gsub("/startseite.*", "", .)

stadiums$teamid <- str_extract(stadiums$link, "(?<=/verein/)\\d+")

countrycode <- c("GB", "DE", "FR", "ES", "IT",
              "PT", "TR", "NL", "BE", "RU")


# Get country assignments for clubs
countries <- transfers.scraped.clean %>% 
  select(from.id, league.from) %>% 
  filter(!is.na(league.from)) %>% 
  distinct() %>% 
  rowwise() %>% 
  mutate(country = countrycode[which(league.from == leagues)]) %>% 
  select(from.id, country) %>% 
  rename(teamid = from.id)


stadiums_geocoded <- stadiums %>% 
  left_join(countries)

stadiums_geocoded <- stadiums_geocoded %>% 
  mutate(stadiumCountry = paste0(stadium, ", ", country))


# Get lat lon for each club
stadiums_geocoded <- stadiums_geocoded %>% 
  geocode(stadiumCountry, method = "osm", lat = lat, long = lon)

### For 29 missing lat long use Claude ------
stadiums_geocoded_missing <- stadiums_geocoded %>% 
  filter(is.na(lat)) %>% 
  select(-lat, -lon)

lat <- c(
  47.7981,  # Stade de l'Abbé-Deschamps, Auxerre FR
  36.5271,  # JP Financial (Nuevo Mirandilla), Cadiz ES
  36.8482,  # UD Almería Stadium ES
  41.5853,  # Estádio FC Vizela PT
  41.7391,  # Estádio Municipal Eng.º Manuel Branco Teixeira, Chaves PT
  41.8360,  # Estádio C. J. de Almeida Freitas, Moreirense PT
  38.8721,  # Complexo Desportivo FC Alverca PT
  40.9893,  # Şükrü Saracoğlu, Fenerbahce TR
  36.5438,  # Alanya Oba Stadyumu TR
  41.0390,  # Beşiktaş Park TR
  36.3526,  # Sarıseki Fuat Tosyalı, Hatay TR
  41.0765,  # RAMS Park, Galatasaray TR
  37.0662,  # Gaziantep Stadyumu TR
  41.0264,  # Ümraniye Belediyesi Şehir Stadyumu TR
  41.0179,  # Esenyurt Necmi Kadıoğlu Stadyumu TR
  40.9029,  # Pendik Stadı TR
  37.0344,  # Bodrum İlçe Stadyumu TR
  40.7654,  # Kocaeli Stadyumu TR
  51.8942,  # De Kuip, Feyenoord NL
  51.9225,  # Sparta-stadion Het Kasteel NL
  50.5200,  # Stade du Paray, Seraing BE
  51.0833,  # Het Kuipje, Westerlo BE
  50.8667,  # Dender Football Complex BE
  43.4019,  # Fisht Olympic Stadium, Sochi RU
  45.0514,  # OZON Arena, Krasnodar RU
  56.3378,  # Nizhny Novgorod Stadium RU
  56.8389,  # Yekaterinburg Arena RU
  54.7104,  # Rostec Arena, Kaliningrad RU
  40.9167   # Çotanak Spor Kompleksi, Giresun TR
)

lon <- c(
  3.5673,  # Auxerre
  -6.2902,  # Cadiz
  -2.3645,  # Almería
  -8.3061,  # Vizela
  -7.4716,  # Chaves
  -8.4360,  # Moreirense
  -9.0317,  # Alverca
  29.0378,  # Fenerbahce
  32.0077,  # Alanya
  29.0097,  # Beşiktaş
  36.1664,  # Hatay
  28.7663,  # Galatasaray
  37.3833,  # Gaziantep
  29.1247,  # Ümraniye
  28.6611,  # Esenyurt
  29.2364,  # Pendik
  27.4317,  # Bodrum
  29.9273,  # Kocaeli
  4.5230,  # Feyenoord
  4.4664,  # Sparta Rotterdam
  5.4983,  # Seraing
  4.9167,  # Westerlo
  4.0500,  # Dender
  39.9570,  # Sochi
  38.9722,  # Krasnodar
  43.9625,  # Nizhny Novgorod
  60.5933,  # Yekaterinburg
  20.5128,  # Kaliningrad
  38.3833   # Giresun
)

stadiums_geocoded_missing$lat <- lat
stadiums_geocoded_missing$lon <- lon

# One mistake 
stadiums_geocoded$lat[stadiums_geocoded$stadiumCountry == "Diaz Arena, BE"] <- 51.22
stadiums_geocoded$lon[stadiums_geocoded$stadiumCountry == "Diaz Arena, BE"] <- 2.88

# Rejoin
stadiums_sf <- stadiums_geocoded %>% 
  filter(!is.na(lat), !is.na(lon)) %>% 
  bind_rows(stadiums_geocoded_missing) %>% 
  filter(!is.na(lat), !is.na(lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Check
mapview(stadiums_sf, zcol = "country")


# Compute distance matrix
dist_matrix <- st_distance(stadiums_sf)

dist_matrix <- as.numeric(dist_matrix) %>%
  matrix(nrow = nrow(stadiums_sf)) / 1000

rownames(dist_matrix) <- stadiums_sf$teamid
colnames(dist_matrix) <- stadiums_sf$teamid

# saveRDS(dist_matrix, "dist_matrix.rds")


## Clean standings data  ---------------

standings$teamid <- str_extract(standings$teamlink, "(?<=/verein/)\\d+")

standings <- standings %>% 
  group_by(teamid) %>% 
  summarise(avgPts = mean(Pts), logo = first(logo)) 


nodedataexternal <- standings %>% 
  inner_join(stadiums_sf %>% 
               select(teamid, totMarketVal, geometry))


parse_market_val <- function(x) {
  num  <- str_extract(x, "[\\d\\.]+(?=k|m|bn)") %>% as.numeric()
  unit <- case_when(
    str_detect(x, "bn") ~ 1000,
    str_detect(x, "m")  ~ 1,
    str_detect(x, "k")  ~ 0.001,
    TRUE                 ~ NA_real_
  )
  num * unit
}

nodedataexternal <- nodedataexternal %>%
  mutate(totMarketVal = parse_market_val(totMarketVal))

nodedataexternal$totMarketVal[is.na(nodedataexternal$totMarketVal)] <- min(nodedataexternal$totMarketVal, na.rm = TRUE)

# saveRDS(nodedataexternal, "Data/nodedataexternal.rds")

# Filter down to euro top 10 network ---------------------------------------

transfers.euro <- transfers.scraped.clean %>% 
  filter(!is.na(league.from), !is.na(league.to))


## Remove duplicate moves 
transfers.euro <- transfers.euro %>% 
  select(-from, -to) %>% 
  distinct()


# write_rds(transfers.euro, "transfers.euro.rds")



