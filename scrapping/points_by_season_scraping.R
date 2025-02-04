##################################################################################
######################### POINTS AT THE END OF THE SEASON PER CLUB ##################
###############################################################################

# load packages
library(tidyverse)
library(RSelenium)
library(netstat)

# start the server

# rs_driver_object <- rsDriver(browser = 'chrome',
#                              chromever = "106.0.5249.21",
#                              verbose = FALSE,
#                              port = free_port())

rs_driver_object <- rsDriver(browser = "firefox",
                             #verbose = FALSE,
                             #chromever = "131.0.6778.265", # the lastest
                             port = free_port())

# create a client object
remDr <- rs_driver_object$client

# open a browser
remDr$open()

# maximize window
remDr$maxWindowSize()

urls <- list("https://www.wyniki.pl/pko-bp-ekstraklasa-1998-1999/#/YR3njUNd/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-1999-2000/#/CI2jklw3/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2000-2001/#/fshel8h9/table/overall",
             #"https://www.wyniki.pl/pko-bp-ekstraklasa-2001-2002/#/27e3nnNL/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2002-2003/#/6VG0sp0k/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2003-2004/#/IJWnEkJL/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2004-2005/#/zesrFV3F/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2005-2006/#/d4rvGBl9/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2006-2007/#/lhGsCgZ2/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2007-2008/#/A1HwDZJd/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2008-2009/#/EgDgrOO2/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2009-2010/#/SdHcsrv9/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2010-2011/#/r5SUjkae/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2011-2012/#/zXrc8SIB/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2012-2013/#/4hOhngVE/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2013-2014/#/MFbMedql/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2014-2015/#/CUfFTpEE/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2015-2016/#/86mTH0UC/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2016-2017/#/hYVk7Jqe/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2017-2018/#/bZWyoJnA/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2018-2019/#/dhoVcL5r/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2019-2020/#/v5p2SRke/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2020-2021/#/0YeuAKmU/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2021-2022/#/noYKsAu8/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2022-2023/#/4fofM1vn/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa-2023-2024/#/EsvRI4zf/table/overall",
             "https://www.wyniki.pl/pko-bp-ekstraklasa/#/Qu6CIhxL/table/overall")

### by loop ###
points <- c()
place <- c()
team <- c()

for (i in 1:length(urls)) { # without season 01/02

  remDr$navigate(urls[[i]])

  #cookies <- remDr$findElement(using = "xpath", '//*[@id="onetrust-reject-all-handler"]')
  #cookies$clickElement()

  Tabela <- remDr$findElement(using = 'class', 'standings_table')
  Tabela$clickElement()

  miejsce <- remDr$findElements(using = "class", "tableCellRank")
  miejsca <- lapply(miejsce, function(x) x$getElementText()) %>% unlist()

  Teams <- remDr$findElements(using = "class", "tableCellParticipant__name")
  Team <- lapply(Teams, function(x) x$getElementText() %>% unlist()) %>% unlist()

  Points <- remDr$findElements(using = "class", "table__cell--points")
  Points <- lapply(Points, function(x) x$getElementText()) %>% unlist()
  Points <- as.numeric(Points[-1])

  points <- append(points, Points)
  team <- append(team, Team)
  place <- append(place, miejsca)
  print(i)
  print(length(miejsca))
}

seasons <- c("98/99", "99/00", "00/01", "02/03", "03/04", "04/05", "05/06", "06/07",
             "07/08", "08/09", "09/10", "10/11", "11/12", "12/13", "13/14", "14/15",
             "15/16", "16/17", "17/18", "18/19", "19/20", "20/21", "21/22", "22/23",
             "23/24", "24/25")

season <- rep(seasons, c(rep(16, 4), rep(14, 2), rep(16, 16), rep(18, 4)))

DFF <- data.frame(team = team,
                  place = place,
                  points = points,
                  season = season)

DFF$place <- DFF$place %>% str_replace_all("[.]", "") %>% as.numeric()
