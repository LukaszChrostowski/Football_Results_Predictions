library(dplyr)
library(ggplot2)
library(tidyr)
library(lattice)

## merge data frames

## notice that from season 13/14 to 19/20 after 30 games, standings table was divided into two, championship group and inheritance group,
## For now we take into account only basic round (without division)
## without season 01/02 because of different league structure - to consider how to solve it 

DFF$season <- as.factor(DFF$season) # DFF is dataframe from standings_table.Rdata in data directory
comp_bal_df1 <- DFF %>% group_by(season) %>% summarise(sum_points = points/sum(points)) %>% as.data.frame()
comp_bal_df1 <- comp_bal_df1 %>% group_by(season) %>% summarise(hhicb = n() * sum(sum_points^2),
                                                                entropy = sum(sum_points * log(sum_points))/log(1/n()))
dff <- comp_bal_df1 %>% filter(season == "98/99" | season == "99/00")
dfff <- comp_bal_df1 %>% filter(!c(season == "98/99" | season == "99/00"))
H_entropy <- rbind(dff, dfff)

seasons <- c("98/99", "99/00", "00/01", "02/03", "03/04", "04/05", "05/06", "06/07",
             "07/08", "08/09", "09/10", "10/11", "11/12", "12/13", "13/14", "14/15",
             "15/16", "16/17", "17/18", "18/19", "19/20", "20/21", "21/22")

ggplot(data = H_entropy, aes(x = factor(season, level = seasons), y = hhicb)) +
  geom_point() +
  scale_x_discrete(breaks = c("00/01", "05/06", "10/11", "15/16", "20/21")) +
  xlab("Season") +
  ylab("HHICB") +
  ggtitle("HHICB statistic for assesing competitive balance")

ggplot(data = H_entropy, aes(x = factor(season, level = seasons), y = entropy)) +
  geom_point() +
  scale_x_discrete(breaks = c("00/01", "05/06", "10/11", "15/16", "20/21")) +
  xlab("Season") +
  ylab("Relative entropy") +
  ggtitle("Entropy statistic for assesing competitive balance")
