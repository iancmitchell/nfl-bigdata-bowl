###################################################################################################
# whaynes
# 10/30/2020
#
# Classify offensive plays by their route schemes. Apply clustering
###################################################################################################

library(magrittr)
library(dplyr)
#library(odbc)
library(stringr)
library(readxl)
library(xlsx)
library(tidyr)
library(cluster)
library(factoextra)


table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)
setwd('/mnt/76A8B9E0A8B99F55/Projects/nfl-bigdata-bowl/data')
options(stringsAsFactors = F)

## read in the data
games = read.csv('games.csv')
players = read.csv('players.csv')
plays = read.csv('plays.csv')

set.seed(142)

## read in these files one at a time
combined_routes = data.frame()

for (i in list.files(pattern = 'week')) {
  this_week = read.csv(i)

  route_rollup = this_week %>% filter(route != "") %>% select(gameId,playId,displayName,route) %>% 
                 unique() %>% group_by(gameId,playId,route) %>% 
                 summarise(Count = n()) %>% as.data.frame()#%>% spread(key = 'route',value = 'Count',fill = 0)
  
  combined_routes = rbind(combined_routes,route_rollup)
  rm(this_week) 
}



## roll up to get route combinations
play_route_summary = combined_routes %>% group_by(gameId,playId) %>% 
                     summarise(Route_Combo = paste0(rep(route,Count),collapse = "|")) %>% as.data.frame()


## merge back on plays
## excluding ones where routes not named
plays_with_route_combo = merge.data.frame(plays,play_route_summary %>% filter(!grepl('undefined',Route_Combo)))


## which route combos are most effective?
completion_rollup = plays_with_route_combo %>% group_by(Route_Combo) %>% 
                    summarise(Passes = n(),
                              Completed = sum(passResult == 'C'),
                              AvgGain = mean(offensePlayResult),
                              AvgGainOnCompletion = mean(offensePlayResult[passResult == 'C']),
                              mean_epa = mean(epa))
completion_rollup$AvgGainOnCompletion[is.nan(completion_rollup$AvgGainOnCompletion)] = NA
completion_rollup$Pct = completion_rollup$Completed / completion_rollup$Passes

## have a column for each possible route
completion_rollup_factorize = completion_rollup %>% 
                              mutate(Route = str_split(Route_Combo,"\\|")) %>% 
                              unnest(Route) %>% group_by(Route_Combo,Passes,Completed,AvgGain,AvgGainOnCompletion,
                                                         mean_epa,Route,Pct) %>% summarise(Count = n()) %>% 
                              spread(key = 'Route',value = 'Count',fill = 0) %>% as.data.frame()

completion_rollup_factorize$Num_Routes = apply(completion_rollup_factorize[,c(8:19)],1,sum)

## perform silhouette method to find optimal number of clusters
df <- completion_rollup_factorize[,c(8:19)]
fviz_nbclust(df, kmeans, method='silhouette', k.max=20)

## generate clusters
num_clusters <- 10
clusters = kmeans(df, num_clusters)

completion_rollup_factorize$Cluster = clusters$cluster


## analyze cluster output
cluster_analysis = completion_rollup_factorize %>% select(-Route_Combo) %>%  
                   group_by(Cluster) %>% 
                   summarise(TotalPasses = sum(Passes),
                             TotalCompleted = sum(Completed),
                             AvgGain = sum(AvgGain * Passes)/sum(Passes),
                             AvgGainOnCompletion = sum(AvgGainOnCompletion * Completed,na.rm = T)/sum(Completed),
                             mean_epa = sum(Passes * mean_epa)/sum(Passes),
                             Pct = sum(Passes * Pct) / sum(Passes),
                             Num_Routes = sum(Passes * Num_Routes)/sum(Passes),
                             ANGLE = sum(ANGLE * Passes) / sum(Passes),
                             CORNER = sum(CORNER * Passes) / sum(Passes),
                             CROSS = sum(CROSS * Passes) / sum(Passes),
                             FLAT = sum(FLAT * Passes) / sum(Passes),
                             GO = sum(GO * Passes) / sum(Passes),
                             HITCH = sum(HITCH * Passes) / sum(Passes),
                             IN = sum(IN * Passes) / sum(Passes),
                             OUT = sum(OUT * Passes) / sum(Passes),
                             POST = sum(POST * Passes) / sum(Passes),
                             SCREEN = sum(SCREEN * Passes) / sum(Passes),
                             SLANT = sum(SLANT * Passes) / sum(Passes),
                             WHEEL = sum(WHEEL * Passes) / sum(Passes)
                             )%>% as.data.frame()


output = merge.data.frame(completion_rollup_factorize[,c("Route_Combo","Passes","Cluster")],
                          cluster_analysis)
output_dir <- "clustering"
if (!dir.exists(output_dir)) {
  dir.create("clustering")
}

## the output file that powers the tableau report
write.csv(output,file = './clustering/cluster_results.csv',row.names = F)





