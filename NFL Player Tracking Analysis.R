#### This dataset comes from the NFL's Big Data Bowl
### I only consider the opening game from the 2017 season
## T am performing a spatial analysis that will have an expanding scope
# more information on the dataset can be found at https://github.com/nfl-football-ops/Big-Data-Bowl

#load data
library(dplyr)
library(ggplot2)
library(lubridate)
opener <- read.csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017090700.csv")
plays <- read.csv("https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv")

#save files for later
write.csv(plays, "PlayerTrackingData.csv")
write.csv(plays, "PlayData")

#general EDA
names(opener)
str(opener)
names(plays)

#Check how many plays during the game
plays_keep <- plays %>%
                  filter(gameId == 2017090700)
str(plays_keep)
unique(plays_keep$playId) #177 plays

#combine based on playId
nfl = opener %>%
          left_join(plays_keep, by = "playId")

#check the new structure
str(nfl)
dim(nfl) (316025, 40)
summary(nfl)
head(nfl)
nfl$time <- ymd_hms(nfl$time) #convert to date


### I want to get a better sense of the data, so I'll filter for a player and a play
#Anthony Sherman test for first play
sherman <- nfl %>%
  filter(displayName == "Anthony Sherman",
         playId ==44)

#Structure Check
str(sherman)
dim(sherman) #(98,40)

min(sherman$time) #00:41:59
max(sherman$time) #00:42:08
#play lasted 9 seconds, with 98 location readings


# total distance
sum(sherman$dis) #Sherman travelled 58.4 yards on this play
sherman$x[1] #41.6 original x location
sherman$y[1] #16.5 original y location
tail(sherman$x,1) #82.7 final x location
tail(sherman$y,1) #43.4 final y location

### Here a simple function will be used to calculate distance a player travelled from the beginning of a play to end
#distance function  -- will be used later in other functions
distance <- function(x1,y1,x2,y2){
  sqrt((x2-x1)^2 + (y2-y1)^2)
}

#check the function
distance(sherman$x[1],sherman$y[1],tail(sherman$x,1),tail(sherman$y,1)) #correct


### I want to create a function to return the euclidean distance from start to finish, the total distance, and a play description

### The intention is to combine this with a loop in later analysis


#function to return euclidean distance by play from start to end and total distance
player_search <- function(player_name,play){
  filtered <- filter(nfl,displayName == player_name, playId == play) #filter first
  ls <- c(filtered$x[1],filtered$y[1],tail(filtered$x,1),tail(filtered$y,1)) #create list to use in distance function
  euc_dist <- distance(ls[1],ls[2],ls[3],ls[4]) #euc. distance
  total_dist <- sum(filtered$dis) #total distance
  dist_list <- c(euc_dist,total_dist, as.character(filtered$playDescription[1]))
  return(dist_list)
}

#check function
player_search('Anthony Sherman',44) #correct
player_search('Tyreek Hill',680) #correct


##### Next plan is to separate players by team and get plays they were in on and apply the player_search function

#get unique player list for KC -- get syntax ready for loop
player_name_kc <- nfl %>%
                   filter(team == "away") %>%
                   distinct(displayName, playId)
#check
str(player_name_kc) 
filter(player_name_kc, displayName == "Kareem Hunt") 

###kc add euclidean/total distance & play
lst <- c()
for(i in 1:1946){
  lst[[i]] <- player_search(player_name_kc$displayName[i],player_name_kc$playId[i]) #applies player_search to every player & play for KC
}

for(i in 1:1946){
  player_name_kc$euc.dist[[i]] <- lst[[i]][1] 
  player_name_kc$tot.dist[[i]] <- lst[[i]][2]       # these add metrics to the kc player dataframe from lst
  player_name_kc$description[[i]] <- lst[[i]][3]
}

#check data
summary(player_name_kc)
head(player_name_kc)

player_name_kc$euc.dist<- as.numeric(player_name_kc$euc.dist) #convert to numeric
player_name_kc$tot.dist <- as.numeric(player_name_kc$tot.dist) #convert to numeric
player_name_kc$home <- 0 #signify away team
summary(player_name_kc)


### repeat above process for NE
player_name_ne <- nfl %>%
                    filter(team == "home") %>%
                     distinct(displayName, playId) 
#check
str(player_name_ne)
filter(player_name_ne, displayName == "Tom Brady") 

# kc add euclidean/total distance & play
lst_ne <- c()
for(i in 1:length(player_name_ne$displayName)){
  lst_ne[[i]] <- player_search(player_name_ne$displayName[i],player_name_ne$playId[i])
}

for(i in 1:length(player_name_ne$displayName)){
  player_name_ne$euc.dist[[i]] <- lst_ne[[i]][1]
  player_name_ne$tot.dist[[i]] <- lst_ne[[i]][2]
  player_name_ne$description[[i]] <- lst_ne[[i]][3]
}

summary(player_name_ne)
head(player_name_ne)

player_name_ne$euc.dist<- as.numeric(player_name_ne$euc.dist) #convert to numeric
player_name_ne$tot.dist <- as.numeric(player_name_ne$tot.dist) #convert to numeric
player_name_ne$home <- 1 #assign home team
summary(player_name_ne)


### simple EDA here to compare datasets

#combine NE & KC
player_name_comb <- bind_rows(player_name_kc,player_name_ne)
player_name_comb$home <- as.factor(player_name_comb$home)
summary(player_name_comb)

#plot checks total distance travelled
ggplot(player_name_comb, aes(x = tot.dist, fill = home))+ #distribution looks similar
  geom_histogram(bins = 50) +
  facet_grid(~home)


#see the players who ran most yards
total_dist <- player_name_comb %>%
                      group_by(displayName) %>%
                      summarise(yards = sum(tot.dist)) %>%
                      arrange(desc(yards))
  
head(total_dist,5) #Daniel Sorensen(2317), Jordan Richards(2164), Devin McCourty(1746), Kyle Van Noy(1732), Chris Hogan(1676)

##############################################################
### Analysis here is to see average separation from all players on a given play
## Starting place for more detailed analysis later

# Testing a pipe to use later in function so it could be expanded --KC
check_kc <- filter(nfl,team == 'away', playId == 44) %>%
            select(x, y, team, displayName, playId, possessionTeam) %>%
            distinct(displayName,possessionTeam,x,y) %>%
            group_by(displayName) %>%
            mutate(ball = ifelse(possessionTeam == 'KC',1,0)) %>% #checks if team is home or away
            summarise(y_avg = mean(y), #Avg
                      x_avg = mean(x), #Avg
                      possession  = max(ball))
check_kc


# NE
check_ne <- filter(nfl,team == 'home', playId == 846) %>%
             select(x, y, team, displayName, playId, possessionTeam) %>%
             distinct(displayName, possessionTeam, x, y) %>%
             group_by(displayName) %>%
             mutate(ball = ifelse(possessionTeam == 'NE',1,0)) %>%
             summarise(y_avg = mean(y),
                       x_avg = mean(x),
                       possession  = max(ball))
check_ne



### function where inputs are home and away teams, will compute a 11x11 matrix that will compute each players average
  #distance from player on the other team

highest_avg <- c() # to be used in loops
tot_avg <- c() # to be used in loops
max_avg_dist <- function(home,away){
      if(home$possession[1] == 0){   ## if  away team has ball they will be on the rows of dataframe and get avg distance from every defender calculated
  
      for(i in 1:11){
        highest_avg[[i]] <- sqrt((home$x_avg-away$x_avg[i])^2 + (home$y_avg-away$y_avg[i])^2)
      }
      for(i in 1:11){
       names(highest_avg[[i]]) <- home$displayName[i] 
       
     } 
       dd  <-  as.data.frame(matrix(unlist(highest_avg), nrow=length(unlist(highest_avg[1]))))
       colnames(dd) <- home$displayName
       rownames(dd) <- away$displayName
       tot_avg <- rowMeans(dd)
       results <- c(dd, tot_avg)
      return(results)} else{ 
        ## same as above, but only if home team has the ball
        for(i in 1:11){
          highest_avg[[i]] <- sqrt((home$x_avg-away$x_avg[i])^2 + (home$y_avg-away$y_avg[i])^2)
        }
        for(i in 1:11){
          names(highest_avg[[i]]) <- home$displayName[i]
          
        } 
        dd  <-  as.data.frame(matrix(unlist(highest_avg), nrow=length(unlist(highest_avg[1]))))
        colnames(dd) <- away$displayName
        rownames(dd) <- home$displayName
        tot_avg <- rowMeans(dd)
        results <- c(dd, tot_avg)
        return(results)
      }
}

play_846 <- max_avg_dist(check_ne,check_kc) #check.. it works!
### still need to extract to find players with largest average separation


##### The next goal is to loop through every play and utilize the above function to find players that are most separated on average by play


#distinct player list
dist_plays <- nfl %>%
                distinct(playId)
dist_plays


# loop to go through each play
#*** loop has yet to be completed
home_team <- c()
away_team <- c()
for(i in 1:length(dist_plays)){
    home_team[[i]] <- filter(nfl,team == 'home', playId == dist_plays[i]) %>%
        select(x, y, team, displayName, playId, possessionTeam) %>%
        distinct(displayName, possessionTeam, x, y) %>%
        group_by(displayName) %>%
        mutate(ball = ifelse(possessionTeam == 'NE',1,0)) %>%
        summarise(y_avg = mean(y),
                  x_avg = mean(x),
                  possession  = max(ball))
    
    away_team[[i]] <- filter(nfl,team == 'away', playId == dist_plays[i]) %>%
      select(x, y, team, displayName, playId, possessionTeam) %>%
      distinct(displayName, possessionTeam, x, y) %>%
      group_by(displayName) %>%
      mutate(ball = ifelse(possessionTeam == 'KC',1,0)) %>%
      summarise(y_avg = mean(y),
                x_avg = mean(x),
                possession  = max(ball))

}





### Analysis is not complete more work coming soon
