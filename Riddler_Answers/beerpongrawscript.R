# Chess Match Problem ----



## Question 2
percent_chance <- function(selection){
  score <- NULL
  med_games <- NULL
  set.seed(45)
  for(games in selection){
    for (match in 1:10000){
      pts <- sample(x = c(1 , .5, 0), replace = TRUE, prob = c(.2, .65, .15), size = games)
      score <- append(score, sum(pts) > games/2)
    }
    med_games <- append(med_games, mean(score))
  }
  data.frame(games_played = selection,
             med_games) 
}

percent_chance(70:110) # about 95 games to get to 75% chance
percent_chance(240:265) # about 260 games to get to 90% chance
percent_chance(750:800) # About 795 games to get to 99%











# Beer Pong Reimagined ----

# Function for determining how many balls are left at the end of each round
play_pong <- function(round){
  x <- sample(x = c(1, 0), replace = TRUE, size = N, prob = c(1/N1, 1- 1/N1))
  N <- N - sum(x) 
  return(N)
}

# Playing games for N between 1 and 20
med_rounds <- NULL
for (pong_size in 1:40){ 
  
  # Playing 1000 games for each different number of cups
  rounds_played <- NULL
  for ( i in 1:10000){
    N <- pong_size
    N1 <- pong_size
    set.seed(i*100)
    
    # Determining how many rounds it takes for each game
    i <- 0
    while( N > 0){
      i <- i + 1
      N <- play_pong(i) 
    }
    rounds_played <- append(rounds_played, i)
  }
  # Calculating the median number of rounds it took for each number of cup
  med_rounds <- append(med_rounds, median(rounds_played))
  
}

# Looking at impact of adding cups
dat<- data.frame(rd_num = 1:40,
                 med_rounds)

library(ggplot2)
ggplot(data = dat, aes(x = rd_num, y = med_rounds))+
  geom_line()
dat$med_rounds - lead(med_rounds)
mean(dat$med_rounds - lead(med_rounds), na.rm = TRUE)



