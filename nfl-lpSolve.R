setwd("/Users/jasongoldrosen/Documents/Jason/nfl/") 
df <- read.csv("nfl_games_2019.csv")[df$playoff==0,]

df$game_id <- as.integer(rownames(df))


df.1 <- df[,c(1,2,5,9,13)]
df.2 <- df[,c(1,2,6,9,13)]

names(df.1)[3:4] <- c("team","elo_prob")
names(df.2)[3:4] <- c("team","elo_prob")
df.2$elo_prob <- 1 - df.2$elo_prob
df.long <- rbind(df.1,df.2)[order(df.long$game_id),]


df.long$date <- as.Date(df.long$date) 
df.long$dofw <- as.POSIXlt(df.long$date)$wday
df.long$date.2 <- df.long$date + (df.long$dofw == 4)*3 + (df.long$dofw == 1)*(-1) + (df.long$dofw == 6) + 1 
df.long$week <- as.integer((df.long$date.2-as.Date('2019-09-02'))/7)


# k <- 1
# for (i in list(df.1,df.2)){
#   names(i)[3:4] <- c("team","elo_prob")
#   if (k == 2){ i$elo_prob <- 1 - i$elo_prob}
#   print(head(i))
#   k <- k + 1  
# }

length(unique(df.long$team)) + length(unique(df.long$week))


const <- matrix(rep(0,25088), 
                nrow=49, 
                ncol=512)
i <- 1
for (t in unique(df.long$team)){
const[i,] <- df.long$team == t
i <- i + 1
}

for (t in unique(df.long$week)){
  const[i,] <- df.long$week == t
  i <- i + 1
}
obj <- df.long$elo_prob
direction <- c(rep("<=",length(unique(df.long$team))),rep("=",length(unique(df.long$week))))
rhs <- rep(1,nrow(const))

library(lpSolve)

test <- lp(direction = "max", 
           objective.in = obj, 
           const.mat = const, 
           const.dir = direction, 
           const.rhs = rhs)

solns <- df.long[test$solution!=0,c("week","team","elo_prob")]

solns$prob <- cumprod(solns$elo_prob)

print(solns)


ggplot(solns, aes(x=week,y=prob)) + 
  geom_point() + 
  geom_line() + 
  labs(title="Cumulative Survival Probability",x ="NFL Game Week", y = "")
