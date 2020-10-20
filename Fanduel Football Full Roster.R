print(paste('Start Time:',Sys.time()))
library(tidyverse)
library(tictoc)
tic('Run time')

players <- read.csv('football.csv')
players <- players %>% filter(Injury.Indicator == '' & Played > 0) %>%
  select(Id, Position, Nickname, FPPG, Played, Team, Opponent, Salary, Game)
players$Value <- players$FPPG / (players$Salary / 1000)

# Rankings
# Join defense ratings of opponents to offense players
d_rank <- players %>% filter(Position == 'D') %>% arrange(FPPG)
d_rank$Rank <- nrow(d_rank):1
d_rank <- d_rank[,c('Team', 'Rank')]
colnames(d_rank) <- c('Opponent','Matchup')
offense <- players %>% filter(Position != 'D')
offense <- left_join(offense, d_rank, by = 'Opponent')

# Join offense ratings of opponents to defense teams
o_team_rank <- players %>% filter(Position != 'D' & FPPG >= mean(FPPG)) %>% group_by(Team) %>%
  summarize(TeamFPPG = mean(FPPG)) %>% arrange(TeamFPPG)
o_team_rank$Rank <- nrow(o_team_rank):1
o_team_rank <- o_team_rank[,-2]
colnames(o_team_rank) <- c('Opponent','Matchup')
defense <- players %>% filter(Position == 'D')
defense <- left_join(defense, o_team_rank, by = 'Opponent')

players <- rbind(offense, defense)

qb <- players %>% filter(Position == 'QB')
rb <- players %>% filter(Position == 'RB')
wr <- players %>% filter(Position == 'WR')
te <- players %>% filter(Position == 'TE')
def <- players %>% filter(Position == 'D')

qb <- qb %>% filter(FPPG >= mean(FPPG))
rb <- rb %>% filter(FPPG >= mean(FPPG))
wr <- wr %>% filter(FPPG >= mean(FPPG))
te <- te %>% filter(FPPG >= mean(FPPG))
flex <- rbind(rb,wr,te)
def <- def %>% filter(FPPG >= mean(FPPG))

lineup_comb <- data.frame(matrix(ncol = 15, nrow = 0))
colnames(lineup_comb) <- c('QB','RB1','RB2','WR1','WR2','WR3',
                           'TE','FLEX','DEF','Salary','FPPG','Matchup','Score','Value','Dup')

for (i in 1:20000) {
  p1 <- qb[sample(nrow(qb),1), ]
  p23 <- rb[sample(nrow(rb),2), ]
  p46 <- wr[sample(nrow(wr),3), ]
  p7 <- te[sample(nrow(te),1), ]
  p8 <- flex[sample(nrow(flex),1), ]
  p9 <- def[sample(nrow(def),1), ]
  lineup <- rbind(p1,p23,p46,p7,p8,p9)
  lineup_check <- lineup %>% group_by(Team) %>% mutate(Count = n())
  if(max(lineup_check$Count) < 5){
    new_row <- data.frame(QB = paste(p1$Id,p1$Nickname,sep = ':'),
                          RB1 = paste(p23$Id[1],p23$Nickname[1],sep = ':'),
                          RB2 = paste(p23$Id[2],p23$Nickname[2],sep = ':'),
                          WR1 = paste(p46$Id[1],p46$Nickname[1],sep = ':'),
                          WR2 = paste(p46$Id[2],p46$Nickname[2],sep = ':'),
                          WR3 = paste(p46$Id[3],p46$Nickname[3],sep = ':'),
                          TE = paste(p7$Id,p7$Nickname,sep = ':'),
                          FLEX = paste(p8$Id,p8$Nickname,sep = ':'),
                          DEF = paste(p9$Id,p9$Nickname,sep = ':'),
                          Salary = sum(lineup$Salary),
                          FPPG = sum(lineup$FPPG),
                          Matchup = sum(lineup$Matchup) * 0.75,
                          Value = sum(lineup$Value)/2)
    new_row$Score <- as.numeric(new_row$FPPG) + as.numeric(new_row$Matchup)
    new_row[t(apply(new_row,1,duplicated))] <- NA
    if(any(is.na(new_row))){
      next } else {
        if(new_row$Salary <= 60000){
          lineup_comb <- rbind(lineup_comb,new_row)
        }
    }
  }
}

lineup_comb <- lineup_comb[order(-lineup_comb$Score),1:14]
top50 <- lineup_comb[1:50,]
write.csv(top50, 'optimal_football_fanduel.csv')
rm(list = ls())
print(paste('End Time:',Sys.time()))
toc()