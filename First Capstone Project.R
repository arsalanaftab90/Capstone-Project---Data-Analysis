library(dplyr)
library(tidyverse)
par(mar=c(5,4.1,1,2.1))
setwd("E:/Data Set/ipl")
getwd()


ball_by_ball = read.csv("deliveries.csv",stringsAsFactors = TRUE,head = TRUE,sep ="," )
str(ball_by_ball)
matches_data = read.csv("matches.csv",stringsAsFactors = TRUE,head = TRUE,sep ="," )
str(matches_data)
merge_data = merge(ball_by_ball,matches_data, by.x = 'match_id', by.y = 'id')




filter_merged_data = filter(merge_data, dismissal_kind == "hit wicket" | dismissal_kind =="bowled" | 
dismissal_kind =="lbw" | dismissal_kind =="stumped" | dismissal_kind =="caught" | dismissal_kind =="caught and bowled", 
season == 2019 | season == 2018 | season == 2017 | season == 2016)


balls1 = merge_data %>% 
filter(season == 2019 | season == 2018 | season == 2017 | season == 2016) %>% 
group_by(bowler) %>% count()


#Batsman total runs
Batsman_total_runs = merge_data %>% 
  select(batsman,batsman_runs) %>% 
  group_by(batsman) %>%
  summarise(Runs=sum(batsman_runs))

#Count of balls faced by batsman
No_of_balls_faced = merge_data %>% 
  select(batsman) %>% count(batsman,wide_runs = 0) %>% 
  group_by(batsman)
  colnames(No_of_balls_faced)[3] = "Balls_faced_by_batsman"

#Count of innings in which batsman got out
  Batsman_outs = merge_data  %>% select(player_dismissed) %>% 
    group_by(player_dismissed) %>% count()
  colnames(Batsman_outs)[2] = "dismissals"

  
#matches played by batsman
  total_matches_played_by_batsman = merge_data %>% 
    group_by (batsman) %>% 
    distinct(match_id) %>% 
    count()
  

#merging data of batsman runs & no of balls faced 
  batsman_runs_balls_faced = merge(Batsman_total_runs,No_of_balls_faced)

#merging data of batsman runs,balls faced & total matches played
  matches_runs_ballsfaced_mat_played = merge(batsman_runs_balls_faced,total_matches_played_by_batsman)
colnames(matches_runs_ballsfaced_mat_played)[5] = "Matches_played"


working_data=merge_data %>%
  mutate(across(, ~ifelse(.=="", NA, as.character(.))))
View(working_data)

dismissed = working_data %>% select(player_dismissed) %>% group_by(player_dismissed) %>% 
  count() %>% na.omit()
colnames(dismissed)[2] = "Dismissals"
colnames(dismissed)[1] = "batsman"


Seasons_played_by_batsman = working_data %>% select(batsman,season) %>% 
  group_by(batsman) %>% distinct(season) %>% count()


#merging data of batsman runs,balls faced,matches played & dismissed

Final_batsman_data = full_join(matches_runs_ballsfaced_mat_played,dismissed,by="batsman")
  
Final_batsman_season = full_join(Final_batsman_data,Seasons_played_by_batsman,by="batsman") %>% 
  mutate(Dismissals = replace_na(Dismissals, 0)) %>% 
  mutate(non_dismissals = (Matches_played - Dismissals)) %>% 
  mutate(Batsman_average = Runs/Dismissals) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  mutate(Strike_rate = (Runs/Balls_faced_by_batsman)*100) %>% 
  filter(Batsman_average>= 30 | Strike_rate >= 130)

#Batsman wicket against each bowler 
batsman_wk_bowler = working_data %>% select(batsman,bowler,Bowlers.Category,player_dismissed,dismissal_kind) %>% 
group_by(batsman,player_dismissed,dismissal_kind) %>% 
filter(player_dismissed!="NA" & dismissal_kind != "hit wicket" &
dismissal_kind != "retired hurt" & dismissal_kind != "run out" & 
dismissal_kind != "obstructing the field")

#Highest wicket taker bowler
highest_wik_taker = working_data %>% 
  select(bowler,Bowlers.Category,dismissal_kind) %>% 
  group_by(bowler,Bowlers.Category,dismissal_kind) %>%
  filter(dismissal_kind != "NA") %>% count() 
colnames(highest_wik_taker)[4] = "Wickets" 

 HWT = highest_wik_taker %>% select(bowler,Bowlers.Category,Wickets) %>% 
   group_by(bowler,Bowlers.Category) %>% 
  summarise(wickets_taken = sum(Wickets))
 
 
#Bowlers average
bowlers__runs_data = merge_data %>% 
  select(bowler,Bowlers.Category,total_runs)%>% 
  group_by(bowler,Bowlers.Category) %>% 
  mutate(total_runs=sum(total_runs)) %>% distinct()

Bowlers_wkts_data = merge_data %>% 
  filter(dismissal_kind == "caught"|dismissal_kind == "hit wicket"|dismissal_kind == "caught and bowled"|
           dismissal_kind == "bowled"|dismissal_kind =="stumped"|dismissal_kind=="lbw") %>% 
    select(bowler,dismissal_kind) %>% group_by(bowler) %>% count()

Final_bowler_data = full_join(bowlers__runs_data,Bowlers_wkts_data,by="bowler")
colnames(Final_bowler_data)[4] = "Wickets_taken"

bowlers_delieveries_bowled = merge_data %>% select(bowler) %>% 
  group_by(bowler) %>% count()
colnames(bowlers_delieveries_bowled)[2] = "deliveries"


Bowlers_final_data = full_join(Final_bowler_data,bowlers_delieveries_bowled,by="bowler") %>% 
mutate(Wickets_taken = replace_na(Wickets_taken, 0)) %>% 
  mutate(Bowling_average = total_runs / Wickets_taken) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  mutate(bowling_economy_rate = (total_runs/deliveries)*6) %>% 
  mutate(bowler_strike_rate = deliveries/Wickets_taken) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) 

Seasons_played_by_bowler = working_data %>% select(bowler,season) %>% 
  group_by(bowler) %>% distinct(season) %>% count() 
colnames(Seasons_played_by_bowler)[2]="Seasons"

Final_bowler_season = full_join(Bowlers_final_data,Seasons_played_by_bowler,by="bowler")
colnames(Final_batsman_season)[7]="Seasons"













rm(ball_by_ball)
rm(balls1)
rm(Batsman_outs)
rm(batsman_runs_balls_faced)
rm(Batsman_total_runs)
rm(Batsman_total_runs)
rm(batsman_wk_bowler)
rm(bowlers__runs_data)
rm(bowlers_data)
rm(bowlers_delieveries_bowled)
rm(Bowlers_wkts_data)
rm(dismissed)
rm(filter_merged_data)
rm(Final_batsman_data)
rm(highest_wik_taker)
rm(No_of_balls_faced)
rm(Seasons_played_by_batsman)
rm(total_matches_played_by_batsman)
rm(matches_runs_ballsfaced_mat_played)
rm(Seasons_played_by_bowler)
rm(Final_bowler_data)
rm(Bowlers_final_data)



#Strategy against batsman in death overs !

#Best batsman while chasing in death overs
bat_chasing = merge_data %>% 
  filter(inning == 2,over==16|over==17|over==18|over==19|over==20,season == 2019|season == 2018|season==2017|season==2016|season==2015) %>% 
  select(batsman,batsman_runs) %>% group_by(batsman) %>% 
  summarise(batsman_runs_in_death_overs=sum(batsman_runs))

bats_total_run = merge_data %>% 
  filter(inning == 2) %>% 
  select(batsman,batsman_runs) %>% group_by(batsman) %>% 
  summarise(batsman_runs_oveall = sum(batsman_runs))

balls_faced_by_batsman_in_death_overs = merge_data %>%
filter(inning == 2,over==16|over==17|over==18|over==19|over==20,season == 2019|season == 2018|season==2017|season==2016|season==2015) %>% 
select(batsman) %>% group_by(batsman) %>% count()
colnames(balls_faced_by_batsman_in_death_overs)[2]="Balls_faced_in_death_overs"


Final_batsman_chasing = left_join(bat_chasing,bats_total_run,by="batsman") %>% 
mutate(Percentage_of_runs_in_death_overs = (batsman_runs_in_death_overs/batsman_runs_oveall)*100)

Batsman_runs_in_death_overs_last_5_season = full_join(Final_batsman_chasing,balls_faced_by_batsman_in_death_overs,by="batsman") 


rm(bat_chasing)
rm(bats_total_run)
rm(balls_faced_by_batsman_in_death_overs)
rm(Final_batsman_chasing)



#Best death over batsman dismissals against each category of bowler

Death_overs = merge_data %>% 
  filter(inning == 2,over==16|over==17|over==18|over==19|over==20,season == 2019|season == 2018|season==2017|season==2016|season==2015,player_dismissed!="",dismissal_kind !="run out"|dismissal_kind!="obstructing the field"|dismissal_kind!="retired hurt") %>%
select(player_dismissed,Bowlers.Category) %>% group_by(player_dismissed,Bowlers.Category) %>% 
  count()
colnames(Death_overs)[3]="dismissals"

 Batsman_dismissals_in_death_over_against_each_category = Death_overs %>%
   group_by(player_dismissed) %>%
  mutate(max_dismissals = max(dismissals)) %>%
  arrange(desc(max_dismissals)) %>%
  ungroup() %>% 
  filter(dismissals==max_dismissals) %>% 
   select(player_dismissed,Bowlers.Category,max_dismissals)
 rm(Death_overs)

  

#Most Valuable Batsman & Bowlers on the basis of runs & wickets season wise
MVP_bowler = merge_data %>% 
  filter(player_dismissed!="",dismissal_kind !="run out"|dismissal_kind!="obstructing the field"|dismissal_kind!="retired hurt") %>% 
  select(bowler,Bowlers.Category,season) %>% 
  group_by(bowler,season) %>%  count() 
colnames(MVP_bowler)[3]="wickets_taken"


MVP_top_5_bowler_season_wise = MVP_bowler %>% 
  group_by(season) %>% arrange(desc(wickets_taken)) %>%
  slice(1:5)
rm(MVP_bowler)

MVP_top_5_batsman = merge_data %>% 
  filter(batsman_runs!= "0") %>% 
  group_by(batsman,season) %>%
  mutate(totalscore = sum(total_runs)) %>% 
  select(batsman,season,totalscore) %>% 
  distinct() %>% group_by(season) %>% 
  arrange(desc(totalscore)) %>% slice(1:5)
  
rm(Death_overs)
rm(MVP_bowler) 
rm(MVP_batsman)

#Proportion of win when batting first and making 20% or more increase in death overs
df = working_data %>% 
filter(inning==1,over==16|over==17|over==18|over==19|over==20,batting_team == winner) %>% 
select(match_id,inning,batting_team,winner,total_runs)

Batting_first_winner = data.frame(df$match_id,df$inning,df$batting_team,df$winner,as.integer(df$total_runs))
colnames(Batting_first_winner)[1]="Match_id"
colnames(Batting_first_winner)[2]="inning"  
colnames(Batting_first_winner)[3]="batting_team"  
colnames(Batting_first_winner)[4]="winner_team"
colnames(Batting_first_winner)[5]="Total_runs"

rm(df)


Batting_first_winner_death_over = Batting_first_winner %>% 
  group_by(Match_id,inning,batting_team,winner_team) %>% 
  summarise(Total_runs = sum(Total_runs))
rm(Batting_first_winner)

df = working_data %>% 
  filter(inning==1,over==1|over==2|over==3|over==4|over==5|over==6|over==7|over==8|over==9|over==10|over==11|over==12|over==13|over==14|over==15,batting_team == winner) %>% 
  select(match_id,inning,batting_team,winner,total_runs)

Batting_first_winner = data.frame(df$match_id,df$inning,df$batting_team,df$winner,as.integer(df$total_runs))
colnames(Batting_first_winner)[1]="Match_id"
colnames(Batting_first_winner)[2]="inning"  
colnames(Batting_first_winner)[3]="batting_team"  
colnames(Batting_first_winner)[4]="winner_team"
colnames(Batting_first_winner)[5]="Total_runs"
rm(df)

Batting_first_winner_initial_overs = Batting_first_winner %>% 
  group_by(Match_id,inning,batting_team,winner_team) %>% 
  summarise(Total_runs = sum(Total_runs))
rm(Batting_first_winner)

colnames(Batting_first_winner_death_over)[5] = "Death_Over_Runs"
colnames(Batting_first_winner_initial_overs)[5] = "Initial_over_runs"

Death_over_probability_winning = left_join(Batting_first_winner_initial_overs,Batting_first_winner_death_over,by="Match_id")
rm(Batting_first_winner_death_over)
rm(Batting_first_winner_initial_overs)

Batting_first_Death_over_runrate = Death_over_probability_winning %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(Match_id,inning.x,winner_team.x,Initial_over_runs,Death_Over_Runs)
rm(Death_over_probability_winning)





Batting_first_Death_over_runrate_final = Batting_first_Death_over_runrate %>% 
mutate(initial_over_runrate = Initial_over_runs/15,death_over_runrate = Death_Over_Runs/5) %>% 
mutate(initital_over_runrate_plus_20_percent = (initial_over_runrate + (initial_over_runrate * 0.20))) %>% 
filter(death_over_runrate>=initital_over_runrate_plus_20_percent)  
  
         
         
#Proportion of win when batting second and making 20% or more increase in death overs

df = working_data %>% 
  filter(inning==2,over==16|over==17|over==18|over==19|over==20,batting_team == winner) %>% 
  select(match_id,inning,batting_team,winner,total_runs)

Batting_second_winner = data.frame(df$match_id,df$inning,df$batting_team,df$winner,as.integer(df$total_runs))
colnames(Batting_second_winner)[1]="Match_id"
colnames(Batting_second_winner)[2]="inning"  
colnames(Batting_second_winner)[3]="batting_team"  
colnames(Batting_second_winner)[4]="winner_team"
colnames(Batting_second_winner)[5]="Total_runs"

rm(df)


Batting_second_winner_death_over = Batting_second_winner %>% 
  group_by(Match_id,inning,batting_team,winner_team) %>% 
  summarise(Total_runs = sum(Total_runs))
rm(Batting_second_winner)

df = working_data %>% 
  filter(inning==2,over==1|over==2|over==3|over==4|over==5|over==6|over==7|over==8|over==9|over==10|over==11|over==12|over==13|over==14|over==15,batting_team == winner) %>% 
  select(match_id,inning,batting_team,winner,total_runs)

Batting_second_winner = data.frame(df$match_id,df$inning,df$batting_team,df$winner,as.integer(df$total_runs))
colnames(Batting_second_winner)[1]="Match_id"
colnames(Batting_second_winner)[2]="inning"  
colnames(Batting_second_winner)[3]="batting_team"  
colnames(Batting_second_winner)[4]="winner_team"
colnames(Batting_second_winner)[5]="Total_runs"
rm(df)

Batting_second_winner_initial_overs = Batting_second_winner %>% 
  group_by(Match_id,inning,batting_team,winner_team) %>% 
  summarise(Total_runs = sum(Total_runs))
rm(Batting_second_winner)

colnames(Batting_second_winner_death_over)[5] = "Death_Over_Runs"
colnames(Batting_second_winner_initial_overs)[5] = "Initial_over_runs"

Death_over_probability_winning = left_join(Batting_second_winner_initial_overs,Batting_second_winner_death_over,by="Match_id")
rm(Batting_second_winner_death_over)
rm(Batting_second_winner_initial_overs)

Batting_second_Death_over_runrate = Death_over_probability_winning %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  select(Match_id,inning.x,winner_team.x,Initial_over_runs,Death_Over_Runs)
rm(Death_over_probability_winning)





Batting_second_Death_over_runrate_final = Batting_second_Death_over_runrate %>% 
  mutate(initial_over_runrate = Initial_over_runs/15,death_over_runrate = Death_Over_Runs/5) %>% 
  mutate(initital_over_runrate_plus_20_percent = (initial_over_runrate + (initial_over_runrate * 0.20))) %>% 
  filter(death_over_runrate>=initital_over_runrate_plus_20_percent)

#contaigency table for conditional probability
Table_conditional_probability = matrix(c(208,124,332,62.65,112,245,357,31.37,293,396,689,"42.52"),
  nrow = 3,
  ncol=4,
  byrow = TRUE
)
 rownames(Table_conditional_probability)=c("batting_first_win","batting_second_win","Total_matches") 
colnames(Table_conditional_probability)=c("winner_with_20per_or_more_runs_death_overs","winner_with_20per_less_runs_in_death_overs","Total","Proportion_of_winning_by_with_20_or_more_run_rate_in_death_over")

  
#Applying logistic regression for win/loose by runs

logistic_regression_dismissals_in_first_innings = working_data %>%
  select(match_id,inning,player_dismissed) %>% 
  filter(player_dismissed != "NA",inning==1) %>% select(match_id,inning) %>% 
  group_by(match_id,inning) %>% count()
  
logistic_regression_dismissals_in_first_innings$match_id = as.integer(logistic_regression_dismissals_in_first_innings$match_id)
  logistic_regression_dismissals_in_first_innings$inning = as.integer(logistic_regression_dismissals_in_first_innings$inning)
  logistic_regression_dismissals_in_first_innings$n = as.integer(logistic_regression_dismissals_in_first_innings$n)
colnames(logistic_regression_dismissals_in_first_innings)[3] = "Wickets_fall_in_first_inning"
  
  
  logistic_regression = merge_data %>% 
  select(match_id,inning,batting_team,winner,total_runs) %>%
  filter(inning == 1) %>% 
  group_by(match_id,inning,batting_team,winner) %>% 
  summarise(total_runs = sum(total_runs)) %>% 
  mutate(Result=ifelse(batting_team == winner,yes = 1 ,no =0))
colnames(logistic_regression)[6] = "Result"

full_logistic= left_join(logistic_regression,logistic_regression_dismissals_in_first_innings,by.x="match_id",by.y="match_id")

logistic_final = full_logistic %>% ungroup() %>% 
  select(match_id,total_runs,Result,Wickets_fall_in_first_inning)
df = as.data.frame(full_logistic)
df$Result=as.factor(df$Result)

#splitting the data 
library(caTools)
split <- sample.split(df,SplitRatio = 0.8)
split
training <- subset(df, split=="TRUE")
testing <- subset(df,split=="FALSE")


my_model = glm(Result ~ total_runs + 
       Wickets_fall_in_first_inning,data = training,family ="binomial")

summary(my_model)

#Running the test data through the model

res = predict(my_model,testing,type = "response")
res

res = predict(my_model,training,type = "response")
res

#Validate the model - Confusion Matrix 
confmatrix = table(Actual_Value = training$Result,Predicted_Value = res>0.47)
confmatrix

#Accuracy of the model
(confmatrix[[1,1]]+confmatrix[[2,2]])/sum(confmatrix)

#how to be sure for threshold
#we will make ROC curve to find the optimum threshold 

res = predict(my_model,training,type = "response")


library(ROCR)
 ROCR_predictor = prediction(res,training$Result)
ROCR_performance = performance(ROCR_predictor,"tpr","fpr") 

par(mar=c(7,4,4,6.4))
plot(ROCR_performance,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

#Logit function to check probability
new = data.frame(total_runs = 200,Wickets_fall_in_first_inning = 5 )
predict(my_model,new,type = "response")




#Exporting data frames
library(dplyr)
library(openxlsx)
library(rio)
export(Final_batsman_season,"Question1a.batsman.xlsx")
export(Final_bowler_season,"Question1b.bowler.xlsx") 
export(Batting_first_Death_over_runrate_final,"Batting first death overs data.xlsx")
export(Batting_second_Death_over_runrate_final,"Batting second death over data.xlsx")
export(Batsman_runs_in_death_overs_last_5_season,"Batsman runs in death overs of last 5 season.xlsx")
export(Batsman_dismissals_in_death_over_against_each_category,"Batsman dismissals in death overs against each category.xlsx")
export(MVP_top_5_batsman,"MVP batsman.xlsx")
export(MVP_top_5_bowler_season_wise,"MVP bowler.xlsx")

hist(Final_batsman_season$Strike_rate)
hist(Final_batsman_season$Batsman_average)
hist(Final_bowler_season$Bowling_average)
hist(Final_bowler_season$bowling_economy_rate)
hist(Final_bowler_season$bowler_strike_rate)
