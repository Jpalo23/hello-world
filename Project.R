#Load in needed packages
library(tidyverse)

#Load in combine data
combine_data = read.csv("combine.csv")

#Clean Data
fortyyd_data = filter(combine_data, fortyyd > 0)
twentyss_data = filter(combine_data, twentyss > 0)
broad_data = filter(combine_data, broad > 0)
vertical_data = filter(combine_data, vertical > 0)
bench_data = filter(combine_data, bench > 0)
three_cone_data = filter(combine_data, threecone > 0)
nfl_grade_data = filter(combine_data, nflgrade > 0)
college_data = filter(combine_data, college > 0)

#Group_by Data
players_by_year = group_by(combine_data, year)
players_by_position = group_by(combine_data, position)
players_by_college = group_by(college_data, college)
players_by_year_position = group_by(combine_data, year, position)
players_by_college_position = group_by(college_data, college, position)

#Avg over different Categories
avg_weight = round(mean(combine_data$weight),2)
avg_height = round(mean(combine_data$heightinchestotal),2)
avg_40 = format(round(mean(fortyyd_data$fortyyd),2), nsmall = 2)
avg_20 = format(round(mean(twentyss_data$twentyss),2), nsmall = 2)
avg_three_cone = format(round(mean(three_cone_data$threecone),2), nsmall = 2)
avg_vertical = round(mean(vertical_data$vertical),2)
avg_broad = round(mean(broad_data$broad),2)
avg_bench = round(mean(bench_data$bench),2)
avg_nfl_grade= round(mean(nfl_grade_data$nflgrade),2)

#Number of players from 1999-2015
num_players = nrow(combine_data)

#Number of players by year
num_players_by_year = count(players_by_year)

#Number of players by year graphed
ggplot(players_by_year, aes(year))+
  geom_bar()+
  ggtitle('Number of players by year')

#Number of players by position
num_players_by_position = count(players_by_position)

#Number of players by position graphed
ggplot(players_by_position, aes(position))+
  geom_bar()+
  coord_flip()+
  ggtitle('Number of players by position')

#Number of players by school
num_players_by_college = count(players_by_college)

#Top Ten Schools with players in the combine
Top_Ten_College = arrange(num_players_by_college, desc(n))[1:10, ]

#Number of players by year and position
num_players_by_year_position = count(players_by_year_position)

#Top 10 over different Categories
Best_Ten_40 = select(arrange(fortyyd_data, fortyyd), year, name, position, college, fortyyd)[1:10, ]
Best_Ten_20 = select(arrange(twentyss_data, twentyss), year, name, position, college, twentyss)[1:10, ]
Best_Ten_three_cone = select(arrange(three_cone_data, threecone), year, name, position, college, threecone)[1:10, ]
Best_Ten_vertical = select(arrange(vertical_data, desc(vertical)), year, name,position, college, vertical)[1:10, ]
Best_Ten_broad = select(arrange(broad_data, desc(broad)), year, name, position, college, broad)[1:10, ]
Best_Ten_bench = select(arrange(bench_data, desc(bench)), year, name, position, college, bench)[1:10, ]
Best_Ten_nfl_grade = select(arrange(nfl_grade_data, desc(nflgrade)), year, name, position, college, nflgrade)[1:10, ]

#Bottom 10 over different Categories
Worst_Ten_40 = select(arrange(fortyyd_data, desc(fortyyd)), year, name, position, college, fortyyd)[1:10, ]
Worst_Ten_20 = select(arrange(twentyss_data, desc(twentyss)), year, name, position, college, twentyss)[1:10, ]
Worst_Ten_three_cone = select(arrange(three_cone_data, desc(threecone)), year, name, position, college, threecone)[1:10, ]
Worst_Ten_vertical = select(arrange(vertical_data, vertical), year, name,position, college, vertical)[1:10, ]
Worst_Ten_broad = select(arrange(broad_data, broad), year, name, position, college, broad)[1:10, ]
Worst_Ten_bench = select(arrange(bench_data, bench), year, name, position, college, bench)[1:10, ]
Worst_Ten_nfl_grade = Best_Ten_nfl_grade = select(arrange(nfl_grade_data, nflgrade), year, name, position, college, nflgrade)[1:10, ]

#Heights and Weight Plot
ggplot(combine_data, aes(weight, heightinchestotal))+
  geom_smooth()+
  xlab('lbs')+
  ylab('inches')+
  ggtitle('Height and Weight')

#Players by Year and Position Graphed
ggplot(combine_data, aes(year))+
  geom_bar(aes(fill = position))+
  ggtitle('Players by Year and Position')

#Position Distribution Graph
ggplot(combine_data, aes(position))+
  geom_bar(aes(y = ..prop.., group = 1))+
  ggtitle('Position Distribution')

#Does a player's position directly affect the players combine performance?
#The 40 yard dash
ggplot(fortyyd_data, aes(reorder(position, fortyyd), fortyyd))+
  geom_boxplot()+
  ggtitle('40 Yard Dash Times by Position')+
  ylab('Seconds')+
  xlab('Position')
#The 20 yard shuffle
ggplot(twentyss_data, aes(reorder(position, twentyss), twentyss))+
  geom_boxplot()+
  ggtitle('20 Yard Shuffle Times by Position')+
  ylab('Seconds')+
  xlab('Position')
#The Three Cone 
ggplot(three_cone_data, aes(reorder(position, threecone), threecone))+
  geom_boxplot()+
  ggtitle('Three Cone Times by Position')+
  ylab('Seconds')+
  xlab('Position')
#The Vertical Jump
ggplot(vertical_data, aes(reorder(position, vertical), vertical))+
  geom_boxplot()+
  ggtitle('Vertical Distance by Position')+
  ylab('Inches')+
  xlab('Position')
#The Broad Jump
ggplot(broad_data, aes(reorder(position, broad), broad))+
  geom_boxplot()+
  ggtitle('Broad Jump Distance by Position')+
  ylab('Inches')+
  xlab('Position')
#The Bench Press
ggplot(bench_data, aes(reorder(position, bench), bench))+
  geom_boxplot()+
  ggtitle('Bench Press Reps by Position')+
  ylab('Reps')+
  xlab('Position')

#Does a player's weight and height directly affect the players combine performance?
#The 40 Yard Dash by Weight
ggplot(fortyyd_data, aes(weight, fortyyd))+
  geom_point()+
  geom_smooth()+
  ggtitle('40 Yard Dash by Weight')+
  ylab('Seconds')+
  xlab('Weight')
#The 40 Yard Dash by Height
ggplot(fortyyd_data, aes(heightinchestotal, fortyyd))+
  geom_point()+
  geom_smooth()+
  ggtitle('40 Yard Dash by Height')+
  ylab('Seconds')+
  xlab('Height')
#The 20 Yard Shuffle by Weight
ggplot(twentyss_data, aes(weight, twentyss))+
  geom_point()+
  geom_smooth()+
  ggtitle('20 Yard Shuffle by Weight')+
  ylab('Seconds')+
  xlab('Weight')
#The 20 Yard Shuffle by Height
ggplot(twentyss_data, aes(heightinchestotal, twentyss))+
  geom_point()+
  geom_smooth()+
  ggtitle('20 Yard Shuffle by Height')+
  ylab('Seconds')+
  xlab('Height')
#The Three Cone by Weight
ggplot(three_cone_data, aes(weight, threecone))+
  geom_point()+
  geom_smooth()+
  ggtitle('Three Cone by Weight')+
  ylab('Seconds')+
  xlab('Weight')
#The Three Cone by Height
ggplot(three_cone_data, aes(heightinchestotal, threecone))+
  geom_point()+
  geom_smooth()+
  ggtitle('Three Cone by Height')+
  ylab('Seconds')+
  xlab('Height')
#The Vertical Jump by Weight
ggplot(vertical_data, aes(weight, vertical))+
  geom_point()+
  geom_smooth()+
  ggtitle('Vertical Jump by Weight')+
  ylab('Inches')+
  xlab('Weight')
#The Vertical Jump by Height
ggplot(vertical_data, aes(heightinchestotal, vertical))+
  geom_point()+
  geom_smooth()+
  ggtitle('Vertical Jump by Height')+
  ylab('Inches')+
  xlab('Height')
#The Broad Jump by Weight
ggplot(broad_data, aes(weight, broad))+
  geom_point()+
  geom_smooth()+
  ggtitle('Broad Jump by Weight')+
  ylab('Inches')+
  xlab('Weight')
#The Broad Jump by Height
ggplot(broad_data, aes(heightinchestotal, broad))+
  geom_point()+
  geom_smooth()+
  ggtitle('Broad Jump by Height')+
  ylab('Inches')+
  xlab('Height')
#The Bench Press by Weight
ggplot(bench_data, aes(weight, bench))+
  geom_point()+
  geom_smooth()+
  ggtitle('Bench Press by Weight')+
  ylab('Reps')+
  xlab('Weight')
#The Bench Press by Height
ggplot(bench_data, aes(heightinchestotal, bench))+
  geom_point()+
  geom_smooth()+
  ggtitle('Bench Press by Height')+
  ylab('Reps')+
  xlab('Height')

#Do certain colleges generate better players overall and by position based off of Avg NFL Grade?
#Best College by NFL Grade
college_grade_data = filter(college_data, nflgrade > 0)
players_by_college_nflgrade = group_by(college_grade_data, college)
college_nflgrade_avg = summarise(players_by_college_nflgrade, avg_grade = mean(nflgrade))
Top_Ten_College_NFLgrade = arrange(college_nflgrade_avg, desc(avg_grade))[1:10, ]
#Best College by Position for NFL Grade
players_by_college_position_nflgrade = group_by(college_grade_data, college, position)
college_position_nflgrade_avg = summarise(players_by_college_position_nflgrade, avg_grade = mean(nflgrade))
Top_Ten_Colleges_by_Position_NFLgrade = arrange(college_nflgrade_avg, desc(avg_grade))[1:10, ]

#How many players fall within the Top 10% of all combine stats by year, and an avg per year?

#How have combine stats been trending over the years?
#The 40 Yard Dash
ggplot(fortyyd_data, aes(year, fortyyd))+
  geom_smooth()+
  ggtitle('40 Yard Dash Times by Year')+
  ylab('Seconds')+
  xlab('Year')
#The 20 Yard Shuffle
ggplot(twentyss_data, aes(year, twentyss))+
  geom_smooth()+
  ggtitle('20 Yard Shuffle Times by Year')+
  ylab('Seconds')+
  xlab('Year')
#The Three Cone
ggplot(three_cone_data, aes(year, threecone))+
  geom_smooth()+
  ggtitle('Three Cone Times by Year')+
  ylab('Seconds')+
  xlab('Year')
#The Vertical Jump
ggplot(vertical_data, aes(year, vertical))+
  geom_smooth()+
  ggtitle('Vertical Jump Distance by Year')+
  ylab('Inches')+
  xlab('Year')
#The Broad Jump 
ggplot(broad_data, aes(year, broad))+
  geom_smooth()+
  ggtitle('Broad Jump Distance by Year')+
  ylab('Inches')+
  xlab('Year')
#The Bench Press
ggplot(bench_data, aes(year, bench))+
  geom_smooth()+
  ggtitle('Bench Press Reps by Year')+
  ylab('Reps')+
  xlab('Year')
