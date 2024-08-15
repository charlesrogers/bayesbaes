devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)

serieA_2020 <- fb_match_results(country = "ITA", gender = "M", season_end_year = 2020, tier = "1st")
serieA_2021 <- fb_match_results(country = "ITA", gender = "M", season_end_year = 2021, tier = "1st")



df.serieA_2020<- serieA_2020 %>%
  mutate(Wk=as.numeric(Wk))
df.training <- df.serieA_2020 %>%
  filter(Wk<11)

df.training.clean <- df.training %>%
  mutate(winner=case_when(
    HomeGoals>AwayGoals~Home,
    HomeGoals<AwayGoals~Away,
    TRUE~"Tie"),
    loser=case_when(HomeGoals<AwayGoals~Home,
    HomeGoals>AwayGoals~Away,
    TRUE~"Tie")) %>%
  pivot_longer(cols=c("winner","loser"),names_to = "outcome",values_to = "team")%>%
  filter(team!="Tie")%>%
  group_by(team)%>%
  count(outcome) %>%
  pivot_wider(names_from = "outcome",values_from = "n",values_fill = 0)%>%
  mutate(ties=10-sum(winner+loser),
         not_wins=sum(loser+ties)) %>%
  rename(wins=winner,
         losses=loser)%>%
  mutate(alpha=wins+4,
         beta=losses+4)

df.2020.test <- df.serieA_2020 %>%
  mutate(winner=case_when(
    HomeGoals>AwayGoals~Home,
    HomeGoals<AwayGoals~Away,
    TRUE~"Tie"),
    loser=case_when(HomeGoals<AwayGoals~Home,
                    HomeGoals>AwayGoals~Away,
                    TRUE~"Tie")) %>%
  pivot_longer(cols=c("winner","loser"),names_to = "outcome",values_to = "team")%>%
  filter(team!="Tie")%>%
  group_by(team)%>%
  count(outcome) %>%
  pivot_wider(names_from = "outcome",values_from = "n",values_fill = 0)%>%
  mutate(ties=38-sum(winner+loser),
         not_wins=sum(loser+ties)) %>%
  rename(wins=winner,
         losses=loser)%>%
  mutate(alpha=wins+4,
         beta=losses+4)