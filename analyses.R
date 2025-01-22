# analyses and graphs for escape room

library(tidyverse)
library(lmerTest)
library(brms)
library(bmlm)
library(ggpubr)
library(corrplot)

setwd("/Users/benjaminsilver/Google Drive/My Drive/Grad School/Projects/Escape_Room/manuscript/submissions/")

######### CLEAN #############
pre <- read_csv('sharing_github/pre_clean.csv')
post <- read_csv('sharing_github/post_clean.csv')
week <- read_csv('sharing_github/week_clean.csv')

change <- pre %>% 
  rename_at(vars(-c(ID,Age,Gender,Race,Group,Teammate,
                    contains("Pred"))),
            ~ paste0(.,"_pre")) %>% 
  left_join(post %>% 
              rename_at(vars(-c(ID,Group,Teammate,
                                contains("Pred"))),
                        ~ paste0(.,"_post")), 
            by = c("ID","Group","Teammate")) %>% 
  left_join(week %>% 
              rename_at(vars(-c(ID,Group,Teammate,
                                contains("Pred"))),
                        ~ paste0(.,"_week")), 
            by = c("ID","Group","Teammate")) %>% 
  pivot_longer(cols = c(starts_with("Morality"),
                        starts_with("Ability"),
                        starts_with("Sociability")),
               names_to = c("Dimension",".value"),
               names_sep = "_") %>% 
  mutate(postpreDiff.Real = post - pre,
         postpreDiff.Abs = abs(postpreDiff.Real),
         postpreDiff.d = ifelse(postpreDiff.Real <= 0, 0, 1),
         weekpostDiff.Real = week - post,
         weekpostDiff.Abs = abs(weekpostDiff.Real),
         weekpostDiff.d = ifelse(weekpostDiff.Real <= 0, 0, 1),
         Interact_pre.r = 7 - Interact_pre,
         Familiar_pre = (Interact_pre.r+FirstMeet_pre)/2,
         Familiar_pre.c = scale(Familiar_pre, center = T, 
                                scale = F),
         Like_pre.c = scale(Like_pre, center = T, scale = F),
         Closeness_pre.c = scale(Closeness_pre, center = T, scale = F),
         Similar_pre.c = scale(Similar_pre, center = T, scale = F),
         Like_post.c = scale(Like_post, center = T, scale = F),
         Closeness_post.c = scale(Closeness_post, center = T, scale = F),
         Similar_post.c = scale(Similar_post, center = T, scale = F),
         Like_pre.cs = scale(Like_pre,center = T, scale = T),
         Familiar_pre.cs = scale(Familiar_pre, center = T, scale = T),
         Closeness_pre.cs = scale(Closeness_pre,center = T, scale = T),
         Similar_pre.cs = scale(Similar_pre,center = T, scale = T),
         Like_post.cs = scale(Like_post,center = T, scale = T),
         Closeness_post.cs = scale(Closeness_post,center = T, scale = T)) %>% 
  select(-c(contains("Mastermind"),contains("ColoredLetters"),
            contains("Cables"),contains("FlyerCipher"),
            contains("Myspace"),contains("Maze")))

change1 <- change %>% 
  pivot_longer(cols = contains("Diff."),
               names_to = c("Comparison",".value"),
               names_sep = "Diff.") %>% 
  mutate(Comparison.d = case_when(
    Comparison == "postpre" ~ 0,
    Comparison == "weekpost" ~ 1
  ),
  Comparison.drev = case_when(
    Comparison == "postpre" ~ 1,
    Comparison == "weekpost" ~ 0
  ))

change2 <- change %>% 
  pivot_longer(cols = c("pre","post","week"),
               names_to = "Time",
               values_to = "Rating")


# get objective competence performance scores
video <- read_csv("sharing_github/video_data.csv")

video_sum <- video %>% 
  mutate(Puzzle = recode(Puzzle, EMPID = "flyer",
                         crumpet = "myspace")) %>% 
  group_by(Group) %>% 
  count(Subject) %>% 
  filter(Subject != 'None',
         Group != 28) %>% 
  rename(ParticipationObjective = "n") %>%
  rename(Teammate = "Subject")

# get objective sociability performance scores
transcript <- read_csv('sharing_github/transcript_info.csv')
transcript_sum <- transcript %>% 
  rename(Group = "group",Teammate = "participant") %>% 
  mutate(
    firstplural_pct = (firstplural/num_words)*100) %>% 
  filter(Teammate != "GM",Teammate != "GM2",
         Group != 28) %>% 
  group_by(Group) %>% 
  mutate(collab_ratio = ((firstplural + second)/sum((num_words)))*100) %>% 
  ungroup() %>% 
  mutate(collab_ratio2 = (firstplural + second)/first,
         collab_ratio2 = ifelse(is.infinite(collab_ratio2),NA,collab_ratio2))

change <- change %>% 
  left_join(video_sum) %>% 
  left_join(transcript_sum) %>% 
  mutate(ParticipationObjective = if_else(Teammate == '23A',0,ParticipationObjective),
         ParticipationObjective.cs = scale(ParticipationObjective, center = T, scale = T),
         collab_ratio.cs = scale(collab_ratio, center = T, scale = T),
         num_utterances.cs = scale(num_utterances, center = T, scale = T),
         collab_ratio2.cs = scale(collab_ratio2, center = T, scale = T))

###### demographics and descriptives ######
### demographics ######
# gender
change %>% 
  distinct(ID, .keep_all = T) %>% 
  count(Gender)

# age
change %>% 
  distinct(ID, .keep_all = T) %>% 
  summarize(mean(Age))

change %>% 
  distinct(ID, .keep_all = T) %>% 
  summarize(min(Age))

change %>% 
  distinct(ID, .keep_all = T) %>% 
  summarize(max(Age))

change %>% 
  distinct(ID, .keep_all = T) %>% 
  count(Age < 23)

# race
change %>% 
  distinct(ID, .keep_all = T) %>% 
  count(Race)

### descriptives #####
# means and sds
change_descriptives <- change %>% 
  distinct(ID, Teammate, .keep_all = T) %>% 
  summarize(Like_pre_sd = sd(Like_pre,na.rm = T),
            Interact_pre.r_sd = sd(Interact_pre, na.rm = T),
            FirstMeet_pre_sd = sd(FirstMeet_pre, na.rm = T),
            Similar_pre_sd = sd(Similar_pre,na.rm = T),
            Like_pre = mean(Like_pre, na.rm = T),
            Interact_pre.r = mean(Interact_pre, na.rm = T),
            FirstMeet_pre = mean(FirstMeet_pre, na.rm = T),
            Similar_pre = mean(Similar_pre, na.rm = T))

change %>% 
  group_by(Dimension) %>% 
  summarize(pre_mean = mean(pre,na.rm = T),
            pre_sd = sd(pre,na.rm = T),
            post_mean = mean(post,na.rm = T),
            post_sd = sd(post,na.rm = T),
            week_mean = mean(week,na.rm = T),
            week_sd = sd(week,na.rm = T))

# correlation matrix
change_corrmat <- change %>% 
  distinct(ID, Teammate, .keep_all = T) %>% 
  group_by(Group) %>% 
  mutate(Like_pre_gavg = mean(Like_pre, na.rm = T),
         Familiar_pre_gavg = mean(Familiar_pre, na.rm = T),
         Similar_pre_gavg = mean(Similar_pre, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Like_pre = Like_pre - Like_pre_gavg,
         Familiar_pre = Familiar_pre - Familiar_pre_gavg,
         Similar_pre = Similar_pre - Similar_pre_gavg) %>% 
  select(Like_pre,
         Similar_pre,
         Familiar_pre)
cor(change_corrmat, use="complete.obs")^2

## descriptive figures ####
# first met
library(tidygraph)
library(ggraph)

familiarity <- change %>% 
  distinct(ID,Teammate,.keep_all = T) %>% 
  select(ID,Group,Teammate, FirstMeet_pre) %>% 
  filter(!(Group == 14 & Teammate == '14E'),
         !(Group == 19 & Teammate == '19E'),
         !(Group == 21 & Teammate == '21D'),
         !(Group == 21 & Teammate == '21E'),
         !(Group == 27 & Teammate == '27C'),
         !(Group == 31 & Teammate == '31E'),
         !(Group == 34 & Teammate == '34E'),
         !(Group == 36 & Teammate == '36E')) %>% 
  mutate(ID = str_extract(ID, "[A-Za-z]"),
         Teammate = str_extract(Teammate, "[A-Za-z]"),
         Group = as.integer(factor(Group)),
         FirstMeet_pre = as.factor(FirstMeet_pre)) %>% 
  rename(from = ID, to = Teammate, group = Group, weight = FirstMeet_pre)

graph <- tbl_graph(edges = familiarity, directed = FALSE)

# Plot all groups using facets
ggraph(graph, layout = "circle") +
  geom_edge_link(aes(color = weight)) +
  geom_node_point(size = 2, color = 'black') +
  scale_edge_color_manual(values = RColorBrewer::brewer.pal(6, "Blues"),
                          labels = c("Past month","Past 6 months",
                                     "Past year","Past 3 years",
                                     "Past 5 years","More than 5 years ago")) +
  #scale_color_manual(values = c(`1` = "blue", `2` = "pink")) + 
  labs(edge_color = "First meet") +
  theme_void() +
  facet_wrap(~group, ncol = 5) +
  labs(title = "When did you first meet X?") +
  theme(plot.margin = unit(c(.5,.5,.5,.5),"lines"),
        plot.title = element_text(hjust = .5))
ggsave('figs/supp1.jpg',width = 5, height = 5)

familiarity <- change %>% 
  distinct(ID,Teammate,.keep_all = T) %>% 
  select(ID,Group,Teammate, Interact_pre) %>% 
  filter(!(Group == 14 & Teammate == '14E'),
         !(Group == 19 & Teammate == '19E'),
         !(Group == 21 & Teammate == '21D'),
         !(Group == 21 & Teammate == '21E'),
         !(Group == 27 & Teammate == '27C'),
         !(Group == 31 & Teammate == '31E'),
         !(Group == 34 & Teammate == '34E'),
         !(Group == 36 & Teammate == '36E')) %>% 
  mutate(ID = str_extract(ID, "[A-Za-z]"),
         Teammate = str_extract(Teammate, "[A-Za-z]"),
         Group = as.integer(factor(Group)),
         Interact_pre = as.factor(Interact_pre)) %>% 
  rename(from = ID, to = Teammate, group = Group, weight = Interact_pre)

graph <- tbl_graph(edges = familiarity, directed = FALSE)

# Plot all groups using facets
ggraph(graph, layout = "circle") +
  geom_edge_link(aes(color = weight)) +
  geom_node_point(size = 2, color = "black") +
  scale_edge_color_manual(values = RColorBrewer::brewer.pal(6, "Blues"),
                          labels = c("Less than several times/year",
                                     "Several times/year",
                                     "Several times/6 months",
                                     "Several times/month",
                                     "Several times/week","Every day")) +
  labs(edge_color = "Interaction frequency") +
  theme_void() +
  facet_wrap(~group, ncol = 5) +
  labs(title = "How often do you interact with X?") +
  theme(plot.margin = unit(c(.5,.5,.5,.5),"lines"),
        plot.title = element_text(hjust = .5))
ggsave('figs/supp2.jpg',width = 5.5, height = 5)


##### QUESTION 1 #######
## Models ####

mDirChangeComp1 <- brm(Rating ~ Time.d + 
                      (1 + Time.d | Group / ID),
                    data = change2 %>% 
                      filter(Dimension == 'Ability',
                             Time != 'week') %>% 
                      mutate(Time.d = if_else(Time == 'pre',0,1)),
                    cores = 4, chains = 2, iter = 6000)

mDirChangeComp2 <- brm(Rating ~ Time.d + 
                      (1 + Time.d | ID),
                    data = change2 %>% 
                      filter(Dimension == 'Ability',
                             Time != 'post') %>% 
                      mutate(Time.d = if_else(Time == 'pre',0,1)),
                    cores = 4, chains = 2, iter = 6000)

mDirChangeSoc1 <- brm(Rating ~ Time.d + 
                     (1 + Time.d | Group / ID),
                   data = change2 %>% 
                     filter(Dimension == 'Sociability') %>% 
                     mutate(Time.d = if_else(Time == 'pre',0,1)),
                   cores = 4, chains = 2, iter = 6000)

mDirChangeSoc2 <- brm(Rating ~ Time.d + 
                     (1 + Time.d | ID),
                   data = change2 %>% 
                     filter(Dimension == 'Sociability',
                            Time != 'post') %>% 
                     mutate(Time.d = if_else(Time == 'pre',0,1)),
                   cores = 4, chains = 2, iter = 6000)

mChangeComp1 <- brm(Abs ~ Comparison.d +
                      (1 + Comparison.d | Group / ID), 
                    data = change1 %>%
                      filter(Dimension == 'Ability'),
                    cores = 4, chains = 2, iter = 6000)

mChangeComp2 <- brm(Abs ~ Comparison.drev +
                      (1 + Comparison.drev | Group / ID), 
                    data = change1 %>%
                      filter(Dimension == 'Ability'),
                    cores = 4, chains = 2, iter = 6000)

mChangeSoc1 <- brm(Abs ~ Comparison.d +
                      (1 + Comparison.d | Group / ID), 
                    data = change1 %>%
                      filter(Dimension == 'Sociability'),
                    cores = 4, chains = 2, iter = 6000)

mChangeSoc2 <- brm(Abs ~ Comparison.drev +
                      (1 + Comparison.drev | Group / ID), 
                    data = change1 %>%
                      filter(Dimension == 'Sociability'),
                    cores = 4, chains = 2, iter = 6000)

# percent positive vs negative change
change %>% 
  filter(Dimension == 'Ability') %>% 
  mutate(impression_change = post - pre,
         change_dir = if_else(impression_change > 0,2,
                              if_else(impression_change == 0,1,0))) %>% 
  count(change_dir)

change %>% 
  filter(Dimension == 'Sociability') %>% 
  mutate(impression_change = post - pre,
         change_dir = if_else(impression_change > 0,2,
                              if_else(impression_change == 0,1,0))) %>% 
  count(change_dir)
  

## Figures ####

lines <- tibble(Comparison = c("postpre","postpre","weekpost","weekpost"),
                Dimension = c("Ability","Sociability","Ability","Sociability"),
                mean = c(fixef(mChangeComp1)[1,1],fixef(mChangeSoc1)[1,1],
                         fixef(mChangeComp2)[1,1],fixef(mChangeSoc2)[1,1]),
                low = c(fixef(mChangeComp1)[1,3],fixef(mChangeSoc1)[1,3],
                        fixef(mChangeComp2)[1,3],fixef(mChangeSoc2)[1,3]),
                high = c(fixef(mChangeComp1)[1,4],fixef(mChangeSoc1)[1,4],
                         fixef(mChangeComp2)[1,4],fixef(mChangeSoc2)[1,4]))

change1 <- change1 %>% 
  left_join(lines)

f2a <- ggplot(change2 %>% 
                filter(Dimension == "Ability") %>% 
                mutate(Time = factor(Time,
                                     levels = c("pre","post","week"))),
              aes(x = Time, y = Rating)) +
  geom_violin(fill = "orchid3") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = .6) +
  geom_segment(x = 1, xend = 2, y = 6, yend = 6, 
               linewidth = .75, color = "black",linetype = 'dashed') +
  annotate("text",label = "*",x = 1.5, y = 6.05, size = 10) +
  geom_segment(x = 1, xend = 3, y = 5, yend = 5, 
               linewidth = .75, color = "black",linetype = 'dashed') +
  annotate("text",label = "*",x = 2.3, y = 5.05, size = 10) +
  theme_classic() +
  scale_x_discrete(labels = c("1-7 days prior\nto escape room",
                              "Immediately after\nescape room",
                              '1 week after\nescape room')) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(y = "Competence Rating",
       title = "Change in perceived competence") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        axis.title.x = element_blank())

f2b <- ggplot(change1 %>%
         filter(Dimension == 'Ability'),
       aes(x = Abs, fill = as.factor(Comparison.d),
           color = as.factor(Comparison.d))) +
  geom_histogram() +
  facet_grid(rows = vars(Comparison)) +
  geom_vline(aes(xintercept = mean)) +
  geom_vline(aes(xintercept = low), linetype = "dashed") +
  geom_vline(aes(xintercept = high), linetype = "dashed") +
  theme_bw() +
  labs(x = 'Absolute change in competence ratings\nbetween timepoints',
       y = 'Count',
       title = 'Competence rating change\nafter escape room game',
       fill = 'Comparison') +
  scale_fill_manual(values = c("orchid1","orchid4"),
                    labels = c('Immediately after',"1 week after")) +
  scale_color_manual(values = c("orchid1","orchid4"),
                    labels = c('Immediately after',"1 week after"),
                    guide = "none") +
  theme(legend.position = "bottom",
        strip.text = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

f2c <- ggplot(change2 %>% 
                filter(Dimension == "Sociability") %>% 
                mutate(Time = factor(Time,
                                     levels = c("pre","post","week"))),
              aes(x = Time, y = Rating)) +
  geom_violin(fill = "darkgoldenrod2") +
  stat_summary(fun = mean, fun.min = mean, fun.max = mean,
               geom = "crossbar", width = .6) +
  geom_segment(x = 1, xend = 2, y = 6, yend = 6, 
               linewidth = .75, color = "black",linetype = 'dashed') +
  annotate("text",label = "*",x = 1.5, y = 6.05, size = 10) +
  geom_segment(x = 1, xend = 3, y = 5, yend = 5, 
               linewidth = .75, color = "black",linetype = 'dashed') +
  annotate("text",label = "*",x = 2.3, y = 5.05, size = 10) +
  theme_classic() +
  scale_x_discrete(labels = c("1-7 days prior\nto escape room",
                              "Immediately after\nescape room",
                              '1 week after\nescape room')) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  labs(y = "Sociability Rating",
       title = "Change in perceived sociability") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        axis.title.x = element_blank())

f2d <- ggplot(change1 %>%
                filter(Dimension == 'Sociability'),
              aes(x = Abs, fill = as.factor(Comparison.d),
                  color = as.factor(Comparison.d))) +
  geom_histogram() +
  facet_grid(rows = vars(Comparison)) +
  geom_vline(aes(xintercept = mean)) +
  geom_vline(aes(xintercept = low), linetype = "dashed") +
  geom_vline(aes(xintercept = high), linetype = "dashed") +
  theme_bw() +
  labs(x = 'Absolute change in sociability ratings\nbetween timepoints',
       y = 'Count',
       title = 'Sociability rating change\nafter escape room game',
       fill = 'Comparison') +
  scale_fill_manual(values = c("darkgoldenrod1","darkgoldenrod4"),
                    labels = c('Immediately after',"1 week after")) +
  scale_color_manual(values = c("darkgoldenrod1","darkgoldenrod4"),
                     labels = c('Immediately after',"1 week after"),
                     guide = "none") +
  theme(legend.position = "bottom",
        strip.text = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 18, hjust = .5),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggarrange(f2a,f2b,f2c,f2d,
          labels = c("A","B","C","D"),
          ncol = 2, nrow = 2,
          font.label = list(size = 20))
ggsave("figs/Fig2.jpg", scale = 1.8, width = 6, height = 6)

###### QUESTION 2 ######
## Models ######
mLikeFamiliarSimComp <- brm(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                              (1 + Like_pre.cs*Familiar_pre.cs* 
                                 Similar_pre.cs + pre | Group / ID),
                            data = change %>% 
                              filter(Dimension == 'Ability'),
                            cores = 4, chains = 2, iter = 8000,
                            control = list(adapt_delta = .9))

mLikeFamiliarSimSoc <- brm(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                             (1 + Like_pre.cs*Familiar_pre.cs* 
                                Similar_pre.cs + pre | Group / ID),
                           data = change %>% 
                             filter(Dimension == 'Sociability'),
                           cores = 4, chains = 2, iter = 8000,
                           control = list(adapt_delta = .9))

# not used in paper
mLikeFamiliarSimCompWeek <- brm(week ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                              (1 + Like_pre.cs*Familiar_pre.cs*
                                 Similar_pre.cs + pre | Group / ID),
                            data = change %>% 
                              filter(Dimension == 'Ability'),
                            cores = 4, chains = 2, iter = 8000,
                            control = list(adapt_delta = .9))

# not used in paper
mLikeFamiliarSimSocWeek <- brm(week ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                             (1 + Like_pre.cs*Familiar_pre.cs*
                                Similar_pre.cs + pre | Group / ID),
                           data = change %>% 
                             filter(Dimension == 'Sociability'),
                           cores = 4, chains = 2, iter = 8000,
                           control = list(adapt_delta = .9))

## Figures ######
# competence
x = c("Liking","Familiarity","Similarity",
      "Liking x\nFamiliarity","Liking x\nSimilarity",
      "Familiarity x\nSimilarity",
      "Three-way\ninteraction")
modelPers = brms::fixef(mLikeFamiliarSimComp)

effect = c(modelPers[2,1],modelPers[3,1],modelPers[4,1],
           modelPers[6,1],modelPers[7,1],modelPers[8,1],modelPers[9,1])

lower = c(modelPers[2,3],modelPers[3,3],modelPers[4,3],
          modelPers[6,3],modelPers[7,3],modelPers[8,3],modelPers[9,3])

upper = c(modelPers[2,4],modelPers[3,4],modelPers[4,4],
          modelPers[6,4],modelPers[7,4],modelPers[8,4],modelPers[9,4])

ci80Pers = brms::posterior_samples(mLikeFamiliarSimComp)

# 80% CIs
lower80 = c(quantile(ci80Pers[,2],.1),
            quantile(ci80Pers[,3],.1),
            quantile(ci80Pers[,4],.1),
            quantile(ci80Pers[,6],.1),
            quantile(ci80Pers[,7],.1),
            quantile(ci80Pers[,8],.1),
            quantile(ci80Pers[,9],.1))

# 80% CIs
upper80 = c(quantile(ci80Pers[,2],.9),
            quantile(ci80Pers[,3],.9),
            quantile(ci80Pers[,4],.9),
            quantile(ci80Pers[,6],.9),
            quantile(ci80Pers[,7],.9),
            quantile(ci80Pers[,8],.9),
            quantile(ci80Pers[,9],.9))

pdf <- data.frame(x = x, 
                  effect = effect, lower = lower,
                  upper = upper,
                  lower80 = lower80, upper80 = upper80)
pdf$x <- factor(pdf$x, levels = rev(x))

f3a<- ggplot(pdf, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'orchid3') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, linewidth = 1.5, color = 'orchid3') +  
  geom_point(size = 3, color = 'orchid3') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta",
       title = "Effects of relational factors on\ntrait-level competence perceptions") +
  scale_x_continuous(n.breaks = 8) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = .5, size = 18))

# sociability

x = c("Liking","Familiarity","Similarity",
      "Liking x\nFamiliarity","Liking x\nSimilarity",
      "Familiarity x\nSimilarity",
      "Three-way\ninteraction")
modelPers = brms::fixef(mLikeFamiliarSimSoc)

effect = c(modelPers[2,1],modelPers[3,1],modelPers[4,1],
           modelPers[6,1],modelPers[7,1],modelPers[8,1],modelPers[9,1])

lower = c(modelPers[2,3],modelPers[3,3],modelPers[4,3],
          modelPers[6,3],modelPers[7,3],modelPers[8,3],modelPers[9,3])

upper = c(modelPers[2,4],modelPers[3,4],modelPers[4,4],
          modelPers[6,4],modelPers[7,4],modelPers[8,4],modelPers[9,4])

ci80Pers = brms::posterior_samples(mLikeFamiliarSimSoc)

# 80% CIs
lower80 = c(quantile(ci80Pers[,2],.1),
            quantile(ci80Pers[,3],.1),
            quantile(ci80Pers[,4],.1),
            quantile(ci80Pers[,6],.1),
            quantile(ci80Pers[,7],.1),
            quantile(ci80Pers[,8],.1),
            quantile(ci80Pers[,9],.1))

# 80% CIs
upper80 = c(quantile(ci80Pers[,2],.9),
            quantile(ci80Pers[,3],.9),
            quantile(ci80Pers[,4],.9),
            quantile(ci80Pers[,6],.9),
            quantile(ci80Pers[,7],.9),
            quantile(ci80Pers[,8],.9),
            quantile(ci80Pers[,9],.9))

pdf <- data.frame(x = x, 
                  effect = effect, lower = lower,
                  upper = upper,
                  lower80 = lower80, upper80 = upper80)
pdf$x <- factor(pdf$x, levels = rev(x))

f3b<-ggplot(pdf, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'darkgoldenrod2') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, linewidth = 1.5, color = 'darkgoldenrod2') +  
  geom_point(size = 3, color = 'darkgoldenrod2') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta",
       title = "Effects of relational factors on\ntrait-level sociability perceptions") +
  scale_x_continuous(n.breaks = 8) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = .5, size = 18))

ggarrange(f3a,f3b,
          labels = c("A","B"),
          ncol = 2, nrow = 1,
          font.label = list(size = 20))
ggsave("figs/Fig3.jpg", scale = 1.8, width = 6, height = 3)

###### QUESTION 3 ######
## Models #####
# objective performance #####

mPerfComp <- brm(post ~ ParticipationObjective.cs + pre + 
                   (1  + pre | Group / ID),
                 data = change %>% 
                   filter(Dimension == 'Ability'),
                 cores = 4, chains = 2, iter = 12000,
                 control = list(adapt_delta = .95))

mPerfSoc <- brm(post ~ collab_ratio.cs + pre + 
                  (1 + pre | Group / ID),
                data = change %>% 
                  filter(Dimension == 'Sociability'),
                cores = 4, chains = 2, iter = 12000,
                control = list(adapt_delta = .95))

# PAB #####
change <- change %>% 
  mutate(ParticipationObjective.rs = scales::rescale(ParticipationObjective,to = c(0,10)),
         collab_ratio.rs = scales::rescale(collab_ratio, to = c(0,10)),
         num_utterances.rs = scales::rescale(collab_ratio, to = c(0,10)),
         TeamCollabDiff_post = TeamCollab_post - collab_ratio.rs,
         SolvingPuzzlesDiff_post = SolvingPuzzles_post - ParticipationObjective.rs, 
         TeamCollabSum_post = TeamCollab_post + collab_ratio.rs,
         SolvingPuzzlesSum_post = SolvingPuzzles_post + ParticipationObjective.rs,
         SolvingPuzzles_post.cs = scale(SolvingPuzzles_post, center = T, scale = T),
         TeamCollab_post.cs = scale(TeamCollab_post,center = T, scale = T))

mAccComp <- brm(SolvingPuzzlesDiff_post ~ Familiar_pre.cs*Like_pre.cs*Similar_pre.cs +
                  (1 + Familiar_pre.cs*Like_pre.cs*Similar_pre.cs | Group / ID), 
                data = change %>% 
                  distinct(ID,Teammate,.keep_all = T),
                cores = 4, chains = 2, iter = 8000,
                control = list(adapt_delta = .91))

mAccSoc <- brm(TeamCollabDiff_post ~ Familiar_pre.cs*Like_pre.cs*Similar_pre.cs + 
                 (1 + Familiar_pre.cs*Like_pre.cs*Similar_pre.cs | Group / ID), 
               data = change %>% 
                 distinct(ID,Teammate,.keep_all = T),
               cores = 4, chains = 2, iter = 8000,
               control = list(adapt_delta = .91))

# PAB-traits #####

mAccTraitComp <- brm(post ~ SolvingPuzzlesDiff_post + SolvingPuzzlesSum_post + pre +
                       (1 + SolvingPuzzlesDiff_post + SolvingPuzzlesSum_post + pre | Group / ID),
                     data = change %>% 
                       filter(Dimension == 'Ability'),
                     cores = 4, chains = 2, iter = 10000,
                     control = list(adapt_delta = .95))

mAccTraitSoc <- brm(post ~ TeamCollabDiff_post + TeamCollabSum_post + pre +
                       (1 + TeamCollabDiff_post + TeamCollabSum_post + pre | Group / ID),
                     data = change %>% 
                       filter(Dimension == 'Sociability'),
                     cores = 4, chains = 2, iter = 4000,
                     control = list(adapt_delta = .95))

# PAB-traits based on performance level ####
change_explore <- change %>% 
  mutate(performance_puzzles = if_else(ParticipationObjective > median(ParticipationObjective, na.rm = T),
                                       'high','low'),
         performance_collab = if_else(collab_ratio > median(collab_ratio,na.rm = T),'high','low'),
         impression_change = post - pre)

mAccTraitComp2 <- brm(post ~ SolvingPuzzlesDiff_post*performance.d + pre +
                        (1 + SolvingPuzzlesDiff_post*performance.d + pre | Group/ID),
                      data = change_explore %>% 
                        filter(Dimension == 'Ability') %>% 
                        mutate(performance.d = if_else(performance_puzzles == 'low',0,1)),
                      cores = 4, chains = 2, iter = 4000,
                      control = list(adapt_delta = .95))

mAccTraitSoc2 <- brm(post ~ TeamCollabDiff_post*performance.d + pre +
                        (1 + TeamCollabDiff_post*performance.d + pre | Group/ID),
                      data = change_explore %>% 
                        filter(Dimension == 'Sociability') %>% 
                        mutate(performance.d = if_else(performance_collab == 'low',0,1)),
                      cores = 4, chains = 2, iter = 4000,
                      control = list(adapt_delta = .95))

# PAB-traits with CRA #####
# first just include components separately
mAccTraitCompCRA1 <- brm(post ~ SolvingPuzzles_post + ParticipationObjective.rs + pre +
                           (1 + SolvingPuzzles_post + ParticipationObjective.rs + pre | Group / ID),
                         data = change %>% 
                           filter(Dimension == 'Ability'),
                         cores = 4, chains = 2, iter = 8000,
                         control = list(adapt_delta = .95))

mAccTraitSocCRA1 <- brm(post ~ TeamCollab_post + collab_ratio.rs + pre +
                          (1 + TeamCollab_post + collab_ratio.rs + pre | Group / ID),
                        data = change %>% 
                          filter(Dimension == 'Sociability'),
                        cores = 4, chains = 2, iter = 8000,
                        control = list(adapt_delta = .95))

# implement with lavaan
library(lavaan)

# competence
model <- 'post ~ 1 + c1*SolvingPuzzles_post + c2*ParticipationObjective.rs
a1 := c1+c2
a3 := c1-c2
c1_times_2 := 2*c1
c2_times_2 := 2*c2
minus_c1_times_2 := -2*c1
minus_c2_times_2 := -2*c2'

regression <- sem(model, data=change %>% filter(Dimension == 'Ability'))

estimates <- parameterEstimates(regression)

if (subset(estimates, label == "a3")["est"]>=0 & subset(estimates, label == "a1")["est"]>0){
  abs <- round(subset(estimates, label == "minus_c2_times_2")["est"],2)
  se <- round(subset(estimates, label == "minus_c2_times_2")["se"],2)
  a3_is <- "positive"
  
  # compute one-tailed p-value of abs for the hypothesis abs > 0, 
  # depending on the tail in which abs is positioned
  if (abs >= 0){pvalue <- round(subset(estimates, label == "minus_c2_times_2")["pvalue"]/2, 5)}
  if (abs < 0){pvalue <- round(1 - (subset(estimates, label == "minus_c2_times_2")["pvalue"]/2), 5)}
}


if (subset(estimates, label == "a3")["est"]<0 & subset(estimates, label == "a1")["est"]>0){
  abs <- round(subset(estimates, label == "minus_c1_times_2")["est"],2)
  se <- round(subset(estimates, label == "minus_c1_times_2")["se"],2)
  a3_is <- "negative"
  
  # compute one-tailed p-value of abs for the hypothesis abs > 0, 
  # depending on the tail in which abs is positioned
  if (abs >= 0){pvalue <- round(subset(estimates, label == "minus_c1_times_2")["pvalue"]/2, 5)}
  if (abs < 0){pvalue <- round(1 - (subset(estimates, label == "minus_c1_times_2")["pvalue"]/2), 5)}
}


if (subset(estimates, label == "a3")["est"]>=0 & subset(estimates, label == "a1")["est"]<0){
  abs <- round(subset(estimates, label == "c1_times_2")["est"],2)
  se <- round(subset(estimates, label == "c1_times_2")["se"],2)
  a3_is <- "positive"
  
  # compute one-tailed p-value of abs for the hypothesis abs > 0, 
  # depending on the tail in which abs is positioned
  if (abs >= 0){pvalue <- round(subset(estimates, label == "c1_times_2")["pvalue"]/2, 5)}
  if (abs < 0){pvalue <- round(1 - (subset(estimates, label == "c1_times_2")["pvalue"]/2), 5)}
}


if (subset(estimates, label == "a3")["est"]<0 & subset(estimates, label == "a1")["est"]<0){
  abs <- round(subset(estimates, label == "c2_times_2")["est"],2)
  se <- round(subset(estimates, label == "c2_times_2")["se"],2)
  a3_is <- "negative"
  
  # compute one-tailed p-value of abs for the hypothesis abs > 0, 
  # depending on the tail in which abs is positioned
  if (abs >= 0){pvalue <- round(subset(estimates, label == "c2_times_2")["pvalue"]/2, 5)}
  if (abs < 0){pvalue <- round(1 - (subset(estimates, label == "c2_times_2")["pvalue"]/2), 5)}
}

print(c("abs"=abs,se,pvalue,"a3 is"=a3_is))

# sociability
model <- 'post ~ 1 + c1*TeamCollab_post + c2*collab_ratio.rs
a1 := c1+c2
a3 := c1-c2
c1_times_2 := 2*c1
c2_times_2 := 2*c2
minus_c1_times_2 := -2*c1
minus_c2_times_2 := -2*c2'

regression <- sem(model, data=change %>% filter(Dimension == 'Sociability'))

estimates <- parameterEstimates(regression)

if (subset(estimates, label == "a3")["est"]>=0 & subset(estimates, label == "a1")["est"]>0){
  abs <- round(subset(estimates, label == "minus_c2_times_2")["est"],2)
  se <- round(subset(estimates, label == "minus_c2_times_2")["se"],2)
  a3_is <- "positive"
  
  # compute one-tailed p-value of abs for the hypothesis abs > 0, 
  # depending on the tail in which abs is positioned
  if (abs >= 0){pvalue <- round(subset(estimates, label == "minus_c2_times_2")["pvalue"]/2, 5)}
  if (abs < 0){pvalue <- round(1 - (subset(estimates, label == "minus_c2_times_2")["pvalue"]/2), 5)}
}


if (subset(estimates, label == "a3")["est"]<0 & subset(estimates, label == "a1")["est"]>0){
  abs <- round(subset(estimates, label == "minus_c1_times_2")["est"],2)
  se <- round(subset(estimates, label == "minus_c1_times_2")["se"],2)
  a3_is <- "negative"
  
  # compute one-tailed p-value of abs for the hypothesis abs > 0, 
  # depending on the tail in which abs is positioned
  if (abs >= 0){pvalue <- round(subset(estimates, label == "minus_c1_times_2")["pvalue"]/2, 5)}
  if (abs < 0){pvalue <- round(1 - (subset(estimates, label == "minus_c1_times_2")["pvalue"]/2), 5)}
}


if (subset(estimates, label == "a3")["est"]>=0 & subset(estimates, label == "a1")["est"]<0){
  abs <- round(subset(estimates, label == "c1_times_2")["est"],2)
  se <- round(subset(estimates, label == "c1_times_2")["se"],2)
  a3_is <- "positive"
  
  # compute one-tailed p-value of abs for the hypothesis abs > 0, 
  # depending on the tail in which abs is positioned
  if (abs >= 0){pvalue <- round(subset(estimates, label == "c1_times_2")["pvalue"]/2, 5)}
  if (abs < 0){pvalue <- round(1 - (subset(estimates, label == "c1_times_2")["pvalue"]/2), 5)}
}


if (subset(estimates, label == "a3")["est"]<0 & subset(estimates, label == "a1")["est"]<0){
  abs <- round(subset(estimates, label == "c2_times_2")["est"],2)
  se <- round(subset(estimates, label == "c2_times_2")["se"],2)
  a3_is <- "negative"
  
  # compute one-tailed p-value of abs for the hypothesis abs > 0, 
  # depending on the tail in which abs is positioned
  if (abs >= 0){pvalue <- round(subset(estimates, label == "c2_times_2")["pvalue"]/2, 5)}
  if (abs < 0){pvalue <- round(1 - (subset(estimates, label == "c2_times_2")["pvalue"]/2), 5)}
}

print(c("abs"=abs,se,pvalue,"a3 is"=a3_is))

# unused in paper ######
# don't use in paper but helpful for visualizing

ggplot(change_explore %>% filter(Dimension == 'Ability'),
       aes(x = SolvingPuzzlesDiff_post, y = post, color = performance_puzzles)) +
  geom_jitter() +
  geom_smooth(method = 'lm')

ggplot(change_pabtest %>% filter(Dimension == 'Sociability',
                                 !is.na(performance_collab)),
       aes(x = TeamCollabDiff_post, y = post, color = performance_collab)) +
  geom_jitter() +
  geom_smooth(method = 'lm')
  
# don't use these in the paper but helpful if I want to begin to conceptualize as mediation
change_med <- change %>% 
  select(ID, Teammate,Familiar_pre.cs,Like_pre.cs,Similar_pre.cs,
         SolvingPuzzlesDiff_post,TeamCollabDiff_post,Dimension,pre,post)

# similarity, competence 
change_med_comp <- change_med %>% 
  filter(Dimension == 'Ability') %>% 
  isolate(by = "ID",
          value = c("Similar_pre.cs","SolvingPuzzlesDiff_post","post"))

fit_med_comp <- mlm(d = change_med_comp %>% 
                      filter(!is.na(Similar_pre.cs_cw),
                             !is.na(SolvingPuzzlesDiff_post_cw),
                             !is.na(post_cw)),
                    id = "ID", x = "Similar_pre.cs_cw",
                    m = "SolvingPuzzlesDiff_post_cw",y = "post_cw",
                    iter = 8000, cores = 4)

# liking, sociability 
change_med_soc <- change_med %>% 
  filter(Dimension == 'Sociability') %>% 
  isolate(by = "ID",
          value = c("Like_pre.cs","TeamCollabDiff_post","post"))

fit_med_soc <- mlm(d = change_med_soc %>% 
                     filter(!is.na(Like_pre.cs_cw),
                            !is.na(TeamCollabDiff_post_cw),
                            !is.na(post_cw)),
                   id = "ID", x = "Like_pre.cs_cw",
                   m = "TeamCollabDiff_post_cw",y = "post_cw",
                   iter = 8000, cores = 4)

## Figures #####
# Objective performance #####

f4a <- ggplot(change %>% 
                filter(Dimension == "Ability",
                       !is.na(ParticipationObjective.cs)),
              aes(x = ParticipationObjective, y = post)) +
  geom_jitter(alpha = .2, color = "orchid3") +
  geom_smooth(method = "lm", color = "orchid3") +
  theme_classic() +
  labs(x = "Puzzle solving score (out of 50)", y = "Post-game competence rating",
       title = "Puzzle solving performance") +
  theme(plot.title = element_text(size = 18, hjust = .5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

f4b <- ggplot(change %>% 
         filter(Dimension == "Sociability",
                !is.na(collab_ratio.cs)),
         aes(x = collab_ratio, y = post)) +
  geom_jitter(alpha = .2, color = "darkgoldenrod2") +
  geom_smooth(method = "lm", color = "darkgoldenrod2") +
  theme_classic() +
  labs(x = "Team collaboration score (%)", 
       y = "Post-game sociability rating",
       title = "Team collaboration performance") +
  theme(plot.title = element_text(size = 18, hjust = .5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggarrange(f4a,f4b,
          labels = c("A","B"),
          ncol = 2, nrow = 1,
          font.label = list(size = 20))
ggsave("figs/Fig4.jpg", scale = 1.8, width = 6, height = 3)

# Effects of RFs on PAB #######

x = c("Liking","Familiarity","Similarity")
modelPers = brms::fixef(mAccComp)

effect = c(modelPers[3,1],modelPers[2,1],modelPers[4,1])

lower = c(modelPers[3,3],modelPers[2,3],modelPers[4,3])

upper = c(modelPers[3,4],modelPers[2,4],modelPers[4,4])

ci80Pers = brms::posterior_samples(mAccComp)

# 80% CIs
lower80 = c(quantile(ci80Pers[,3],.1),
            quantile(ci80Pers[,2],.1),
            quantile(ci80Pers[,4],.1))

# 80% CIs
upper80 = c(quantile(ci80Pers[,3],.9),
            quantile(ci80Pers[,2],.9),
            quantile(ci80Pers[,4],.9))

pdf <- data.frame(x = x, 
                  effect = effect, lower = lower,
                  upper = upper,
                  lower80 = lower80, upper80 = upper80)
pdf$x <- factor(pdf$x, levels = rev(x))

f5a<- ggplot(pdf, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'orchid3') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, size = 1.5, color = 'orchid3') +  
  geom_point(size = 3, color = 'orchid3') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta",
       title = "Effects of relational factors on\nPAB - puzzle solving ability") +
  scale_x_continuous(n.breaks = 8) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = .5, size = 18))

# sociability

x = c("Liking","Familiarity","Similarity")
modelPers = brms::fixef(mAccSoc)

effect = c(modelPers[3,1],modelPers[2,1],modelPers[4,1])

lower = c(modelPers[3,3],modelPers[2,3],modelPers[4,3])

upper = c(modelPers[3,4],modelPers[2,4],modelPers[4,4])

ci80Pers = brms::posterior_samples(mAccSoc)

# 80% CIs
lower80 = c(quantile(ci80Pers[,3],.1),
            quantile(ci80Pers[,2],.1),
            quantile(ci80Pers[,4],.1))

# 80% CIs
upper80 = c(quantile(ci80Pers[,3],.9),
            quantile(ci80Pers[,2],.9),
            quantile(ci80Pers[,4],.9))

pdf <- data.frame(x = x, 
                  effect = effect, lower = lower,
                  upper = upper,
                  lower80 = lower80, upper80 = upper80)
pdf$x <- factor(pdf$x, levels = rev(x))

f5b<-ggplot(pdf, aes(x = effect, y = x)) +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = 0, color = 'darkgoldenrod2') +
  geom_errorbar(aes(xmin = lower80, xmax = upper80),
                width = 0, size = 1.5, color = 'darkgoldenrod2') +  
  geom_point(size = 3, color = 'darkgoldenrod2') +
  theme_classic() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Standardized Beta",
       title = "Effects of relational factors on\nPAB - team collaboration ability") +
  scale_x_continuous(n.breaks = 8) +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16),
        plot.title = element_text(hjust = .5, size = 18))

ggarrange(f5a,f5b,
          labels = c("A","B"),
          ncol = 2, nrow = 1,
          font.label = list(size = 20))
ggsave("figs/Fig5.jpg", scale = 1.8, width = 6, height = 3)

###### Post hoc power #####
library(pwr)
library(MuMIn)
library(simr)

# pwr way
mChangeComp1Power <- lmer(Rating ~ Time.d + 
                            (1 + Time.d | Group / ID),
                          data = change2 %>% 
                            filter(Dimension == 'Ability',
                                   Time != 'week') %>% 
                            mutate(Time.d = if_else(Time == 'pre',0,1)))
R2 <- r.squaredGLMM(mChangeComp1Power)
R <- sqrt(R2[1])
pwr.f2.test(u = 3, f2 = R, sig.level = .05, power = .8)

mLikeFamiliarSimCompPower <- lmer(post ~ Like_pre.cs*Familiar_pre.cs*Similar_pre.cs + pre +
                                    (1 | Group / ID),
                                  data = change %>% 
                                    filter(Dimension == 'Ability'))

R2 <- r.squaredGLMM(mLikeFamiliarSimCompPower)
R <- sqrt(R2[1])
pwr.f2.test(u = 3, f2 = R, sig.level = .05, power = .8)

mAccCompPower <- lmer(SolvingPuzzlesDiff_post ~ Familiar_pre.cs*Like_pre.cs*Similar_pre.cs + 
                  (1  | Group / ID), 
                data = change %>% 
                  distinct(ID,Teammate,.keep_all = T))

R2 <- r.squaredGLMM(mAccCompPower)
R <- sqrt(R2[1])
pwr.f2.test(u = 3, f2 = R, sig.level = .05, power = .8)

# simr way, not using
mDirChangeComp1Power <- lmer(Rating ~ Time.d + 
                          (1 + Time.d | Group / ID),
                        data = change2 %>% 
                          filter(Dimension == 'Ability',
                                 Time != 'week') %>% 
                          mutate(Time.d = if_else(Time == 'pre',0,1)))
power1 <- powerSim(mDirChangeComp1Power,
                   test = fixed("Time.d"),
                   nsim = 1000)


