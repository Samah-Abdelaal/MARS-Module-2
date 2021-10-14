# t test & ANOVA

# dependent variables: respect & satisfaction, openness & sharing
# relevant independent variables are:
# sex, professional category, hospital category, educational category
# required table items: variable category, N, mean+-SD, t-test, p-value,
# 95% CI mean difference

# resp & satis : column 13:21
# open & share : column 22:30
# ass factors : column 31:44

#library(finalfit)

pca_DM %>% 
  mutate(resp.satis = apply(resp_satis, 1, sum),
         open.share = apply(open_share, 1, sum)) -> scored_df
View(scored_df)

#sapply(scored_df$resp.satis, scored_df$Sex, mean)         
# WTF is this shit

MeanSD <- function(x){
  paste(round(mean(x, na.rm = TRUE), 2),
        "±",
        round(sd(x, na.rm = TRUE), 2))
}

boxplot(scored_df$resp.satis)
hist(scored_df$resp.satis)
shapiro.test(scored_df$resp.satis)
scored_df %>% group_by(Sex) %>% 
  summarise(variance = var(resp.satis))
mwt <- wilcox.test(scored_df$resp.satis ~ scored_df$Sex,
            alternative = "two.sided")

ttest <- t.test(scored_df$resp.satis ~ scored_df$Sex,
            alternative = "two.sided")

#ttest <- data.frame(
  #t <-  c(ttest$statistic,
   #                 ttest$p.value,
    #                ttest$conf.int)

# table 5

scored_df %>% 
  group_by(Sex) %>% 
  summarize(N = n(),
            'Mean±SD' = MeanSD(resp.satis)) -> tab5

library(flextable)

as_flextable(cbind(tab5,ttest))
flextable::flextable(scored_df) %>% 
  compose(
          value = as_paragraph(
            as_chunk(tab5)
          ))
