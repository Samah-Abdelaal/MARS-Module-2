# inter-item reliability

library(psych)

psych::alpha(resp_satis, check.keys=TRUE)

psych::alpha(open_share, check.keys = TRUE)

# factor analysis

factanal(ass_factors, factors = 3)

# calculating %SM

MeanSD <- function(x){
  paste(round(mean(x, na.rm = TRUE), 2),
        "±",
        round(sd(x, na.rm = TRUE), 2))
}

# %SM for resp_satis

#sum(resp_satis, na.rm = TRUE) # useless
total_resp_satis <- apply(resp_satis, 1, sum)
MeanSD(total_resp_satis)

(perc_resp_satis <- ((total_resp_satis - 9) / (45 - 9)) * 100)
MeanSD(perc_resp_satis)

# %SM for open_share

total_open_share <- apply(open_share, 1, sum)
MeanSD(total_open_share)

(perc_open_share <- ((total_open_share - 9) / (45 - 9)) * 100)
MeanSD(perc_open_share)

hist(total_resp_satis, nclass = 20, col = "darkseagreen4")
hist(perc_resp_satis, nclass = 20, col = "darkseagreen4")

hist(total_open_share, nclass = 20, col = "darkseagreen4")
hist(perc_open_share, nclass = 20, col = "darkseagreen4")

factanal(pcall, factors = 3)
