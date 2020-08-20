
rm(list = ls())

library(tidyverse)

source("R/auxiliary-functions.r")
source("R/read-data.r")

for(n in c(names(data), "Pooled.sample")){
  assign(
    paste(n, "bl1", sep = "_"), 
    as.data.frame(
      t(
        rbind(
          digit = 1:9,
          observed = table(enframe(msdigit(eval(as.name(n))), name = NULL, value = "Digit"))/sum(table(enframe(msdigit(eval(as.name(n))), name = NULL, value = "Digit"))),
          expected = theta_benford(1),
          sample = rep(n, times = 9)
        )
      )
    )
  )
}

for(n in c(names(data), "Pooled.sample")){
  assign(
    paste(n, "bl2", sep = "_"), 
    as.data.frame(
      t(
        rbind(
          digit = 0:9,
          observed = table(enframe(smsdigit(eval(as.name(n))), name = NULL, value = "Digit"))/sum(table(enframe(smsdigit(eval(as.name(n))), name = NULL, value = "Digit"))),
          expected = theta_benford(2),
          sample = rep(n, times = 10)
        )
      )
    )
  )
}

bl1_sample_names <- grep("_bl1", ls(), value = TRUE)
bl2_sample_names <- grep("_bl2", ls(), value = TRUE)

bl1_list <- list()
bl2_list <- list()

for (i in 1:13) {
  bl1_list[[i]] <- eval(as.name(bl1_sample_names[i]))
  bl2_list[[i]] <- eval(as.name(bl2_sample_names[i]))
}

for(i in seq_along(bl1_list)){
  bl1_list[[i]]$observed <- as.numeric(as.character(bl1_list[[i]]$observed))
  bl1_list[[i]]$expected <- as.numeric(as.character(bl1_list[[i]]$expected))
  bl1_list[[i]]$digit <- as.numeric(as.character(bl1_list[[i]]$digit))
  
}

for(i in seq_along(bl2_list)){
  bl2_list[[i]]$observed <- as.numeric(as.character(bl2_list[[i]]$observed))
  bl2_list[[i]]$expected <- as.numeric(as.character(bl2_list[[i]]$expected))
  bl2_list[[i]]$digit <- as.numeric(as.character(bl2_list[[i]]$digit))
}

data_bl1 <- do.call("rbind", bl1_list)
data_bl2 <- do.call("rbind", bl2_list)

data_bl1$sample <-  gsub(pattern = "Pooled.sample", replacement = "Pooled Sample", x = data_bl1$sample)
data_bl2$sample <-  gsub(pattern = "Pooled.sample", replacement = "Pooled Sample", x = data_bl2$sample)

data_bl1$sample <- fct_relevel(as.factor(data_bl1$sample), "Pooled Sample", after = Inf)
data_bl2$sample <- fct_relevel(as.factor(data_bl2$sample), "Pooled Sample", after = Inf)

ggplot(data = data_bl1,
       aes(x = digit, group = 1)) +
  geom_point(aes(y = expected), size = 2) +
  geom_line(aes(y = expected), linetype = "solid") +
  geom_point(aes(y = observed), shape = 0, size = 3) +
  geom_line(aes(y = observed), linetype = "longdash") +
  facet_wrap(. ~ sample) + 
  ylim(0, 0.5) +
  ylab("Proportion") +
  theme_bw() + 
  scale_x_discrete(name ="Digit") 
ggsave(plot = last_plot(), filename = "plots/bl1.png")

ggplot(data = data_bl2,
       aes(x = digit, group = 1)) +
  geom_point(aes(y = expected), size = 2) +
  geom_line(aes(y = expected), linetype = "solid") +
  geom_point(aes(y = observed), shape = 0, size = 3) +
  geom_line(aes(y = observed), linetype = "longdash") +
  facet_wrap(. ~ sample) + 
  ylim(0, 0.25) +
  ylab("Proportion") +
  theme_bw() + 
  scale_x_discrete(name ="Digit") 
ggsave(plot = last_plot(), filename = "plots/bl2.png")

########## Pooled Samples ###########

# Uncomment to see isolated polts of the pooled samples  

# Pooled.sample_bl1 <- tibble(
#   digit = 1:9,
#   observed = as.numeric(table(msdigit(Pooled.sample))/sum(table(msdigit(Pooled.sample)))),
#   expected = theta_benford(1),
#   sample = "BL1"
# )
# 
# Pooled.sample_bl2 <- tibble(
#   digit = 0:9,
#   observed = table(smsdigit(Pooled.sample))/sum(table(smsdigit(Pooled.sample))),
#   expected = theta_benford(2),
#   sample = "BL2"
# )
# 
# pooled_data <- rbind(Pooled.sample_bl1, Pooled.sample_bl2)

# ggplot(data = pooled_data,
#        aes(x = digit, group = 1)) +
#   geom_point(aes(y = expected), size = 2) +
#   geom_line(aes(y = expected), linetype = "solid") +
#   geom_point(aes(y = observed), shape = 0, size = 3) +
#   geom_line(aes(y = observed), linetype = "longdash") +
#   ylim(0, .35) +
#   ylab("Proportion") +
#   scale_x_discrete(name ="Digit", limits=c(0:9)) +
#   facet_wrap(. ~ sample) + 
#   theme_bw() 
# ggsave(plot = last_plot(), filename = "plots/pooled.png")




