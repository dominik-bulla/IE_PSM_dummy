### Description ----------------------- ----------------------- ----------------------- -----------------------
# This dummy project demonstrates propensity-score matching (PSM) to analyse observational data.
# The data is real-life data that was collected as part of an assignment given to Dominik Bulla by humanitarian actors.  
#
# This file analyses the dataset. 
#
# Author: Dominik Bulla
# Date: 21/11/2024
# Contact: dominik.bulla@gmail.com



### Environment ----------------------- ----------------------- ----------------------- -----------------------

setwd("C:/Users/domin/GitHub/IE_PSM_dummy")
MAR_ORIGINAL <- par("mar")
par(mar=c(5,4,1,1))
rm(list=ls())
options(scipen=999)



### Load packages ----------------------- ----------------------- ----------------------- -----------------------

library(openxlsx)
library(dplyr)
library(psych)
library(fastDummies)
library(ggplot2)
library(showtext)
library(scales)
library(MatchIt)
library(patchwork)
library(stargazer)



### Import data ----------------------- ----------------------- ----------------------- -----------------------

data <- read.xlsx("02 processed data/data_clean_20240909_V01.xlsx")
data <- data %>%
  mutate(cva = factor(cva, levels = unique(cva)),
         age2 = as.numeric(age2), 
         income2 = as.numeric(income2))



### Descriptive statistics ----------------------- ----------------------- ----------------------- -----------------------

data_cols <- c("IP_A", "IP_B", "IP_C", "IP_D", "IP_E", 
               "host", "idp", "refugees",
               "age", "female", "disability", "married", "children", 
               "edu_no_education", "edu_no_completion", "edu_primary", "edu_secondary", 
               "assafe",  "lesssafe", "safer", 
               "parenting_better",  "relationship_better", 
               "asworried", "lessworried", "moreworried",
               "income")
data_summary <- data %>%
  group_by(cva) %>%
  summarise(across(data_cols[1:5], ~round(sum(., na.rm = TRUE), 2))) %>%
  mutate(cva = as.character(cva)) 
data_summary$Total <- rowSums(data_summary[, 2:6])
data_summary <- rbind(data_summary, 
                      data.frame(cva = "Total",
                                 IP_A = sum(data_summary$IP_A),
                                 IP_B = sum(data_summary$IP_B),
                                 IP_C = sum(data_summary$IP_C),
                                 IP_D = sum(data_summary$IP_D),
                                 IP_E = sum(data_summary$IP_E),
                                 Total = sum(data_summary$Total)))
write.csv(data_summary, "03 Results/03.2 Tables/Table 1_sample size_20240912.csv", row.names = FALSE)



### Summary stats ----------------------- ----------------------- ----------------------- -----------------------

data_summary <- data %>%
  group_by(cva) %>%
  summarise(across(data_cols[6 : 26], ~round(mean(.) * 100, 2))) %>%
  mutate(cva = as.character(cva),
         type = "m") %>%
  mutate(age = round(age / 100, 2),
         income = round(income / 100, 2))
data_summary_sd <- data %>%
  group_by(cva) %>%
  summarise(across(data_cols[6 : 26], ~round(sd(.) * 100, 2))) %>%
  mutate(cva = as.character(cva),
         type = "sd") %>%
  mutate(age = round(age / 100, 2),
         income = round(income / 100, 2))
data_summary <- rbind(data_summary, data_summary_sd)
rm(data_summary_sd)
data_summary <- data_summary %>%
  arrange(cva, type) %>%
  select(-c(type)) %>%
  mutate(IP = "Total") %>%
  select(IP, everything())
data_summaryIP <- data %>%
  group_by(IP, cva) %>%
  summarise(across(data_cols[6 : 26], ~round(mean(.) * 100, 2))) %>%
  mutate(cva = as.character(cva),
         type = "m")  %>%
  mutate(age = round(age / 100, 2),
         income = round(income / 100, 2))
data_summaryIP_sd <- data %>%
  group_by(IP, cva) %>%
  summarise(across(data_cols[6 : 26], ~round(sd(.) * 100, 2))) %>%
  mutate(cva = as.character(cva),
         type = "sd") %>%
  mutate(age = round(age / 100, 2),
         income = round(income / 100, 2))
data_summaryIP <- rbind(data_summaryIP, data_summaryIP_sd)
rm(data_summaryIP_sd)
data_summaryIP <- data_summaryIP %>%
  arrange(IP, cva, type) 
data_summaryIP <- rbind(data_summaryIP, data_summary)
write.csv(data_summaryIP[, which(colnames(data_summaryIP) != "type")], "03 Results/03.2 Tables/Table 2_sample means of evaluation sample_20240912.csv", row.names = FALSE)

lapply(data[, data_cols[6 : 26]], function(x) t.test(x ~ cva, data = data)$p.value)
rm(data_cols)





### Summary graphs ----------------------- ----------------------- ----------------------- -----------------------

ggplot(data, aes(x = income)) +
  geom_histogram(aes(y = ..density..), bins = 100, fill = "#0072BC", alpha = 0.7) +
  geom_density(aes(y=..density..)) +
  facet_wrap(~cva, ncol = 2, 
             strip.position = "top",
             labeller = as_labeller(c("received" = "CVA's received", "not received" = "CVA's received"))) +
  labs(
    #title = "Income distributed by treatment status",
    subtitle = paste0("All data (n = ", nrow(data), ")"),
    x = "Annual income (in XYZ)",
    y = "Density",
    caption = "Source: endine 2023 \n© CONSORTIUM") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        axis.title = element_text(face = "plain"),
        axis.text.x = element_text(size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "white")) 

ggsave(paste0("01 income by cva_histogram.png"),
       plot = last_plot(),
       device = png(),
       path = "03 Results/03.1 Graphs/",
       scale = 1,
       dpi = 150,
       bg = 'white')
dev.off()

ggplot(data_summaryIP[data_summaryIP$type == "m" & data_summaryIP$IP != "Total", ], aes(x = IP, y = income, fill = cva)) +
          geom_bar(position = "dodge", 
                   stat= "identity") +
          labs(
            subtitle = paste0("All data (n = ", nrow(data), ")"),
            y = "The average income (in XYZ)",
            caption = "Source: endine 2023 \n© CONSORTIUM") + 
          geom_text(aes(y = income, 
                        label = income), 
                    position = position_dodge(width = 0.9),
                    vjust= -1,
                    size = 3, 
                    color = "black") + 
          scale_y_continuous(
            expand = expansion(c(0, 0.1)),
          ) + 
          scale_x_discrete() +
          scale_fill_manual("CVA status", values = c("received" = "#0072BC", "not received" = "#8EBEFF"), 
                            labels = c("received" = "CVA's received", "not received" = "CVA's not received")) +
          theme(panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(color = "grey"),
                axis.title = element_text(face = "plain"),
                axis.title.x = element_blank(),
                axis.text.x = element_text(size = 12, face = "plain"),
                axis.text.y = element_text(size = 12, face = "plain"),
                panel.grid.major.x = element_blank(),
                legend.title = element_blank(),
                legend.position = "bottom")

ggsave(paste0("02 income by IP and cva.png"),
       plot = last_plot(),
       device = png(),
       path = "03 Results/03.1 Graphs/",
       scale = 1,
       dpi = 150,
       bg = 'white')
dev.off()



### Estimation of propensity scores ----------------------- ----------------------- ----------------------- -----------------------

m_pscore <- glm(cva ~ IP_B + IP_C + IP_D + IP_E + 
                idp + refugees + 
                age + female + disability + married + children + 
                  edu_no_education + edu_no_completion + edu_primary + edu_secondary + edu_university + edu_other,
                family = binomial(), data = data)
summary(m_pscore)
data_pscore <- data.frame(pr_score = predict(m_pscore, type = "response"),
                          cva = m_pscore$model$cva,
                          IP_B = m_pscore$model$IP_B                          ,
                          IP_C = m_pscore$model$IP_C,
                          IP_D = m_pscore$model$IP_D,
                          IP_E = m_pscore$model$IP_E)



### Matching up units ----------------------- ----------------------- ----------------------- -----------------------

match <- matchit(cva ~ IP_B + IP_C + IP_D + IP_E + 
                       idp + refugees + 
                       age + female + disability + married + children + 
                       edu_no_education + edu_no_completion + edu_primary + edu_secondary + edu_university + edu_other,
                       method = "nearest", data = data)
matching_results <- summary(match)
matching_results_all <- data.frame(round(matching_results$sum.all, 2))
rownames(matching_results_all) <- c("P score", 
                                    "beneficiary of partner B", "beneficiary of partner C", "beneficiary of partner D", "beneficiary of partner E",
                                    "being IDP", "being a refugee",
                                    "age", "being female", "having disabilities",
                                    "being married", "# of children", 
                                    "no education", "no degree", "primary completed", "secondary completed", "university completed", "other degrees")
matching_results_all <- matching_results_all %>%
  rename("Means (CVA received)" = Means.Treated,
         "Means (CVA not received)" =  Means.Control,)
write.csv(matching_results_all, "03 Results/03.2 Tables/Table 3_Summary of balance for all data_20240912.csv", row.names = TRUE)

matching_results_matched <- data.frame(round(matching_results$sum.matched, 2))
rownames(matching_results_matched) <- c("P score", 
                                    "beneficiary of partner B", "beneficiary of partner C", "beneficiary of partner D", "beneficiary of partner E",
                                    "being IDP", "being a refugee",
                                    "age", "being female", "having disabilities",
                                    "being married", "# of children", 
                                    "no education", "no degree", "primary completed", "secondary completed", "university completed", "other degrees")
matching_results_matched <- matching_results_matched %>%
  rename("Means (CVA received)" = Means.Treated,
         "Means (CVA not received)" =  Means.Control,)
write.csv(matching_results_matched, "03 Results/03.2 Tables/Table 4_Summary of balance for matched data_20240912.csv", row.names = TRUE)

   # extract matched-up data

data_PSM <- match.data(match)
data_PSM_all <- match.data(match, drop.unmatched = FALSE)



### Common support in terms of propensity scores ----------------------- ----------------------- ----------------------- -----------------------
data_PSM_all$c <- 0
data_PSM$c <- 1
data_PSM_c <- rbind(data_PSM_all, data_PSM)
ggplot(data_PSM_c, aes(x = distance, fill = cva)) +
  geom_histogram(aes(y = ..density..), bins = 100, alpha = 0.7)  +
  scale_fill_manual("CVA status", values = c("received" = "#0072BC", "not received" = "#8EBEFF"), 
                    labels = c("received" = "CVA's received", "not received" = "CVA's not received")) +
  facet_wrap(~c, ncol = 2, 
             strip.position = "top",
             labeller = as_labeller(c("0" = paste0("All data (n = ", nrow(data_PSM_all), ")"), "1" = paste0("Matched data (n = ", nrow(data_PSM), ")")))) +
  labs(
    x = "Probability of receiving cash vouchers",
    y = "Density",
    caption = "Source: endine 2023 \n© CONSORTIUM") +
  theme(plot.title = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey"),
        axis.title = element_text(face = "plain"),
        axis.text.x = element_text(size = 12, face = "plain"),
        axis.text.y = element_text(size = 12, face = "plain"),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        strip.background = element_rect(fill = "white"))

ggsave(paste0("03 propensity scores by cva_histogram.png"),
       plot = last_plot(),
       device = png(),
       path = "03 Results/03.1 Graphs/",
       scale = 1,
       dpi = 150,
       bg = 'white')
dev.off()




### Common support in terms of propensity scores [by implementing partners] ----------------------- ----------------------- ----------------------- -----------------------

variables <- c("IP_A", "IP_B", "IP_C", "IP_D", "IP_E")
c <- 1
for (var in variables) {
  ggplot(data_PSM_c[data_PSM_c[, var] == 1, ], aes(x = distance, fill = cva)) +
    geom_histogram(aes(y = ..density..), bins = 100, alpha = 0.7)  +
    scale_fill_manual("CVA status", values = c("received" = "#0072BC", "not received" = "#8EBEFF"), 
                      labels = c("received" = "CVA's received", "not received" = "CVA's not received")) +
    facet_wrap(~c, ncol = 2, 
               strip.position = "top",
               labeller = as_labeller(c("0" = paste0("All data (n = ", nrow(data_PSM_c[data_PSM_c[, var] == 1, ]), ")"), "1" = paste0("Matched data (n = ", nrow(data_PSM_c[data_PSM_c[, var] == 1 & data_PSM_c$weights == 1, ]), ")")))) +
    labs(
      x = "Probability of receiving cash vouchers",
      y = "Density",
      caption = "Source: endine 2023 \n© CONSORTIUM") +
    theme(plot.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey"),
          axis.title = element_text(face = "plain"),
          axis.text.x = element_text(size = 12, face = "plain"),
          axis.text.y = element_text(size = 12, face = "plain"),
          panel.grid.major.x = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill = "white"))
  
  ggsave(paste0("04.",c,"_propensity scores by cva_histogram_", var,".png"),
         plot = last_plot(),
         device = png(),
         path = "03 Results/03.1 Graphs/",
         scale = 1,
         dpi = 150,
         bg = 'white')
  dev.off()
  c <- c + 1
}



### Visual examination of the distribution in terms of matching variables [categorical] ----------------------- ----------------------- ----------------------- -----------------------

variables <- c("IP_A", "IP_B", "IP_C", "IP_D", "IP_E", 
               "host", "idp", "refugees", 
               "female", "disability", "married",
               "edu_primary", "edu_secondary", "edu_university", "edu_no_completion", "edu_no_education", "edu_other")
variables_label <- c("implementing partner (A)", 
                     "implementing partner (B)", 
                     "implementing partner (C)", 
                     "implementing partner (D)", 
                     "implementing partner (E)", 
                     "status (host)", "status (IDP)", "status (refugees)", 
                     "gender (female)", "disability status", "marital status",
                     "education (primary)", 
                     "education (secondary)", 
                     "education (university)", 
                     "education (no completion)", 
                     "education (no education at all)", 
                     "education (other)")

c <- 0                            #  counter for the sub-numbering of the graphs in 5 and 6
for (var in variables) {
  data2 <- data[,c("cva", var)] 
  colnames(data2)[2] <- "variable" 
  data_summary <- data2 %>%
    group_by(cva) %>%
    summarise(value = mean(variable) * 100) %>%
    mutate(cva = paste("CVA", cva))
  
  data_PSM2 <- data_PSM[,c("cva", var)]
  colnames(data_PSM2)[2] <- "variable" 
  data_PSM_summary <- data_PSM2 %>%
    group_by(cva) %>%
    summarise(value = mean(variable) * 100) %>%
    mutate(cva = paste("CVA", cva))
  
  plot1 <- ggplot(data_summary, aes(x = cva, y = value)) +
    geom_bar(stat = "identity", position = "dodge", fill = "#0072BC") + 
    labs(title = paste0("All data ", "(n = ", nrow(data), ")"),
         y = "proportion (in %)") +
    geom_text(aes(y = value, 
                  label = paste0(round(value, 2), "%")), 
              position = position_dodge(width = 0.9),
              vjust= -1,
              size = 5, 
              color = "black") + 
    scale_y_continuous(limits = c(0, ceiling ((max(data_summary$value, data_PSM_summary$value) + 5)/ 5) * 5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey"),
          axis.title = element_text(face = "plain"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12, face = "plain"),
          axis.text.y = element_text(size = 12, face = "plain"),
          panel.grid.major.x = element_blank())
  
  plot2 <- ggplot(data_PSM_summary, aes(x = cva, y = value)) +
    geom_bar(stat = "identity", position = "dodge", fill = "#0072BC") + 
    labs(title = paste0("Matched data ", "(n = ", nrow(data_PSM), ")"),
         y = "proportion (in %)") +
    geom_text(aes(y = value, 
                  label = paste0(round(value, 2), "%")), 
              position = position_dodge(width = 0.9),
              vjust= -1,
              size = 5, 
              color = "black") + 
    scale_y_continuous(limits = c(0, ceiling ((max(data_summary$value, data_PSM_summary$value) + 5)/ 5) * 5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey"),
          axis.title = element_text(face = "plain"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12, face = "plain"),
          axis.text.y = element_text(size = 12, face = "plain"),
          panel.grid.major.x = element_blank())
  
  c <- c +1
  
  combined_plot <- (plot1 + plot2)  + 
    plot_annotation(#title = "Comparison of all data with matched data",
                    subtitle = paste0("Matching variable: ", variables_label[which(var == variables)]),
                    caption = "Source: endine 2023 \n© CONSORTIUM"
    )
  print(combined_plot)
  
  ggsave(paste0("05.",c, " Comparison of all data with matched data_", var,".png"),
         plot = last_plot(),
         device = png(),
         path = "03 Results/03.1 Graphs/",
         scale = 1,
         dpi = 150,
         bg = 'white')
  dev.off()
  
}
rm(data2, data_PSM2)



### Visual examination of the distribution in terms of matching variables [continuous] ----------------------- ----------------------- ----------------------- -----------------------

variables <- c("age",  "children")
variables_label <- c("age", "number of children")

for (var in variables) {
  data2 <- data[,c("cva", var)] 
  colnames(data2)[2] <- "variable" 
  data_summary <- data2 %>%
    group_by(cva) %>%
    summarise(value = mean(variable)) %>%
    mutate(cva = paste("CVA", cva))
  
  
  data_PSM2 <- data_PSM[,c("cva", var)]
  colnames(data_PSM2)[2] <- "variable" 
  data_PSM_summary <- data_PSM2 %>%
    group_by(cva) %>%
    summarise(value = mean(variable)) %>%
    mutate(cva = paste("CVA", cva))
  
  plot1 <- ggplot(data_summary, aes(x = cva, y = value)) +
    geom_bar(stat = "identity", position = "dodge", fill = "#0072BC") + 
    labs(title = paste0("All data ", "(n = ", nrow(data), ")"),
         y = "proportion (in %)") +
    geom_text(aes(y = value, 
                  label = round(value, 2)), 
              position = position_dodge(width = 0.9),
              vjust= -1,
              size = 5, 
              color = "black") + 
    scale_y_continuous(limits = c(0, ceiling ((max(data_summary$value, data_PSM_summary$value) + 5)/ 5) * 5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey"),
          axis.title = element_text(face = "plain"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12, face = "plain"),
          axis.text.y = element_text(size = 12, face = "plain"),
          panel.grid.major.x = element_blank())
  
  plot2 <- ggplot(data_PSM_summary, aes(x = cva, y = value)) +
    geom_bar(stat = "identity", position = "dodge", fill = "#0072BC") + 
    labs(title = paste0("Matched data ", "(n = ", nrow(data_PSM), ")"),
         x = "CVA status",
         y = "proportion (in %)") +
    geom_text(aes(y = value, 
                  label = round(value, 2)), 
              position = position_dodge(width = 0.9),
              vjust= -1,
              size = 5, 
              color = "black") + 
    scale_y_continuous(limits = c(0, ceiling ((max(data_summary$value, data_PSM_summary$value) + 5)/ 5) * 5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey"),
          axis.title = element_text(face = "plain"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12, face = "plain"),
          axis.text.y = element_text(size = 12, face = "plain"),
          panel.grid.major.x = element_blank())
  
  c <- c +1
  combined_plot <- (plot1 + plot2)  + 
    plot_annotation(title = "Comparison of all data with matched data",
                    subtitle = paste0("Matching variable: ", variables_label[which(var == variables)]),
                    caption = "Source: endine 2023 \n© CONSORTIUM"
    )
  print(combined_plot)
  
  ggsave(paste0("05.",c, " Comparison of all data with matched data_", var,".png"),
         plot = last_plot(),
         device = png(),
         path = "03 Results/03.1 Graphs/",
         scale = 1,
         dpi = 150,
         bg = 'white')
  dev.off()
  
}



### Visual examination of the distribution in terms of matching variables [outcome] ----------------------- ----------------------- ----------------------- -----------------------

variables <- c("safer", "parenting_better", "relationship_better", "lessworried", "income" )
variables_label <- c("being safe", "doing better parenting", " having better relationships", "being less worried", "income")

c <- 0                            #  counter for the sub-numbering of the graphs in 7 
for (var in variables) {
  
  if (var == "income") {
    factor <- 1
    factor2 <- 5
  } else {
    factor <- 100
    factor2 <- 5
  }

  data2 <- data[,c("cva", var)] 
  colnames(data2)[2] <- "variable" 
  data_summary <- data2 %>%
    group_by(cva) %>%
    summarise(value = mean(variable) * factor) %>%
    mutate(cva = paste("CVA", cva))
  
  data_PSM2 <- data_PSM[,c("cva", var)]
  colnames(data_PSM2)[2] <- "variable" 
  data_PSM_summary <- data_PSM2 %>%
    group_by(cva) %>%
    summarise(value = mean(variable) * factor) %>%
    mutate(cva = paste("CVA", cva))
  
  plot1 <- ggplot(data_summary, aes(x = cva, y = value)) +
    geom_bar(stat = "identity", position = "dodge", fill = "#0072BC") + 
    labs(title = paste0("All data ", "(n = ", nrow(data), ")"),
         y = "proportion (in %)") +
    geom_text(aes(y = value, 
                  label = round(value, 2)), 
              position = position_dodge(width = 0.9),
              vjust= -1,
              size = 5, 
              color = "black") + 
    scale_y_continuous(limits = c(0, ceiling ((max(data_summary$value, data_PSM_summary$value) + factor2)/ 5) * 5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey"),
          axis.title = element_text(face = "plain"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12, face = "plain"),
          axis.text.y = element_text(size = 12, face = "plain"),
          panel.grid.major.x = element_blank())
  
  plot2 <- ggplot(data_PSM_summary, aes(x = cva, y = value)) +
    geom_bar(stat = "identity", position = "dodge", fill = "#0072BC") + 
    labs(title = paste0("Matched data ", "(n = ", nrow(data_PSM), ")"),
         y = "proportion (in %)") +
    geom_text(aes(y = value, 
                  label = round(value, 2)), 
              position = position_dodge(width = 0.9),
              vjust= -1,
              size = 5, 
              color = "black") + 
    scale_y_continuous(limits = c(0, ceiling ((max(data_summary$value, data_PSM_summary$value) + factor2)/ 5) * 5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey"),
          axis.title = element_text(face = "plain"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12, face = "plain"),
          axis.text.y = element_text(size = 12, face = "plain"),
          panel.grid.major.x = element_blank())
  
  c <- c +1
  combined_plot <- (plot1 + plot2)  + 
    plot_annotation(title = "Comparison of all data with matched data",
                    subtitle = paste0("Matching variable: ", variables_label[which(var == variables)]),
                    caption = "Source: endine 2023 \n© CONSORTIUM"
    )
  print(combined_plot)
  
  ggsave(paste0("06.",c, " Outcome Comparison of all data with matched data_", var,".png"),
         plot = last_plot(),
         device = png(),
         path = "03 Results/03.1 Graphs/",
         scale = 1,
         dpi = 150,
         bg = 'white')
  dev.off()
}
rm(c, var, variables, variables_label)



### Visual examination of the distribution in terms of matching variables ----------------------- ----------------------- ----------------------- -----------------------

variables <- c("IP_A", "IP_B", "IP_C", "IP_D", "IP_E", 
               "host", "idp", "refugees", 
               "age",  "children",
               "female", "disability", "married",
               "edu_primary", "edu_secondary", "edu_university", "edu_no_completion", "edu_no_education", "edu_other")
variables_label <- c("implementing partner (A)", 
                     "implementing partner (B)", 
                     "implementing partner (C)", 
                     "implementing partner (D)", 
                     "implementing partner (E)", 
                     "status (host)", "status (IDP)", "status (refugees)", 
                     "age", "# of children", "gender (female)", "disability status", "marital status",
                     "education (primary)", 
                     "education (secondary)", 
                     "education (university)", 
                     "education (no completion)", 
                     "education (no education at all)", 
                     "education (other)")
c <- 0                            #  counter for the sub-numbering of the graphs in 8
for (var in variables) {
  data_PSM2 <- data_PSM[,c("distance", "cva", var)]
  colnames(data_PSM2)[3] <- "variable" 
  
  ggplot(data_PSM2, aes(x = distance, y = variable, color = cva)) +
         geom_point(size = 1.3) +
         geom_smooth(method = "loess", se = FALSE) +
         xlab("Propensity score") +
         ylab(var) +
         scale_color_manual(values = c("received" = "#0072BC", "not received" = "#8EBEFF")) +
         labs(
              #title = "Relationship between matching variables and propensity scores \nacross comparison groups",
              subtitle = paste0("Matched data only (n = ", nrow(data_PSM),")"),
              y = "",
              x = "Propensity scores",
              caption = "Source: endine 2023 \n© CONSORTIUM") +
         theme(panel.background = element_rect(fill = "white"),
               panel.grid.major = element_line(color = "grey"),
               axis.title = element_text(face = "plain"),
               axis.text.x = element_text(size = 12, face = "plain"),
               axis.text.y = element_text(size = 12, face = "plain"),
               panel.grid.major.x = element_blank()) 
  
  c <- c + 1
  
  ggsave(paste0("07.",c, " Relationship between matching variables and propensity scores_", var,".png"),
         plot = last_plot(),
         device = png(),
         path = "03 Results/03.1 Graphs/",
         scale = 1,
         dpi = 150,
         bg = 'white')
  dev.off()
}



### Estimating treatment effects ----------------------- ----------------------- ----------------------- -----------------------

m_i0 <- glm(income ~ cva,
           family = gaussian(), data = data)
summary(m_i0)

m_i <- glm(income ~ cva + IP_B + IP_C + IP_D + IP_E + 
               idp + refugees + 
               age2 + female + disability + married + children + 
               edu_no_education + edu_no_completion + edu_primary + edu_secondary + edu_university,
             family = gaussian(), data = data)
summary(m_i)

   # Expression of cva effect in terms of standard deviations 

es_unadjusted <- m_i$coefficients[2]/sd(data$income) 



psm_i0 <- glm(income ~ cva,
             family = gaussian(), data = data_PSM)
summary(psm_i0)


psm_i <- glm(income ~ cva + IP_B + IP_C + IP_D + IP_E + 
             idp + refugees + 
               age2 + female + disability + married + children + 
               edu_no_education + edu_no_completion + edu_primary + edu_secondary + edu_university,
             family = gaussian(), data = data_PSM)
summary(psm_i)

# Expression of cva effect in terms of standard deviations 

es_adjusted <-psm_i$coefficients[2]/sd(data$income) 



### Exporting regression results ----------------------- ----------------------- ----------------------- -----------------------

stargazer(m_i0, m_i, psm_i0, psm_i, 
          type="html",
          column.labels = c("1", "2", "3", "4"),
          dep.var.labels=c("Average income attained"),
          model.numbers = FALSE,
          covariate.labels=c("Cash vouchers received", 
                             "Beneficiary of partner B" , "Beneficiary of partner C", "Beneficiary of partner D", "Beneficiary of partner E",
                             "Being an IDP", "Being a refugee", "Age (standardized)", "Being female", "Being disabled",
                             "Being married", "# of children",
                             "Having no education", "Having no degree", 
                             "Primary education completed", "Secondary education completed", "University completed"),
          title = "Estimated CVA effects on income",
          digits = 2,
          out = "03 results/03.2 Tables/Table 5_Estimated CVA effects_20240911_V01.htm")

   # In terms of percentage, it appears that the actual effect size higher by about 

round((es_adjusted - es_unadjusted) / es_unadjusted, 4)*100





