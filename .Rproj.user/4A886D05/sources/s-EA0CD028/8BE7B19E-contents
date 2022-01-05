install.packages("data.table")
library(ggplot2)
library(data.table)
library(svglite)
mtt = read.csv("mtt.csv")
colnames(mtt) = c("Treated", "Untreated", "Time")
day_1_t_test = t.test(mtt[(1:4), 1], mtt[(1:4), 2])
day_2_t_test = t.test(mtt[(5:8), 1], mtt[(5:8), 2])
setDT(mtt)
mttMelt = melt(mtt, id.vars = "Time")
mttMelt_agg = mttMelt[, .(mean = mean(value), se = sd(value)/.N), by = .(Time, variable)]
ggplot(mttMelt_agg, 
       aes(x = Time, y = mean, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymax = mean + 1.96*se, 
                    ymin = mean - 1.96*se,
                    width = 0.2),
                position = position_dodge(0.9)) +
  ylab("MTT Absorbance") +
  theme_classic() +
  labs(fill = "") + 
  scale_fill_grey() +
  theme(axis.title.x = element_blank(), 
        axis.text.y = element_text(size=10, face = "bold", color = "black"),
        axis.title = element_text(size=10, color = "black", face = "bold"),
        axis.text.x= element_text(size=10, face = "bold", color = "black"),
        legend.text = element_text(size = 10),
        legend.position="bottom")
ggsave("mtt.svg", width = 80, height = 80, units = "mm")