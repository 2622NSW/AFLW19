install.packages("devtools")
library(devtools)
devtools::install_github("thomasp85/patchwork")
install.packages("ggplot2")
library(ggplot2)
install.packages("patchwork")
library(patchwork)
df <- read.csv("AFLW2019.csv")
df1 <- subset(df, WinningTotal >= 49 & WinningTotal < 80)
df2 <- subset(df, WinningTotal > 15 & WinningTotal < 49)
df3 <- read.csv("AF.csv")
p1 <- ggplot(df, aes(GameID, WinningTotal)) +
  geom_line(colour="firebrick")+
  geom_point(size=1, colour="firebrick")+
  labs(title = "AFLW 2019", subtitle="Winning Totals",
       x="Game", y = "Winning Total (Points Scored)")+
scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,37))+
  scale_y_continuous(breaks= c(10, 20, 30, 40, 50, 60, 70, 80))+
  theme_minimal()
p1
p2 <- ggplot(df, aes(Round, WinningMargin))+
  geom_point(colour="blue")+
  labs(title = "AFLW 2019", subtitle="Winning Margins",
       x="Week of the Season", y = "Winning Margin (Points Scored)")+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9))+
  theme_minimal()
p2
p1 + p2
p1 + p2 + plot_layout(ncol = 1, heights = c(2, 2))
p1 + plot_spacer() + p2
p3 <- ggplot(df, aes(WinningTotal, WinningMargin))+
  geom_smooth()+
  geom_point()+
  labs(title = "AFLW 2019", subtitle="Winning Totals",
       x="Winning Total (Points Scored)", y = "Winning Margin")
p3
p4 <- ggplot(df, aes(LosingMargin))+
  geom_bar(fill = "darkgreen")+
  labs(title = "AFLW 2019", subtitle="Losing Margins",
       x="Losing Margin (Points)", y="Frequency")+
  xlim(-1, -70)+
  theme_minimal()
p4
p4 + {
  p1 + {
    p2 +
      p3 +
      plot_layout(ncol = 1)
  }
} +
  plot_layout(ncol = 1)
p1 + p2 + p3 + plot_layout(ncol = 1)
p1 + p2 - p3 + plot_layout(ncol = 1)
(p1 | p2 | p3) /
  p4
(p1 + (p2 + p3) + p4 + plot_layout(ncol = 1)) * theme_minimal()
p1 + (p2 + p3) + p4 + plot_layout(ncol = 1) & theme_minimal()
library(ggrepel)
p5 <- ggplot(df, aes(WinningTotal, WinningMargin))+
  geom_point(aes(colour=WinningMargin))+
  geom_vline(xintercept = 49)+
  annotate("text", x=56, y=62, label = "Median = 49")+
  annotate ("text", x=25, y =27, label= "Median = 22")+
  geom_hline(yintercept=22)+
  labs(title = "AFLW 2019", subtitle="Winning Margins",
       x="Winning Total (Points Scored)", y="Winning Margin (Points)")+
  theme_minimal()
p5

p6 <- ggplot(df1, aes(WinningTotal, WinningMargin, label=WinningTeam))+
  geom_point()+
  geom_label_repel(size=2.5)+
  geom_vline(xintercept = 49)+
  geom_hline(yintercept=22)+
  labs(title = "AFLW 2019", subtitle="Winning Total: More than 49 Points Scored",
       x="Winning Total (Points Scored)", y="Winning Margin (Points)")+
  theme_minimal()
p6
p7 <- ggplot(df2, aes(WinningTotal, WinningMargin, label=WinningTeam))+
  geom_point()+
  geom_label_repel(size=2.5)+
  geom_vline(xintercept = 49)+
  geom_hline(yintercept=22)+
  labs(title = "AFLW 2019", subtitle="Winning Total: Less than 49 Points Scored",
       x="Winning Total (Points Scored)", y="Winning Margin (Points)")+
  theme_minimal()
p7
p6 + p7
p5 + p6 + p7 + plot_layout(ncol = 1)
p5 + p6 - p7 + plot_layout(ncol = 1)
p1 +theme_minimal()
p2 + theme_minimal()
p3
p4
p5 + theme_minimal()
p6
p7
p8
p9
p10
p8 <- ggplot(df3, aes(Quarter))+
  scale_colour_manual(values = c("red", "blue"))+
  geom_step(aes(y = ASM, colour = "Adelaide's Median Score Per Quarter")) + 
  geom_step(aes(y = ASFinal, colour= "Adelaide's Score in the Final"))+
  labs(title="AFLW 2019", subtitle = "Adelaide's Points Scoring Season", 
       x="Quarter", y="Points Scored Per Quarter")+
  ylim(0, 40)+
  theme_minimal()
p8
p10 <- ggplot(df3, aes(Quarter))+
  scale_colour_manual(values = c("red", "blue"))+
  geom_step(aes(y = ACM, colour = "Median Points Conceded Season")) + 
  geom_step(aes(y = ACFinal, colour= "Points Conceded in the Final"))+
  labs(title="AFLW 2019", subtitle = "Adelaide's Points Conceding Season", 
       x="Quarter", y="Points Conceded Per Quarter")+
  ylim(0, 40)+
  theme_minimal()
p10
p9 <- ggplot(df3, aes(Quarter))+
  scale_colour_manual(values = c("red", "blue"))+
  geom_step(aes(y = ASPrior, colour = "Adelaide's Probability of Scoring")) + 
  geom_step(aes(y = ASPost, colour= "Adelaide's Scoring Pattern in the Final"))+
  labs(title="AFLW 2019", 
       subtitle = "Adelaide's Season: Probability of Scoring as Priors and Posteriors", 
       x="Quarter", y="Probability")+
  ylim(0, 1)+
  theme_minimal()
p9
p10 <- ggplot(df3, aes(Q))+
  scale_colour_manual(values = c("red", "blue"))+
  geom_step(aes(y = ACPrior, colour = "Adelaide's Probability of Conceding")) + 
  geom_step(aes(y = ACPost, colour= "Adelaide's Conceding Pattern in the Final"))+
  labs(title="AFLW 2019", 
       subtitle = "Adelaide's Season: Probability of Conceding as Priors and Posteriors", 
       x="Quarter", y="Probability")+
  ylim(0, 1)+
  theme_minimal()
p10
# https://cran.r-project.org/web/packages/ggforce/vignettes/Visual_Guide.html
library(tidyverse)
library(ggforce)
install.packages("hrbrthemes")
library(hrbrthemes)
p11 <- ggplot(df, aes(WinningTotal, WinningMargin))+
  geom_point()+
  geom_mark_rect(aes(fill="Scored more than 48 Points",
                     filter=WinningTotal >= 49
  ))+
  labs(title = "AFLW 2019", subtitle="Winning Total: More than 48 Points Scored",
       x="Winning Total (Points Scored)", y="Winning Margin (Points)")+
  theme_minimal()
p11
p12 <- ggplot(df, aes(WinningTotal, WinningMargin))+
  geom_point()+
  geom_mark_rect(aes(fill="Scored more than 49 Points",
                     filter=WinningTotal < 49
  ))+
  labs(title = "AFLW 2019", subtitle="Winning Total: Less than 49 Points Scored",
       x="Winning Total (Points Scored)", y="Winning Margin (Points)")+
  theme_minimal()
p12
p13 <- ggplot(df, aes(WinningTotal, WinningMargin, colour=WinningTotal))+
  geom_point()+
  facet_zoom(x=WinningTotal)+
  labs(title = "AFLW 2019", subtitle="Winning Totals (37 Games)",
       x="Winning Total (Points Scored)", y="Winning Margin (Points)")+
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_y_continuous(breaks= c(10, 20, 30, 40, 50, 60, 70, 80))+
  theme_minimal()
p13
p14 <- ggplot(df, aes(WinningTotal, WinningMargin))+
  geom_point()+
  annotate("rect", xmin=49, xmax=80, ymin = 0, ymax = 80, fill="lightblue", 
           alpha = 0.3)+
  annotate("text", x=75, y=70, label = "Adelaide")+
  annotate("text", x=70, y=57, label = "Adelaide")+
  annotate("text", x=63, y=50, label = "Adelaide")+
  annotate("text", x=69, y=42, label = "Adelaide")+
  labs(title = "AFLW 2019", subtitle="Winning Total: More than 48 Points Scored",
       x="Winning Total (Points Scored)", y="Winning Margin (Points)")+
  theme_minimal()
p14
p15 <- ggplot(df, aes(WinningTotal, WinningMargin))+
  geom_point()+
  annotate("text", x=72, y=70, label = "Adelaide")+
  annotate("text", x=70, y=57, label = "Adelaide")+
  annotate("text", x=63, y=50, label = "Adelaide")+
  annotate("text", x=69, y=42, label = "Adelaide")+
  labs(title = "AFLW 2019", subtitle="Winning Total: More than 48 Points Scored",
       x="Winning Total (Points Scored)", y="Winning Margin (Points)")+
  theme_minimal()
p15