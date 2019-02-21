setwd("~/Desktop/bryn/bryn")
library(tidyverse)
library(hrbrthemes)
library(factoextra)
library(FactoMineR)
library(lubridate)
library(MASS)
library(ordinal)
library(sjPlot)
library(VIM)
library(cowplot)

set.seed(1002)

data <- readRDS("survey_data.rds")
hist(data$total_batsbi)

summary(data$total_batsbi)

data.ca <- data %>% dplyr::select(11:18) %>% gather(key="place",value="extent")
data.ca$extent <- factor(data.ca$extent, levels = c("Never","Sometimes","Often","Always"), ordered=T)
data.ca.table <- table(data.ca)
res.ca <- CA(data.ca.table, ncp=5,graph=F)

fviz_screeplot(res.ca, addlabels = TRUE)
plot1 <- fviz_ca_biplot(res.ca, repel = TRUE, ggtheme= theme_ipsum(),title = "Correspondence analysis")
ggsave("ca-biplot.png", device="png",width=7,height=5, units="in", dpi=350)


fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
fviz_ca_col(res.ca, col.col = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
fviz_contrib(res.ca, choice = "row", axes = 1, top = 10)
fviz_contrib(res.ca, choice = "row", axes = 2, top = 10)
fviz_contrib(res.ca, choice = "col", axes = 1, top = 10)
fviz_contrib(res.ca, choice = "col", axes = 2, top = 10)

res.hcpc <- HCPC(res.ca, nb.clust=-1)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.5      # Augment the room for labels
)

plot2 <- fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_ipsum(),
             main = "Hierarchical clusters of correspondence analysis"
             
)
plot_grid(plot1, plot2, labels = c("A", "B"), ncol = 2)
ggsave("ca-clusters-full.png", device="png",width=12,height=6, units="in", dpi=350)


ggsave("ca-clusters.png", device="png",width=7,height=7, units="in", dpi=350)

## arranging plots
library(cowplot)
cluster.plot<-fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_ipsum(),
             main = "Correspondence analysis grouped by hierarchical cluster"
             
)

ca.plot<- fviz_ca_biplot(res.ca, repel = TRUE, ggtheme= theme_ipsum(),title="Correspondence analysis for level of \ndomain-based Tsova-Tush use")
plot_grid(ca.plot, cluster.plot, labels = c("A", "B"), align = "h")
ggsave("fig2.png", device="png",width=14,height=7, units="in", dpi=350)

data.ca$cluster <- ifelse(data.ca$place == "use_bbl_at_doctor" | data.ca$place == "use_bbl_at_work", 1, 
                          ifelse(data.ca$place == "use_bbl_at_market", 2,
                                 ifelse(data.ca$place == "use_bbl_with_parents" | data.ca$place == "use_bbl_with_spouse", 3, 4)))
data.ca$cluster <- as.factor(data.ca$cluster)
ggplot(data.ca, aes(x=place, fill=extent)) + geom_bar(position="dodge") + theme_ipsum(grid="Yy")+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) # cleanup plot
ggsave("domains.png", device="png",width=7,height=7, units="in", dpi=350)

data.ca <- droplevels(data.ca)
ggplot(data.ca, aes(x=place, fill=extent)) + geom_bar(position="dodge") + theme_ipsum(grid="Yy") + facet_wrap(~cluster, drop=T)+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) # cleanup plot
ggsave("domains-clustered.png", device="png",width=7,height=7, units="in", dpi=350)
### mca
data.mca <- data %>% dplyr::select(10:17, age) 

res.mca <- MCA(data.mca, graph=F,quanti.sup=9)

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
fviz_mca_biplot(res.mca, repel = TRUE,
                ggtheme = theme_minimal())

fviz_mca_var(res.mca, choice = "mca.cor",
             repel = TRUE)
fviz_mca_var(res.mca, repel = TRUE,
             ggtheme= theme_minimal())
fviz_mca_var(res.mca, choice = "quanti.sup",
             ggtheme = theme_minimal())
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

###### regression #####
library(stargazer)
data$importance_IGT <- factor(data$importance_IGT, levels=c("Not important at all", "Somewhat important", "Very important", "Crucial"), ordered=T)
data$age.centered <- scale(data$age, scale=F, center=T)

m <- clm(importance_IGT ~ age.centered+self_rating_bbl+native_batsbi+native_georgian+gender+city_residence, data = data)
summary(m)
exp(0.12230)

stargazer(m,style="ajs")



plot_model(m)
summary(data$gender)

m1 <- glm.nb(total_batsbi ~ age.centered+native_batsbi+gender+native_georgian ,
             data = data, control=glm.control(maxit=300, epsilon=1e-10))
m2 <- glm(total_batsbi ~ age.centered+native_batsbi+native_georgian+gender ,
             data = data, family="poisson")
m3 <- pscl::zeroinfl(total_batsbi ~ age.centered+gender +native_batsbi  | native_georgian,
               data = data, dist = "negbin")

car::vif(lm(total_batsbi ~ age.centered+native_batsbi+gender+native_georgian, data=data))
summary(m1)
summary(m2)
summary(m3)
ggplot(data, aes(x=importance_IGT, y=age)) + geom_violin(trim=T, fill="gray") + geom_boxplot(width=0.1) + theme_ipsum(grid="Yy") +
  labs(
    title="How important is it for young people to know Tsova-Tush",
    subtitle="Responses by age",
    caption="",
    x="",
    y="Age of respondent"
  ) + ylim(20,100)
ggsave("igt-age.png", device="png",width=7,height=7, units="in", dpi=350)
summary(lm(self_rating_bbl~age+gender+native_batsbi+native_georgian,data=data))

ggplot(data, aes(x=age, y=self_rating_bbl)) +geom_point() + theme_ipsum(grid="Yy") +
  labs(
    title="How important is it for young people to know Tsova-Tush?",
    subtitle="Responses by age",
    caption="",
    x="",
    y="Age"
  ) + geom_smooth(method=lm,   # Add linear regression line
                  se=T)

data.rgression <- data %>% dplyr::select(age, self_rating_bbl,gender,native_batsbi,native_georgian)
plot(data.rgression)


## summary tables
library(reporttools)

vars1 <- data[, c("age", "self_rating_bbl")]
cap1 <- "Patient characteristics: nominal variables."
tableContinuous(vars = vars1, cap = cap1, vertical = FALSE, lab =    "tab: nominal1", longtable = FALSE,
                stats=c("n","min","max","mean"))
vars2 <- data[, c("gender", "native_batsbi","native_georgian","city_residence")]
cap1 <- "Patient characteristics: nominal variables."
tableNominal(vars = vars2, cap = cap1, vertical = FALSE, lab =    "tab: nominal1", longtable = FALSE)

