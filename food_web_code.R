

library(readxl)
library(csvread)
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(lmodel2)
library(tinytable)
library(patchwork)


#Read in the .csv files from R Folder 
Hg <- read.csv("./Data/Mercury.csv", fileEncoding="latin1")
# stringsAsFactors=FALSE,

Iso <- read.csv("./Data/StableIsotope.csv", fileEncoding="latin1")
# stringsAsFactors=FALSE,

Meta <- read.csv("./Data/SampleMetadata.csv", fileEncoding="latin1")
# stringsAsFactors=FALSE,

#rename Hg ID's to match SampleMetadata file
Hg <- rename(Hg, c("SampleNumber_Iso" = "FLBS_IsotopeID",
                   "SampleNumber_Hg" = "FLBS_HgID"))

#rename Iso ID's to match SampleMetadata file
Iso <- rename(Iso, c("SampleNumber_Iso" = "Sample.ID"))

####merge the two datasets with the Meta Data


Iso <- Iso %>%
  filter(!str_detect(SampleNumber_Iso, 'Replicate'))

MetaHg <-left_join(Meta,Hg, by="SampleNumber_Iso", "CollectionDate")

MetaHgSI <- left_join(MetaHg, Iso, by="SampleNumber_Iso", "CollectionDate")

##Clean up the dataset, Mercury concentrations are in ng/g or ppb

MetaHgSI <- rename(MetaHgSI, c("CollectionDate" = "CollectionDate.x",
                               "SampleNumber_Hg" = "SampleNumber_Hg.x",
                               "MeHg_dw"= "MeHg_conc_ng.g.dw_ppb",
                               "MeHg_ww"= "MeHg_conc_ng.g.ww_ppb",
                               "THg_dw"= "THg_conc_ng.g.dw_ppb",
                               "THg_ww"= "THg_conc_ng.g.ww_ppb",
                               "d15N" = "X15N",
                               "d13C" = "X13C",
                               "Percent_MeHg" = "X.MeHg"
))


FinalHg <- select(MetaHgSI, -c(CollectionDate.y, SampleNumber_Hg.y, FieldSpeciesCode, Species))

FinalHg <- rename(FinalHg, c("SpeciesCode" = "CERL_SpeciesCode"
                             
))

FinalHg$SpeciesCode <- recode(FinalHg$SpeciesCode, "BEIN" = "CHIR",
                              "ZOOP" = "DAPH"
) 


#check the types of data that are in the dataframe
str(FinalHg)

FinalHg$CollectionDate

#Convert the date from a Character to a Date data type
FinalHg$Date <- mdy(FinalHg$CollectionDate)

#check that the date was converted correctly
#DateCheck <- data.frame(FinalHg$CollectionDate, FinalHg$Date)
#View(DateCheck)
#remove(DateCheck)


#add some useful date year combinations for graphical purposes

FinalHg$MonthNumerical <- format(FinalHg$Date, "%m")

FinalHg$Month <- format(FinalHg$Date, "%B")

FinalHg$Year <- format(FinalHg$Date, "%Y")

FinalHg$Month_Year <- format(FinalHg$Date, "%B %Y")

str(FinalHg)

#Take the log base e not base 10 of the MeHg
FinalHg <- FinalHg %>%
  #Creating an empty column:
  add_column(LogMeHg = NA, .after="MeHg_dw")

FinalHg <- FinalHg %>% mutate(LogMeHg = log(MeHg_dw))



##Just the fish

Fish <- FinalHg %>% filter(SpeciesCode %in% c("BUTR", "LATR", "LAWF"))

Fish$Month_Year <- as.factor(as.character(Fish$Month_Year))

daph15N<- mean(FinalHg$d15N[FinalHg$SpeciesCode=="DAPH"])
latr15N<- mean(FinalHg$d15N[FinalHg$SpeciesCode=="LATR"], na.rm=T)

latr15N-daph15N

(latr15N-daph15N)/3.6


##Fish by species

latr<- FinalHg[FinalHg$SpeciesCode=="LATR", ]

latr$bc<- 1e5* latr$Weight_g / latr$Length_mm^3

butr<- FinalHg[FinalHg$SpeciesCode=="BUTR", ]
lawf<- FinalHg[FinalHg$SpeciesCode=="LAWF", ]





########
#######
#models

##model for total 15N and Hg
tm<- lm(log(MeHg_dw)~d15N, data=FinalHg)
summary(tm)

#coef CI
coef(tm)[2] 
(coef(tm)[2] ) - 2*coef(summary(tm))[2, "Std. Error"]
(coef(tm)[2] ) + 2*coef(summary(tm))[2, "Std. Error"]


###log10slope and ci
coef(tm)[2] /2.303
(coef(tm)[2] /2.303) - 2*coef(summary(tm))[2, "Std. Error"]/2.303
(coef(tm)[2] /2.303) + 2*coef(summary(tm))[2, "Std. Error"]/2.303

### model for lake trout length and MeHg


mean(latr$MeHg_ww[latr$Length_mm<(24*25.4)])
exp(mean(log(latr$MeHg_ww[latr$Length_mm<(24*25.4)])))

latr_length_fit<- lm(log(MeHg_dw)~ Length_mm , data= latr)
summary(latr_length_fit)
0.0032696 - 2*0.0001474

butr_length_fit<- lm(log(MeHg_dw)~ Length_mm , data= butr)
summary(butr_length_fit)
0.001220 + 2*  0.000332
0.001220 - 2*  0.000332

lawf_length_fit<- lm(log(MeHg_dw)~ Length_mm , data= lawf)
summary(lawf_length_fit)
0.0031738  + 2*0.0006653
0.0031738  - 2*0.0006653

latr_d15N_fit<- lm(log(MeHg_dw)~ d15N , data= latr)
summary(latr_d15N_fit)
0.06356  + 2*  0.12994
0.06356  - 2*  0.12994

butr_d15N_fit<- lm(log(MeHg_dw)~ d15N , data= butr)
summary(butr_d15N_fit)
0.34123  + 2*  0.04292
0.34123  - 2*  0.04292

lawf_d15N_fit<- lm(log(MeHg_dw)~ d15N , data= lawf)
summary(lawf_d15N_fit)
0.32534   +2* 0.07561  
0.32534   -2* 0.07561 


##body condition
plot(latr$bc, latr$LogMeHg)
summary(lm(latr$LogMeHg~latr$bc))


###isotope exploration

plot(LogMeHg ~d13C, data = Fish[Fish$SpeciesCode == "LATR",])

##MLR models for iso and 15N for each species.

ltfit <- glm(LogMeHg ~ Length_mm + d15N, data = Fish[Fish$SpeciesCode == "LATR",])
summary(ltfit)

ltfit_C <- lm(LogMeHg ~ Length_mm + d15N+d13C, data = Fish[Fish$SpeciesCode == "LATR",])
summary(ltfit_C)




btfit <- lm(LogMeHg ~ Length_mm + d15N, data = Fish[Fish$SpeciesCode == "BUTR",])
summary(btfit)

btfit_C <- lm(LogMeHg ~ Length_mm + d15N + d13C, data = Fish[Fish$SpeciesCode == "BUTR",])
summary(btfit_C)

btfit <- lm(LogMeHg ~ Length_mm + d15N, data = Fish[Fish$SpeciesCode == "BUTR",])
summary(btfit)


lwfit <- glm(LogMeHg ~ Length_mm + d15N, data = Fish[Fish$SpeciesCode == "LAWF",])
summary(lwfit)

lwfit_C <- lm(LogMeHg ~ Length_mm + d15N+d13C, data = Fish[Fish$SpeciesCode == "LAWF",])
summary(lwfit_C)

coef(summary(lwfit))

##Make table of coefficients
fit_df<- data.frame(
  Species = c("Lake Trout", "Bull Trout", "Lake Whitefish"),
  "$\\beta_{length}$"= c(coef (ltfit)[2],coef (btfit)[2],coef (lwfit)[2]),
  " SE $\\beta_{length}$" = c(coef(summary(ltfit))[2, "Std. Error"],coef(summary(btfit))[2, "Std. Error"],coef(summary(lwfit))[2, "Std. Error"]),
  "$\\beta_{15N}$"=  c(coef (ltfit)[3],coef (btfit)[3],coef (lwfit)[3]),
  "SE $ \\beta_{15N}$" = c(coef(summary(ltfit))[3, "Std. Error"],coef(summary(btfit))[3, "Std. Error"],coef(summary(lwfit))[3, "Std. Error"]),
  "$ R^2$" = c(summary(ltfit)$adj.r.squared,summary(btfit)$adj.r.squared,summary(lwfit)$adj.r.squared),
check.names = FALSE)



table_params<-tt(fit_df) |> format_tt(digits = 2, num_fmt = "significant_cell") |> print("latex")



###Figures


#Set colors for the different species
daph_col<-"#009E73"
chir_col<-"#F0E442"
mysi_col<- "#56B4E9"
butr_col<-"#0072B2"
  lawf_col<-"#E69F00"
  latr_col<- "#D55E00"



point_color <- c(butr_col, chir_col, daph_col, latr_col, lawf_col, mysi_col)


tm_plot<-ggplot(FinalHg, aes(x=d15N, y=MeHg_dw, color=SpeciesCode)) + geom_point(size=1, na.rm = TRUE) +
  #geom_hline(yintercept=2035,  color="red", size=0.5)+
  scale_y_continuous(trans='log10') +
  scale_colour_manual(values=point_color) +
  
  labs(title = "",
       subtitle = "",
       #caption =,
       #tag = "Figure 1",
       x =  expression(paste(delta^15, "N", " (\u2030)")),
       y = "MeHg (ng/g dry mass)") +
  
  theme_classic()   + 
#theme(plot.margin = margin(1, 1, 0.1, 0.5, "cm") ) +
  geom_segment(aes(x = 3, y = exp(2.2+0.42*3), xend = 12, yend = exp(2.2+0.42*12)),  color="black")


ggsave("figures/tmplot.pdf", 
       encoding="MacRoman",
       width = 5, #size that you want
       height = 4, #size that you want
       units = "in")


###Isotpoe biplot
isoplot<-ggplot(FinalHg, aes(x=d13C, y=d15N, color=SpeciesCode)) + geom_point(size=1, na.rm = TRUE) +
  scale_colour_manual(values=point_color) +

  labs(
    #caption =,
    #tag = "Figure 1",
    x = expression(paste(delta^13, "C", " (\u2030)")),
    y = expression(paste(delta^15, "N", " (\u2030)")) ) +
  
  theme_classic() 

ggsave("figures/isoplot.pdf", 
       encoding="MacRoman",
       width = 6, #size that you want
       height = 6, #size that you want
       units = "in")



fish_color<- c(butr_col, latr_col, lawf_col)

fish_15N_plot <- ggplot(Fish, aes(x=d15N, y=MeHg_dw, color=SpeciesCode)) + geom_point(size=1, na.rm = TRUE) +
  scale_y_continuous(trans='log10') +
  scale_colour_manual(values=fish_color) +
  
  labs(title = "",
       subtitle = " ",
       #caption =,
       #tag = "Figure 1",
       x = expression(paste(delta^15, "N", " (\u2030)")),
       y = "MeHg (ng/g)") +
  
  theme_classic() + geom_smooth(method = "lm", se = FALSE)

fish_length_plot<-ggplot(Fish, aes(x=Length_mm, y=MeHg_dw, color=SpeciesCode)) + geom_point(size=1, na.rm = TRUE) +
  scale_y_continuous(trans='log10') +
  scale_colour_manual(values=fish_color) +
  
  
  labs(title = "",
       subtitle = " ",
       #caption =,
       #tag = "Figure 1",
       x = "Length (mm)",
       y = "MeHg (ng/g)") +
  
  theme_classic() + geom_smooth(method = "lm", se = FALSE)

fish_combo_plot<- ggarrange(fish_length_plot, fish_15N_plot,  ncol = 2, nrow = 1, common.legend = TRUE)

ggsave("figures/fish_combo_plot.pdf", 
       encoding="MacRoman",
       width = 6, #size that you want
       height = 3.5, #size that you want
       units = "in")



##########
#length vs 15N for supplement
fish_covarN_plot<-ggplot(Fish, aes(x=Length_mm, y=d15N, color=SpeciesCode)) + geom_point(size=1, na.rm = TRUE) +

  scale_colour_manual(values=fish_color) +
  
  
  labs(title = "",
       subtitle = " ",
       #caption =,
       #tag = "Figure 1",
       x = "Length (mm)",
       y = expression(paste(delta^15, "N", " (\u2030)")) ) +
  
  theme_classic() + geom_smooth(method = "lm", se = FALSE)

#length vs 13C for supplement
fish_covarC_plot<-ggplot(Fish, aes(x=Length_mm, y=d13C, color=SpeciesCode)) + geom_point(size=1, na.rm = TRUE) +

  scale_colour_manual(values=fish_color) +
  
  
  labs(title = "",
       subtitle = " ",
       #caption =,
       #tag = "Figure 1",
       x = "Length (mm)",
       y = expression(paste(delta^13, "C", " (\u2030)")) ) +
  
  theme_classic() + geom_smooth(method = "lm", se = FALSE)

fish_covar_plot<- ggarrange(fish_covarN_plot, fish_covarC_plot,  ncol = 2, nrow = 1, common.legend = TRUE)

ggsave("figures/fish_covar_plot.pdf", plot=fish_covar_plot,
      # encoding="MacRoman",
       width = 6, #size that you want
       height = 3.5, #size that you want
       units = "in")



####Probability models for dose and assocated figures

serve_size_oz<-4

prob_dose<- data.frame(length=NA, meal1=NA, meal2=NA, meal3=NA) 

for(j in 18:36){
  
  
  latr_size<-latr[latr$Length_mm < j*25.4,]
  sampmat<-matrix(nrow = 3000, ncol=2)
  sampmat[,1]<- rep (seq(1:3), each=1000)
  
  for( i in 1:3000){
    sampmat[i,2]<- 0.001*sum(sample(latr_size$MeHg_ww, size= sampmat[i,1], replace=T))
    
  }
  
  hg_dose<-data.frame(meals=sampmat[,1], hg=28.3*serve_size_oz*sampmat[,2]) # 4 28 g/oz
  hg_dose$high<- ifelse(hg_dose$hg-46 > 0, 1, 0)
  dose<-tapply(hg_dose$high, INDEX=as.factor(hg_dose$meals), FUN =sum)
  dose_percent<- dose*0.1 #0.1 here conveted 100 samples to a percentage
  
  prob_dose[j,]<-c(j,dose_percent)
  
}

###Now for whitefish
prob_dose_whitefish<- data.frame(length=NA, meal1=NA, meal2=NA, meal3=NA) 




for(j in 16:27){
  
  
  lawf_size<-lawf[lawf$Length_mm < j*25.4,]
  sampmat<-matrix(nrow = 3000, ncol=2)
  sampmat[,1]<- rep (seq(1:3), each=1000)
  
  for( i in 1:3000){
    sampmat[i,2]<- 0.001*sum(sample(lawf_size$MeHg_ww, size= sampmat[i,1], replace=T))
    
  }
  
  hg_dose<-data.frame(meals=sampmat[,1], hg=28.3*serve_size_oz*sampmat[,2]) # 4 28 g/oz
  hg_dose$high<- ifelse(hg_dose$hg-46 > 0, 1, 0)
  dose<-tapply(hg_dose$high, INDEX=as.factor(hg_dose$meals), FUN =sum)
  dose_percent<- dose*0.1 #0.1 here conveted 100 samples to a percentage
  
  prob_dose_whitefish[j,]<-c(j,dose_percent)

}

ggplot(hg_dose, aes(x=as.factor(meals), y=hg)) + geom_violin()+ geom_hline(yintercept=46, color="red", size=2) +  geom_boxplot(width=0.1) +
  theme_classic() +
  annotate("text", x=0.85, y=90, label= as.character(prob_high[1])) +
  annotate("text", x=1.85, y=90, label=  as.character(prob_high[2])) +
  annotate("text", x=2.85, y=90, label=  as.character(prob_high[3]) ) +
  xlab("Number of meals per week")+ ylab("Weekly Hg dose (µg)"  )



pdf("figures/dose_exceedence_plot.pdf", width = 3.5, height=5)

par(mfcol=c(2,1), mai=c(0.62,0.6,0.1,0.1), mgp=c(2,1,0), omi=c(0.03,0.2,0.03,0.03), cex=0.8)

plot(prob_dose$length, prob_dose$meal1, ylim=c(0,100), xlim=c(18,36), ylab="", xlab="Fish length (inches)", pch=16, col="darkgreen", axes=F)
axis(1)
axis(2)
points(prob_dose$length, prob_dose$meal2, pch=16, col="orange")
points(prob_dose$length, prob_dose$meal3, pch=16, col="darkred")
text(24,95, "Lake Trout")

plot(prob_dose_whitefish$length,  prob_dose_whitefish$meal1, ylim=c(0,100), xlim=c(18,36), ylab="", xlab="Fish length (mm)", pch=16, col="dark green", axes=F)
axis(1, at=c(500/25.4, 600/25.4,700/25.4,800/25.4, 900/25.4), lab=c("500", "600", "700", "800", "900"))
axis(2)
points(prob_dose_whitefish$length, prob_dose_whitefish$meal2, pch=16, col="orange")
points(prob_dose_whitefish$length, prob_dose_whitefish$meal3, pch=16, col="darkred")
legend(30, 100, legend=c("1 meal", "2 meals", "3 meals"), pch=16,
       col = c("darkgreen","orange", "darkred"))
text(24,95, "Lake Whitefish")

mtext("% times where FDA Hg dose is exceeded", side = 2, line = -0.5, outer = TRUE, cex=0.8) 

dev.off()