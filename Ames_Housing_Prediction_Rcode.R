# Import libraries----
library(dplyr)
library(ggplot2)
library(tidyverse)
library(Amelia) 
library(lattice)
library(hrbrthemes)
library(corrplot)
library(gginference)
library(visualize)
library(TeachingDemos) #to run z-test in R
library(lessR) # to check collinearity 
library(MASS) #stepwise and backward regression
library(fastDummies)
library(carData)
library(car)
library(GGally)
library(leaps)

#1. Load the Ames housing dataset----
Data <- read.csv("C:/Users/Kirudang/Desktop/AmesHousing.csv",header = TRUE)
str(Data)

#Data cleaning and manipulation
#Check for missing values
par(mar=c(4,4,4,8))
missmap(Data, main = "Missing values in dataset")
sapply(Data,function(x) sum(is.na(x)))
Data <- subset(Data, select = -c(Alley, Pool.QC, Fence, Misc.Feature, Fireplace.Qu))

#Remove duplicated values /  rows
Data<- Data[!duplicated(Data), ]

# Create Age and remove the time series data
Data$Age <- Data$Yr.Sold - Data$Year.Remod.Add
Data <- subset(Data, select = -c(Yr.Sold, Mo.Sold, Year.Remod.Add, Year.Built))

# Location:
location_affect <-  Data %>% 
  group_by(Neighborhood) %>%   
  summarise(Median_Price= median(SalePrice)) %>%
  arrange(desc(Median_Price))     
location_affect

p1 <- ggplot(data = Data, aes(x=Neighborhood, y = SalePrice, colour = Neighborhood)) +
  geom_jitter() + 
  geom_boxplot(alpha=0.6, size= 1) + 
  ylab("Neighborhood") + 
  xlab("Price distribution") + 
  ggtitle("Boxplot of price by neighborhood") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )                                     
p1
#Filter data
Data <- subset(Data, select = c(MS.Zoning,Lot.Frontage,Lot.Area,Overall.Qual, 
Overall.Cond,Total.Bsmt.SF, Gr.Liv.Area, Garage.Area,Age,Full.Bath,Bedroom.AbvGr,
Kitchen.AbvGr, TotRms.AbvGrd, Fireplaces, Misc.Val, SalePrice))

# Dummify data
Data$MS.Zoning <- gsub("[[:punct:]]", " ", Data$MS.Zoning)
Data$MS.Zoning <- gsub('\\s+', '',Data$MS.Zoning)

Data <- fastDummies::dummy_cols(Data, select_columns = "MS.Zoning")

#Pre-final Data
str(Data)

#2. Perform EDA----
                  
#Histogram of lot area
p2 <- ggplot(Data, aes(x= Lot.Frontage)) + 
  geom_histogram(alpha=1,binwidth = 1, aes(fill=MS.Zoning)) +
  ylab("Frequency") + 
  xlab("Lot Frontage") + 
  ggtitle("Hitogram of lot frontage distribution with location zone") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )
p2
# Descriptive analysis
psych::describe(Data$Lot.Frontage)

# Remove outliers
Data <- subset(Data, Lot.Frontage!=313)

# Histogram by location
p3 <- ggplot(Data, aes(x= Lot.Frontage)) + 
  geom_histogram(alpha=1,binwidth = 1, aes(fill=MS.Zoning)) + 
  facet_grid(MS.Zoning~.)
  ylab("Frequency") + 
  xlab("Lot Frontage") + 
  ggtitle("Hitogram of lot frontage distribution by location zone") +
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )
p3

p4 <- ggplot(Data, aes(x= Lot.Frontage)) + 
  geom_histogram(alpha=1,binwidth = 1, aes(fill=MS.Zoning)) + facet_grid(.~MS.Zoning)
  ylab("Frequency") + 
  xlab("Lot Frontage")+ 
  ggtitle("Hitogram of lot frontage distribution by location zone")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )
p4

# boxplot to decide using mean or median
p5 <- ggplot(Data, aes(y= Lot.Frontage,x=MS.Zoning, colour = MS.Zoning)) + 
  geom_jitter()+ 
  geom_boxplot(alpha=0.6, size= 1) + 
  ylab("Lot Frontage Area") + 
  xlab("Location Zone")+ 
  ggtitle("Boxplot of Lot frontage area with location zone")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )
p5

#Calculate the median by group 
location_frontage <-  Data %>% 
  group_by(MS.Zoning) %>%   
  summarise(Median_area= median(Lot.Frontage))%>%
  arrange(desc(Median_area))     
location_frontage

#3. Missing values process----
#Remove 2 rows with NA values
Data <- subset(Data, !(is.na(Data["Total.Bsmt.SF"]) | is.na(Data["Garage.Area"])))
#Replace NA values for lot frontage 
Data$Lot.Frontage[is.na(Data$Lot.Frontage) & Data$MS.Zoning == "Iall"] <- 109
Data$Lot.Frontage[is.na(Data$Lot.Frontage) & Data$MS.Zoning == "Aagr"] <- 102
Data$Lot.Frontage[is.na(Data$Lot.Frontage) & Data$MS.Zoning == "RL"] <- 72
Data$Lot.Frontage[is.na(Data$Lot.Frontage) & Data$MS.Zoning == "FV"] <- 65
Data$Lot.Frontage[is.na(Data$Lot.Frontage) & Data$MS.Zoning == "Call"] <- 60
Data$Lot.Frontage[is.na(Data$Lot.Frontage) & Data$MS.Zoning == "RH"] <- 60
Data$Lot.Frontage[is.na(Data$Lot.Frontage) & Data$MS.Zoning == "RM"] <- 52

#Final check for missing values
sapply(Data,function(x) sum(is.na(x)))

#4. Correlation matrix of the numeric values----
#Subset data for Correlation and Regression
Data_Reg <- subset(Data, select = -MS.Zoning)

Data_Cor <- subset(Data_Reg, select = -c(MS.Zoning_Aagr,MS.Zoning_Call,
    MS.Zoning_Iall,MS.Zoning_FV,MS.Zoning_RH,MS.Zoning_RL,MS.Zoning_RM))
# Correlation
M <- round(cor(Data_Cor),2)
M

# 5. Correlation matrix----
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", 
         order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         diag=FALSE, 
         title = "Correlation Matrix",
         mar = c(1, 1, 1, 1))

#combine Pvalue
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(Data_Cor)
head(p.mat[, 1:5])

corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE, title = "Correlation Matrix with significant level of 0.05",
         mar = c(1, 1, 1, 1))

dev.off()

# 6. Scatter plot----

p6 <-  ggplot(Data, aes(x=Gr.Liv.Area,y=SalePrice)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  ggtitle(label = "Scatter Plot with regression line", 
          subtitle = "Overall Above grade (ground) living area square feet for Price") +
  labs(x= "Above grade (ground) living area square feet", y="Sales Price")
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "blue", 
                                  size = 12, 
                                  face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5)
        )

p6

p7 <-  ggplot(Data, aes(x=Misc.Val,y=SalePrice)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  ggtitle(label = "Scatter Plot with regression line", 
          subtitle = "Value of miscellaneous feature for Price") +
  labs(x= "Value of miscellaneous feature", y="Sales Price")
theme(plot.title = element_text(hjust = 0.5, 
                                color = "blue", 
                                size = 12, 
                                face = "bold"), 
      plot.subtitle = element_text(hjust = 0.5)
      )

p7

p8 <-  ggplot(Data, aes(x=Lot.Frontage,y=SalePrice)) + 
  geom_point() + 
  geom_smooth(method=lm) +
  ggtitle(label = "Scatter Plot with regression line", 
          subtitle = "Frontage area for Price") +
  labs(x= "Frontage area", y="Sales Price")
theme(plot.title = element_text(hjust = 0.5, 
                                color = "blue", 
                                size = 12, 
                                face = "bold"), 
      plot.subtitle = element_text(hjust = 0.5)
      )

p8

# scatter plot matrix
Data %>% ggpairs(columns = c("SalePrice", "Gr.Liv.Area" ,"Misc.Val", "Lot.Frontage"), 
                 upper = list (continuous = wrap("cor", size = 5)))

#7. Fitting the model----
lm.fit <- lm(formula = SalePrice~., 
             data = Data_Reg) 
summary(lm.fit)

#8. Interpret the model----
#9. Plot your regression model----
plot(lm.fit)
par(mfrow = c (2,2))
plot(lm.fit)
dev.off()


# 10. Multi-collinearity ----
# Drop MS.Zoning_RM in the model
Data_Reg1 <- subset(Data_Reg, select = -MS.Zoning_RM)

lm.fit1 <- lm(formula = SalePrice~., 
              data = Data_Reg1) 
summary(lm.fit1)


vif(lm.fit1)

# 11. Outliers----
outlierTest(model = lm.fit1)

#12. Improving by backward selection----
stepAIC(lm.fit1, direction = "backward")

#Backward selection
backward_model <- step(lm.fit1, direction = "backward", trace = FALSE)
summary(backward_model)

lm.fit2 <- lm(formula = SalePrice ~ Lot.Frontage + Lot.Area + Overall.Qual + 
                Overall.Cond + Total.Bsmt.SF + Gr.Liv.Area + Garage.Area + 
                Age + Bedroom.AbvGr + Kitchen.AbvGr + TotRms.AbvGrd + Fireplaces + 
                Misc.Val + MS.Zoning_FV + MS.Zoning_RL, 
              data = Data_Reg1)

# Component Residual Plots for updated model
crPlots(model = lm.fit2)

#Data transformation
lm.fit3 <- lm(formula = SalePrice ~ Lot.Frontage + log(Lot.Area) + Overall.Qual + 
                Overall.Cond + I(Total.Bsmt.SF^1.25) + Gr.Liv.Area + Garage.Area + 
                Age + Bedroom.AbvGr + Kitchen.AbvGr + TotRms.AbvGrd + Fireplaces + 
                log(Misc.Val+1) + MS.Zoning_FV + MS.Zoning_RL,
              data = Data_Reg1)
summary(lm.fit3)

#Transform the outcome
hist(Data_Reg1$SalePrice,main ="Histogram of Sales Price", 
     xlab = " Sales Price", 
     col = "cyan4")
psych::describe(Data_Reg1$SalePrice)

summary(powerTransform(Data_Reg1$SalePrice))

Data_Reg1$SalePrice_New <- Data_Reg1$SalePrice^-0.015
hist(Data_Reg1$SalePrice_New,
     main ="Histogram of Transformed Sales Price", 
     xlab = " Transformed Sales Price", 
     col = "cyan2")
psych::describe(Data_Reg1$SalePrice_New)


lm.fit4 <- lm(formula = SalePrice_New ~ Lot.Frontage + Lot.Area + Overall.Qual + 
                         Overall.Cond + Total.Bsmt.SF + Gr.Liv.Area + Garage.Area + 
                         Age + Bedroom.AbvGr + Kitchen.AbvGr + TotRms.AbvGrd + Fireplaces + 
                         Misc.Val + MS.Zoning_FV + MS.Zoning_RL, 
              data = Data_Reg1)
summary(lm.fit4)


#13. Use the all subsets regression method to identify the "best" model----
leaps <-regsubsets(SalePrice_New ~ Lot.Frontage + Lot.Area + Overall.Qual + 
                     Overall.Cond + Total.Bsmt.SF + Gr.Liv.Area + Garage.Area + 
                     Age + Bedroom.AbvGr + Kitchen.AbvGr + TotRms.AbvGrd + Fireplaces + 
                     Misc.Val + MS.Zoning_FV + MS.Zoning_RL, 
                   data = Data_Reg1, 
                   nbest=10)
plot(leaps, 
     scale = "adjr2", 
     main = "Subset Regression")

#best subset model
lm.fit5 <- lm(formula = SalePrice_New ~Overall.Qual + Overall.Cond + Total.Bsmt.SF + 
                Gr.Liv.Area + Garage.Area + Age + Fireplaces + MS.Zoning_RL, 
              data = Data_Reg1)
summary(lm.fit5)

#14. Compare model----
par(mfrow = c (2,2))
plot(lm.fit)
plot(lm.fit4)
plot(lm.fit5)
dev.off()