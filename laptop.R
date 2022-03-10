
##########################
# Data Analysis 2 &      #
# Coding-1 Final Project #
#                        #
#       Ersan            #
#     Kucukoglu          #
#                        #
#                        #
##########################

rm(list=ls())

# Libraries
#install.packages("AER")
library(AER)
library(tidyverse)
library(lspline)
library(fixest)
library(modelsummary)
library(ggpubr)
# Extra for correlation heatmap
library(reshape2)
library(kableExtra)

#DATA IMPORT
# Get the data
url <- 'https://raw.githubusercontent.com/ersan-kucukoglu/DA2/main/Final_Term_Project/data/laptop_price.csv'
raw_data <- read_csv(url)

# Look at first 5 rows 
head(raw_data)

# Look at datatype of columns in the table
glimpse(raw_data)

# Get stats of each column
summary(raw_data) 

#DATA CLEANING

#   1) Correct the processor into simple numeric variable
#   2) check with `typeof()`
#   3) convert the variable into a numeric variable
df <- separate( raw_data ,  Cpu , "Intel Core i" , 
                into = c( "Cpu" ,"intelCore_i") )

df <- select( df, -Cpu )

#filter the laptops with the intel processors
df <- df %>% filter(!is.na(`intelCore_i`)) 


df <- separate( df ,intelCore_i, " " , 
                into = c( "intelCore_i","garbage") )
df <- select( df , -garbage )

df$`intelCore_i` <- as.numeric( df$`intelCore_i` )
typeof( df$`intelCore_i` )

#   1) Correct the Ram into simple numeric variable
#   2) check with `typeof()`
#   3) convert the variable into a numeric variable
df <- separate( df ,  Ram , "G" , 
                into = c( "RamGB" ) )
typeof( df$`RamGB` )
df$`RamGB` <- as.numeric( df$`RamGB` )

#drop unnecessary columns which I do not use
df<- select(df,-laptop_ID,-ScreenResolution,-Gpu,-Weight,-Inches)


#TRANSFORMATION

# converting prices in log to normal distribute the right hand side distribution.
#log price
df$lnprice <- log(df$Price_euros)

# Install fastDummies:
#install.packages('fastDummies')
library('fastDummies')
# Create dummy variables for OpSys categorical data
df <- dummy_cols(df, select_columns = 'OpSys')
#rename the variables
df <- rename(df,"OpSys_NoOS"=`OpSys_No OS`, 
             "OpSys_Windows7"=`OpSys_Windows 7`,
             "OpSys_Windows10"=`OpSys_Windows 10`)

# Create dummy variables for TypeName categorical data
df <- dummy_cols(df, select_columns = 'TypeName')

# Create dummy variables for intelCore_i discrete quantiative values
df <- dummy_cols(df, select_columns = 'intelCore_i')

#EXPLOLE THE DATA, CHECK THE DISTRIBUTION OF THE VARIABLES
#  price_euros
p0 <- ggplot( df , aes(x = Price_euros)) +
  geom_histogram( binwidth = 100, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Laptop Prices (Euros)") +
  theme_bw()


# log price_euros
p1 <- ggplot( df , aes(x = log(Price_euros))) +
  geom_histogram( binwidth = 0.1, fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Laptop Prices (Euros)") +
  theme_bw()

association_figs2 <- ggarrange(p0, p1,
                               hjust = -0.6,
                               ncol = 2, nrow = 1)
association_figs2

# intelCore_i
p2 <- ggplot( df , aes(x = factor(intelCore_i))) +
  geom_histogram(stat = "count",fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "IntelCore Processor") +
  theme_bw()

# RamGB
p3 <- ggplot( df , aes(x = factor(RamGB))) +
  geom_histogram(stat = "count",fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Ram (GB)") +
  theme_bw()

#check the most frequent screen size (Inches)
# Inches
p4 <- ggplot( df , aes(x = factor(Inches))) +
  geom_histogram(stat="count",fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Screen Size (inches)") +
  theme_bw()

#check the most frequent manufacturer company
# Company
p5 <- ggplot( df , aes(x = factor(Company))) +
  geom_histogram(stat="count",fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "Company") +
  theme(axis.text.x = element_text(angle=90, hjust=1))
#check the most frequent operating system values which is planned to use as a control variable
# OpSys
p6 <- ggplot( df , aes(x = factor(OpSys))) +
  geom_histogram(stat="count",fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "OpSys") +
  theme(axis.text.x = element_text(angle=90, hjust=1))
#check the most frequent laptop types  which is planned to use as a control variable
# TypeName
p7 <- ggplot( df , aes(x = factor(TypeName))) +
  geom_histogram(stat="count",fill='navyblue', color = 'white' ) +
  labs(y = 'Count',x = "TypeName") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

#drop unnecessary columns, keep the most frequent OpSys and typename dummy variables
df<- select(df,-TypeName_Netbook,-TypeName_Workstation,-`TypeName_2 in 1 Convertible`,-`OpSys_Chrome OS`,-`OpSys_Mac OS X`,-`OpSys_Windows 10 S`)



#CHECK THE DESCRIPTIVES + ASSOCIATION

#Descriptives

P95 <- function(x){quantile(x,0.95,na.rm=T)}
P05 <- function(x){quantile(x,0.05,na.rm=T)}
datasummary((`Price (€)` = Price_euros )+
              (`ln_Price (€)`= lnprice)+
              (`Ram (GB)` = RamGB) + 
              (`i3`=intelCore_i_3)+
              (`i5`=intelCore_i_5)+
              (`i7`=intelCore_i_7)+
              (`OpSys_W10_d`=`OpSys_Windows10`)+
              (`OpSys_W7_d`=OpSys_Windows7)+
              (`OpSys_Linux_d`=OpSys_Linux)+
              (`OpSys_macOS`=OpSys_macOS)+
              (`OpSys_No_OS`=OpSys_NoOS)+
              (`Gaming Type`=TypeName_Gaming)+
              (`Notebook Type`=TypeName_Notebook)+
              (`Ultrobook Type`=TypeName_Ultrabook)~
              Mean + Median + SD + Min + Max + P05 + P95 , 
            data = df ,
            title = 'Descriptive statistics') %>% 
  kable_styling(latex_options = c("HOLD_position","scale_down"))

datasummary_skim( df )
df %>% filter(df$intelCore_i==7)
##Association
numeric_df <- keep( df , is.numeric )
cT <- round( cor( numeric_df , use = "complete.obs") , 2 )
# create a lower triangular matrix
cT[ upper.tri( cT ) ] <- NA
# Put it into a tibble format
melted_cormat <- melt( cT , na.rm = TRUE)
# Now we can create a heat-map
cor_matrix <- ggplot( data = melted_cormat, aes( Var2 , Var1 , fill = value ) )+
  geom_tile( color = "white" ) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_bw()+ 
  theme( axis.text.x = element_text(angle = 90, vjust = 1, 
                                    size = 10, hjust = 1))+
  labs(y="",x="",title = "Corelation Matrix")+
  coord_fixed()

rm( cT , numeric_df , melted_cormat )

####
#Checking scatter-plots to decide the functional form
# Create a general function to check the pattern
chck_sp <- function( x_var , x_lab ){
  ggplot( df , aes(x = x_var, y = lnprice)) +
    geom_point(color='red',size=2,alpha=0.6) +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(x = x_lab, y = "Log of Laptop Prices (euros)") +
    theme_bw()
}

# Our main interest: 
#check the lnprice vs. intelCore_i
sc1 <- chck_sp(df$intelCore_i,'Processor')

#check the lnprice vs. RamGB
sc2 <- chck_sp(df$RamGB,'Ram (GB)')+
  scale_x_continuous(expand=c(0.01,0.01), limits= c(0,64), breaks = c( 4 , 6 , 8 , 12 , 16 , 24,32,64 ))


association_figs1 <- ggarrange(sc1, sc2,
                               hjust = -0.6,
                               ncol = 2, nrow = 1)
association_figs1

#####
#MODELLING

# reg1: NO controls, use log price
reg1 <- feols( lnprice ~ intelCore_i_3 + intelCore_i_5 , data = df , vcov = 'hetero')
reg1


# reg2: control for RamGB
reg2 <- feols( lnprice ~ intelCore_i_3 + intelCore_i_5 + RamGB, data = df , vcov = 'hetero')
reg2

# reg3: reg2 + OpSys dummy variables
reg3 <- feols( lnprice ~ intelCore_i_3 + intelCore_i_5 + RamGB +OpSys_Linux + OpSys_macOS+OpSys_Windows10+OpSys_Windows7, data = df , vcov = 'hetero')
reg3

# reg4: reg3 + TypeName dummy variables
reg4 <- feols( lnprice ~ intelCore_i_3 + intelCore_i_5 + RamGB +OpSys_Linux + OpSys_macOS+ OpSys_NoOS+OpSys_Windows10+OpSys_Windows7+TypeName_Notebook+TypeName_Ultrabook, data = df , vcov = 'hetero')
reg4
summary(reg4)
etable(reg1 , reg2 , reg3 , reg4)

#regression summary table
regression_table <- kable( etable( reg1 , reg2 , reg3 , reg4 ,
                                   title = "Regression Model Summary",
                                   se.below = T,
                                   coefstat = 'se',
                                   fitstat = c('n','r2'),
                                   se.row = F,
                                   depvar = F ) , 
                           "latex", booktabs = TRUE,  position = "H",
                           caption = 'Regression Summary Table') %>% kable_styling(latex_options = c("hold_position","scale_down"))

