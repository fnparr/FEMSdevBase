# Test script for package FEMSdevBase  in git: fnparr/FEMSdevBase
library(FEMSdevBase)
library(ggplot2)
rm(list=ls())

serverURL <- "https://demo.actusfrf.org:8080/"

# 1.0 fixed rate bond with bondvr cashflow and plot
pam1 <- bondvr("2013-12-31", maturity = "5 years", nominal = 1000,
               coupon = 0.02, paymentFreq = "1 year", role = "long",
               rateResetFreq = "Fixed rate")
unlist(pam1$contractTerms)

evs1 <- generateEventSeries(pam1, list(), serverURL)
unlist(list(contractID = evs1$contractID,
            contractType=evs1$contractType,
            statusDate= evs1$statusDate,
            riskFactors = evs1$riskFactors
))
evs1$events_df
cashflowPlot(evs1)

# 2.0  Initialize some interest rate riskfactors for YC_EA_AAA for VR contracts
# 2.1 read the riskdata files from inside of the package
falling_fp <- system.file("extdata","UST5Y_fallingRates.csv",
                          package = "FEMSdevBase")
rising_fp <-  system.file("extdata","UST5Y_risingRates.csv",
                          package = "FEMSdevBase")
steady_fp <-  system.file("extdata","UST5Y_steadyRates.csv",
                          package = "FEMSdevBase")
recovering_fp <-  system.file("extdata","UST5Y_recoveringRates.csv",
                              package = "FEMSdevBase")
# 2.2 create ACTUS ReferenceIndex's
rfx_falling <- sampleReferenceIndex(
                rxdfp = falling_fp,rfID = "UST5Y_fallingRates",
                moc = "YC_EA_AAA",base = 100)
rfx_rising <- sampleReferenceIndex(
                rising_fp,"UST5Y_risingRates", "YC_EA_AAA",100)
rfx_steady <- sampleReferenceIndex(
                steady_fp,"UST5Y_steadyRates", "YC_EA_AAA",100)
rfx_recovering <- sampleReferenceIndex(
                recovering_fp,"UST5Y_steadyRates", "YC_EA_AAA",100)

# 2.3 make scenarios i.e. list with single risk facto for each case
fallingScenario <- list(rfx_falling)
risingScenario <- list(rfx_rising)
steadyScenario <- list(rfx_steady)
recoveringScenario <- list(rfx_recovering)

# 3.0 variable rate PAMs
# 3.1  Now generate a variable rate PAM RRMOC="YC_EA_AAA" by default
pam2 <- bondvr("2020-12-31", maturity = "5 years", nominal = 50000,
               coupon = 0.02, paymentFreq = "3 months", role = "long",
               rateResetFreq = "1 years", rateResetSpread = 0.01 )
unlist(pam2$contractTerms)

evs2 <- generateEventSeries(pam2, list(rfx_falling), serverURL)
evs2$events_df
cashflowPlot(evs2)
# 3.2  Same variable rate PAM but different scenario
evs3<- generateEventSeries(pam2, list(rfx_rising), serverURL)
evs3$events_df
cashflowPlot(evs3)

# 3.3  Variable rate with 6 months payment - date arithmetic issue !
pam21 <- bondvr("2020-12-31", maturity = "5 years", nominal = 50000,
                coupon = 0.02, paymentFreq = "6 months", role = "long",
                rateResetFreq = "1 years", rateResetSpread = 0.01 )
unlist(pam21$contractTerms)
evs21 <- generateEventSeries(pam21, list(rfx_falling), serverURL)
evs21$events_df
cashflowPlot(evs21)

# 3.4  6 month payment frequency Fixed Rate PAM
pam22 <- bondvr("2020-12-31", maturity = "5 years", nominal = 50000,
                coupon = 0.02, paymentFreq = "6 months", role = "long",
                rateResetFreq = "Fixed rate", rateResetSpread = 0.01 )
unlist(pam21$contractTerms)
evs21 <- generateEventSeries(pam21, list(rfx_falling), serverURL)
evs21$events_df
cashflowPlot(evs21)

# 4.1 Testing mortgage( ) implicit fixed rate
ann1 <- mortgage("2020-12-31",maturity ="20 years", nominal= 1000,
                 coupon = 0.08, paymentFreq = "1 year", role= "long")
unlist(ann1$contractTerms)
evs4 <- generateEventSeries(ann1, list(), serverURL)
evs4$events_df
cashflowPlot(evs4)

# 4.2 Testing Mortgage( ) explicit fixed rate
ann2 <- mortgage("2020-12-31",maturity ="20 years", nominal= 1000,
                 coupon = 0.08, paymentFreq = "1 year", role= "long",
                 rateResetFreq = "Fixed rate")
unlist(ann2$contractTerms)
evs5 <- generateEventSeries(ann2, list(), serverURL)
evs5$events_df
cashflowPlot(evs5)

# 4.3 Testing Mortgage( ) variable rate
ann3 <- mortgage("2020-12-31", maturity = "20 years", nominal = 1000,
                 coupon =0.08 , paymentFreq = "1 year", role = "long",
                 rateResetFreq = "1 year", rateResetSpread = 0.03 )
unlist(ann3$contractTerms)
evs6 <- generateEventSeries(ann3, list(rfx_rising), serverURL)
evs6$events_df
cashflowPlot(evs6)

pam3 <- bondvr("2020-12-31", maturity = "5 years", nominal = 10000,
               coupon = 0.03, paymentFreq = "3 months", role = "RPA",
               rateResetFreq="1 years", rateResetSpread= 0.1 )
unlist(pam3$contractTerms)
evs3 <- generateEventSeries(pam3, fallingScenario, serverURL)

# skipping display of scenario data for now  because done with zoo.plot
# rather than ggplot2 and also pulls in xts

# 5.  creation of the events for the bond under different scenarios
evs1 <- generateEventSeries(contract = pam3, risingScenario, serverURL )
evs2 <- generateEventSeries(contract = pam3, fallingScenario, serverURL )
evs3 <- generateEventSeries(contract = pam3, steadyScenario, serverURL )
evs4 <- generateEventSeries(contract = pam3, recoveringScenario, serverURL )

# 5.1 creation of the desired plots  bond with differeng scenarios
cashFlowPlot1 <- cashflowPlot(evs1)
cashFlowPlot2 <- cashflowPlot(evs2)
cashFlowPlot3 <- cashflowPlot(evs3)
cashFlowPlot4 <- cashflowPlot(evs4)

#optional  display dataframe of the events
evs1$events_df
evs2$events_df
evs3$events_df
evs4$events_df

# 6.0  Portfolio test start here
#  6.1 create the sample portfolio of bonds
cdfn1 <- system.file("extdata","BondPortfolio.csv",package = "FEMSdevBase")
rfdfn <- system.file("extdata","RiskFactors.csv",package = "FEMSdevBase")
ptf1   <-  samplePortfolio(cdfn1,rfdfn)

#create eventSeries and plots for the portfolio with selected scenario
plotlist1 <- simulatePortfolio(ptf1, serverURL, fallingScenario,
                               rfx_falling$riskFactorID)
plotlist2 <- simulatePortfolio(ptf1, serverURL, risingScenario,
                               rfx_rising$riskFactorID)
plotlist3 <- simulatePortfolio(ptf1, serverURL, steadyScenario,
                               rfx_rising$riskFactorID)
plotlist4 <- simulatePortfolio(ptf1, serverURL, recoveringScenario,
                               rfx_rising$riskFactorID)

analysisType <- c("monthly income","cumulative income",
                   "monthly liquidity change","accumulated liquidity")

plotlist1[[analysisType[2]]]
plotlist2[[analysisType[[3]]]]

#  6.2 create the sample portfolio of mortgages
cdfn2 <- system.file("extdata","AnnuityPortfolio.csv",package = "FEMSdevBase")
ptf2   <-  samplePortfolio(cdfn2,rfdfn)

#create eventSeries and plots for the portfolio with selected scenario
plotlist4 <- simulatePortfolio(ptf2, serverURL, fallingScenario,
                               rfx_falling$riskFactorID)
plotlist5 <- simulatePortfolio(ptf2, serverURL, risingScenario,
                               rfx_rising$riskFactorID)
plotlist6 <- simulatePortfolio(ptf2, serverURL, steadyScenario,
                               rfx_rising$riskFactorID)
plotlist7 <- simulatePortfolio(ptf2, serverURL, recoveringScenario,
                               rfx_rising$riskFactorID)

plotlist4[[analysisType[2]]]
plotlist5[[analysisType[[3]]]]

# 7.0 Testing of the rate plots ( and plot performance )

steadyRates <-      read.csv(system.file("extdata", "UST5Y_steadyRates.csv",
                                           package = "FEMSdevBase"))
increasingRates <-  read.csv(system.file("extdata","UST5Y_risingRates.csv",
                                         package = "FEMSdevBase"))
decreasingRates <-  read.csv(system.file("extdata","UST5Y_fallingRates.csv",
                                         package = "FEMSdevBase"))
recoveringRates <-  read.csv(system.file("extdata","UST5Y_recoveringRates.csv",
                                         package = "FEMSdevBase"))
monthlySteadyRates <- monthlyAverageRate(steadyRates)
monthlyIncreasingRates <- monthlyAverageRate(increasingRates)
monthlyDecreasingRates <- monthlyAverageRate(decreasingRates)
monthlyRecoveringRates <- monthlyAverageRate(recoveringRates)

 ggplot(monthlySteadyRates, aes(x=Date,y=Rate)) +
        geom_line(colour = "black") +
        labs(title = "Steady rates scenario - US Treasury 5 Year Rates")
 ggplot(monthlyIncreasingRates, aes(x=Date,y=Rate)) +
        geom_line(colour = "black") +
        labs(title ="Increasing rates scenario - US Treasury 5 Year Rates")
 ggplot(monthlyDecreasingRates, aes(x=Date,y=Rate)) +
        geom_line(colour = "black") +
        labs(title = "Decreasng rates scenario - US Treasury 5 Year Rates")
 ggplot(monthlyRecoveringRates, aes(x=Date,y=Rate)) +
        geom_line(colour = "black") +
        labs(title = "Recovering rates scenario - US Treasury 5 Year Rates")

