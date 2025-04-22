# *************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
# Edits/updates for FEMSdev by Francis Parr April 2022
#   fnparr@gmail.com
# *************************************************************

# *******************************************
# S4 class EventSeries holds a dataframe with the cashflow events generated for
# a single Actus Contract, along with the contractID, contractType, statusDate
# contructors: EventSeries()  EventSeries(<Contract>,<rfs>) 2B exported
# ******************************************
setRefClass("EventSeries",
            fields = list(
              events_df = "data.frame",
              contractID = "character",
              contractType = "character",  # short form e.g. 'PAM'
              statusDate = "character",    # text yyyy-mm-dd
              riskFactors = "list",
              scenarioID = "character",
              simulateToDate = "character", # text yyyy-mm-dd
              monitoringTimes = "list"     # list of text yyyy-mm-dd
            )
)
# *********************
#  constructors:  EventSeries() : (), (<contractType>, <serverURL>, <rf_list>, <scenarioID>, <simulateToDate>, <monitoringTimes>)
setGeneric(name = "EventSeries",
           def = function(contract, serverURL, riskFactors, scenarioID,
                          simulateToDate, monitoringTimes){
             standardGeneric("EventSeries")
           })

setMethod(f = "EventSeries", signature = c(),
          definition = function(){
            return(new("EventSeries"))
          })

# riskFactor list
setMethod(f = "EventSeries",
          signature = c(contract = "ContractType", serverURL = "character",
                        riskFactors = "list", scenarioID = "missing",
                        simulateToDate = "missing", monitoringTimes = "missing"),
          definition = function(contract, serverURL, riskFactors){
            ptf <- Portfolio()
            ptf$contracts <- list(contract)  # singleContractPortfolio
            # Run the cashflow generation on this portfolio
            cshfl_rslt1 <- generateEvents(ptf = ptf,serverURL = serverURL, riskFactors = riskFactors)
            #first cashflow from single contract ptf
            events_df <- eventsLoL2DF(cshfl_rslt1)
            # build the output EventSeries object
            evs <- EventSeries()
            evs$contractID <- contract$contractTerms$contractID
            evs$contractType <- contract$contractTerms$contractType
            evs$statusDate <-  contract$contractTerms$statusDate
            evs$riskFactors <- riskFactors

            events_df$time <- sapply(events_df$time,
                                     function(t){substr(t,1,10)}) # format dates
            evs$events_df <- events_df
            return(evs)
          })

# scenarioID
setMethod(f = "EventSeries",
          signature = c(contract = "ContractType", serverURL = "character",
                        riskFactors = "missing", scenarioID = "character",
                        simulateToDate = "missing", monitoringTimes = "missing"),
          definition = function(contract, serverURL, scenarioID){
            ptf <- Portfolio()
            ptf$contracts <- list(contract)  # singleContractPortfolio
            # Run the cashflow generation on this portfolio
            cshfl_rslt1 <- generateEvents(ptf = ptf,serverURL = serverURL, scenarioID = scenarioID)
            #first cashflow from single contract ptf
            events_df <- eventsLoL2DF(cshfl_rslt1)
            # build the output EventSeries object
            evs <- EventSeries()
            evs$contractID <- contract$contractTerms$contractID
            evs$contractType <- contract$contractTerms$contractType
            evs$statusDate <-  contract$contractTerms$statusDate
            evs$scenarioID <- scenarioID

            events_df$time <- sapply(events_df$time,
                                     function(t){substr(t,1,10)}) # format dates
            evs$events_df <- events_df
            return(evs)
          })

# scenarioID, simulateToDate, monitoringTimes
setMethod(f = "EventSeries",
          signature = c(contract = "ContractType", serverURL = "character",
                        riskFactors = "missing", scenarioID = "character",
                        simulateToDate = "character", monitoringTimes = "list"),
          definition = function(contract, serverURL, scenarioID, simulateToDate, monitoringTimes){
            ptf <- Portfolio()
            ptf$contracts <- list(contract)  # singleContractPortfolio
            # Run the cashflow generation on this portfolio
            cshfl_rslt1 <- generateEvents(ptf = ptf, serverURL = serverURL, scenarioID = scenarioID,
                                          simulateToDate = simulateToDate, monitoringTimes = monitoringTimes)
            #first cashflow from single contract ptf
            events_df <- eventsLoL2DF(cshfl_rslt1)
            # build the output EventSeries object
            evs <- EventSeries()
            evs$contractID <- contract$contractTerms$contractID
            evs$contractType <- contract$contractTerms$contractType
            evs$statusDate <-  contract$contractTerms$statusDate
            evs$scenarioID <- scenarioID
            evs$simulateToDate <- simulateToDate
            evs$monitoringTimes <- monitoringTimes

            events_df$time <- sapply(events_df$time,
                                     function(t){substr(t,1,10)}) # format dates
            evs$events_df <- events_df
            return(evs)
          })


#' generateEventSeries      Generic method definition
#'
#' Defines a generic method on S4 Class Eventseries. The instance is
#' generateEventSeries < contract riskFactors serverURL >
#'
#' @param  contract     the contract to simulate cashflows for
#' @param  serverURL    locate the ACTUS server performing the cashflow analysis
#' @param  riskFactors  list of riskFactors - scenario for contract simulation
#' @param  scenarioID   character   scenarioID for the risk factor list (on the risk server)
#' @param  simulateToDate character string, the date to which the simulation is to be run in case of a scenarioSimulation
#' @param  monitoringTimes list of character strings, the dates to be monitored in case of a scenarioSimulation
#' @return              an EventSeries with cashflow events for the contract
#' @import  methods
#' @include RiskFactor.R
#' @include Portfolio.R
#' @include ContractType.R
setGeneric(name = "generateEventSeries",
           def = function(contract, serverURL, riskFactors, scenarioID, simulateToDate, monitoringTimes){
             standardGeneric("generateEventSeries")
           })

#' generateEventSeries        <contract>, <ACTUS-server-URL>, <risk-factor-list>
#'
#' exported function to simulate a contract cashflows and create an
#'   EventSeries using  "ContractType", "list", "character" method instance.
#'   constructs an EventSeries instance including as its events_df attribute a
#'   dataframe of cashflow events for the input ACTUS contract. This cashflow
#'   is generated with a callout to the ACTUS server located at ACTUS-server-URL
#'   using a risk scenario specified as the list of risk factors. THe method
#'   works by first creating a Portfolio with this ine contract and the supplied
#'   risk factor list, then calling generateEvents on this portfolio.
#'
#' @param  contract    S4 ref      class= ContractType
#' @param  serverURL   character   URL of ACTUS server to simulate the contract
#' @param  riskFactors list        list of S4 ref Class=RiskFactor
#' @return              S4 ref     class=EventSeries
#' @examples{
#'   pam1 <- bondvr("2013-12-31", maturity = "5 years", nominal = 50000,
#'                coupon = 0.02, paymentFreq = "1 years", role = "long")
#'   serverURL <- "https://demo.actusfrf.org:8080/"
#'   evs1 <-generateEventSeries(pam1, serverURL, list())
#'  }
#' @export
#'
setMethod(f = "generateEventSeries",
          signature = c(contract = "ContractType", serverURL = "character",
                        riskFactors = "list", scenarioID = "missing",
                        simulateToDate = "missing", monitoringTimes = "missing"),
          definition = function(contract, serverURL, riskFactors){
              evs <- EventSeries(contract = contract, serverURL = serverURL, riskFactors = riskFactors)
              return(evs)
          })



#' generateEventSeries        <contract>, <ACTUS-server-URL>, <scenarioID>
#'
#' exported function to simulate a contract cashflows and create an
#'   EventSeries using  "ContractType", "character", "character" method instance.
#'   constructs an EventSeries instance including as its events_df attribute a
#'   dataframe of cashflow events for the input ACTUS contract. This cashflow
#'   is generated with a callout to the ACTUS server located at ACTUS-server-URL
#'   using a risk scenario specified on the actus-riskservice and calling it by
#'   its scenarioID. The method works by first creating a Portfolio, then calling
#'   generateEvents on this portfolio
#'
#' @param  contract    S4 ref      class= ContractType
#' @param  serverURL   character   URL of ACTUS server to simulate the contract
#' @param  scenarioID  character   scenarioID for the risk factor list (on the risk server)
#' @return              S4 ref     class=EventSeries
#' @examples{
#'  }
#' @export
#'
setMethod(f = "generateEventSeries",
          signature = c(contract = "ContractType", serverURL = "character",
                        riskFactors = "missing", scenarioID = "character",
                        simulateToDate = "missing", monitoringTimes = "missing"),
          definition = function(contract, serverURL, scenarioID){
            evs <- EventSeries(contract = contract, serverURL = serverURL, scenarioID = scenarioID)
            return(evs)
          })

#' generateEventSeries        <contract>, <ACTUS-server-URL>, <scenarioID>, <simulateToDate>, <monitoringTimes>
#'
#' exported function to simulate a contract cashflows and create an
#'   EventSeries using  "ContractType", "character", "character", "character", "list"
#'   method instance.
#'   constructs an EventSeries instance including as its events_df attribute a
#'   dataframe of cashflow events for the input ACTUS contract. This cashflow
#'   is generated with a callout to the ACTUS server located at ACTUS-server-URL
#'   using a risk scenario specified on the actus-riskservice and calling it by
#'   its scenarioID. Here a behavioral models can be used in the simulation. It will
#'   then be simulated until simulateToDate and at monitoringTimes, events of the
#'   current state of the simulation can be found. The method works by first
#'   creating a Portfolio, then calling generateEvents on this portfolio.
#'
#' @param  contract    S4 ref      class= ContractType
#' @param  serverURL   character   URL of ACTUS server to simulate the contract
#' @param  scenarioID  character   scenarioID for the risk factor list (on the risk server)
#' @param  simulateToDate character string, the date to which the simulation is to be run
#' @param  monitoringTimes list of date character strings
#' @return              S4 ref     class=EventSeries
#' @examples{
#'  }
#' @export
#'
setMethod(f = "generateEventSeries",
          signature = c(contract = "ContractType", serverURL = "character",
                        riskFactors = "missing", scenarioID = "character",
                        simulateToDate = "character", monitoringTimes = "list"),
          definition = function(contract, serverURL, scenarioID, simulateToDate, monitoringTimes){
            evs <- EventSeries(contract = contract, serverURL = serverURL, scenarioID = scenarioID,
                               simulateToDate = simulateToDate, monitoringTimes = monitoringTimes)
            return(evs)
          })
