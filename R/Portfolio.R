# Portfolio.R  FEMS dev code by Francis Parr Feb 2022
# Edits/subset of Portfolio.R in main FEMS branch
# Licensing and Copyright notices from there
# **************************************************
# defines: class Portfolio, Portfolio() constructor,
#    add(<portfolio>,<contract_list>), set(<portfolio>,<rfconn>)
#    generateEvents(<portfolio>, <serverURL>,<riskFactorList>)
#    samplePortfolio(<contractDataFilename>)

# *********************************************************************
# class Portfolio
# *************************************
#' class Portfolio
#'
#' A Portfolio consists of a list of contracts. This is one of the required
#' input parameter for a generateEvents(portfolio, scenario) request
#' @include ContractType.R
#' @import methods
#' @importFrom methods new
#' @export Portfolio
#' @exportClass Portfolio
#'
#' @field contracts  List of contracts, class=ContractType, in the portfolio.
setRefClass("Portfolio",
            fields = list(
              contracts = "list"   # contracts are instances of ContractType
            ))

# **************************************
# constructors Portfolio(...) for a portfolio object
# *************************************
#' Portfolio < >  -  generic function definition
#'
#' Defines generic S4 constructor method on class Portfolio
#' @param  contract   S4 reference Class=ContractType, a contract to include.
#' @param  ...        Not used
setGeneric(name = "Portfolio",
           def = function(contract, contractList){
             standardGeneric("Portfolio")
           })
#' Portfolio ( )  - no parameters instance of Portfolio< >
#'
#' Creates an empty Portfolio object with no attributes initialized.
#' @return  S4 reference with class=Portfolio and no attributes initialized.
setMethod(f = "Portfolio", signature = c(),
          definition = function( ){
            return(new("Portfolio"))
          })
#' Portfolio("ContractType")  Constructs Portfolio containing a single contract.
#'
#' This instance of the generic Portfolio< > method takes a reference to a
#' contract as its input parameter and returns a portfolio with this single
#' contract as its contents
#' @param contract  S4 reference class=ContractType
#' @return   S4 reference class=Portfolio, initialized attributes
setMethod(f = "Portfolio",
          signature = c(contract="ContractType", contractList="missing"),
          definition = function (contract) {
            ptf <- Portfolio()
            ptf$contracts <- list(contract)
            return(ptf)
          })
# This  method is really preferred - because if we have a single contract it is
# always easy to insert it into a list and pass that. Up until now there has
# been no constructor for portfolio taking a contractList. We should deprecate
# the single contract constructor (FNP Jan 2024)
setMethod(f = "Portfolio",
          signature = c(contract="missing", contractList = "list"),
          definition = function(contractList) {
            ptf <- Portfolio()
            ptf$contracts <- contractList
            return(ptf)
          })


#' generateEvents < >     Generic method definition
#'
#' Defines a generic method on S4 Class Portfolio. Instances will call out
#' to an ACTUS server at location serverURL to generate cashflow events for
#' contracts in the portfolio using the risk scenario in the portfolio.
#' Instances of this generic are:
#'  signature ( "Portfolio", serverURL, riskFactors, "ContractAnalysis", ... )
#'
#' @param host        S4 reference Class=scenarioAnalysis, financialModel or contractAnalysis object
#' @param ptf         S4 reference Class=Portfolio
#' @param serverURL   character string, the URL of ACTUS server to call out to.
#' @param riskFactors list of S4 Class=ReferenceIndex
#' @param scenarioID  character string, the ID of the scenario (on the risk server) to be used
#' @param simulateToDate character string, the date to which the simulation is to be run in case of a scenarioSimulation
#' @param monitoringTimes list of character strings, the dates to be monitored in case of a scenarioSimulation
#' @return      List of generated cashflow results - one entry per contract
# setGeneric(name = "generateEvents",
#            def = function(host, ptf, serverURL, riskFactors){
#              standardGeneric("generateEvents")
#            })


setGeneric(name = "generateEvents",
           def = function(host, ptf, serverURL, riskFactors, scenarioID, simulateToDate, monitoringTimes){
             standardGeneric("generateEvents")
           })

# ************************************************************************
# generateEvents(<Portfolio>, ServerURL, list<riskFactors>)
#    -- instance of generic method, proto-scenario
# ************************************************************************
#' generateEvents(<portfolio>, <ACTUS-server-URL>, list<riskFactors>)
#'
#'   The generateEvents(Portfolio, character, list) function takes as
#'   input: (1) an initialized S4 Portfolio of ACTUS contracts, (2) an ACTUS
#'   serverURL,  and (3)  a list of riskFactors defining the risk scenario;
#'   the function constructs a JSON representation of the Portfolio contents and
#'   risk factors then calls out using https POST to the Actus server at URL
#'   serverURL to generate a list of cashflow event lists for each contract in
#'   the Portfolio using the provided list of riskFctors as scenario data.
#'
#' @param ptf    Portfolio S4 object initialized with contract and risk factors
#' @param serverURL  character - identifies the ACTUS server to be called
#' @param riskFactors list - list of S4 class riskFactor objects
#' @return       List of generated cashflow results - one entry per contract
#' @export
#' @import    jsonlite
#' @import    httr
#' @examples {
#'   mydatadir <- tempdir()
#'   installSampleData(mydatadir)
#'   cdfn <- file.path(mydatadir, "BondPortfolio.csv")
#'   ptf   <-  samplePortfolio(cdfn)
#'   serverURL <- "https://dadfir3-app.zhaw.ch/"
#'   rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'   rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'   cfls  <- generateEvents(,ptf,serverURL,list(rfx))
#' }
#'
setMethod (f = "generateEvents",
           signature = c(host = "missing", ptf="Portfolio",
                         serverURL="character", riskFactors= "list",
                         scenarioID = "missing", simulateToDate = "missing",
                         monitoringTimes = "missing") ,
           definition = function(ptf,serverURL,riskFactors){
             simulationRsp <- simulationRequest(ptf = ptf, url = serverURL, rfs = riskFactors)
             response_content <- content(simulationRsp)
             if (simulationRsp$status_code == 200) {
               evs <- response_content
             } else{
               evs <- list()
               print( paste0("Contract simulation error. status_code= ",
                             simulationRsp$status_code,
                             "Error info= ", response_content$error)
               )
             }
             #              return("returning from (ptf,severURL,rfs) version ")
             return(evs)
           })

# ************************************************************************
# generateEvents(<Portfolio>, ServerURL, scenarioID)
#    -- instance of generic method, proto-scenario
# ************************************************************************
#' generateEvents(<portfolio>, <ACTUS-server-URL>, scenarioID)
#'
#'   The generateEvents(Portfolio, character, list) function takes as
#'   input: (1) an initialized S4 Portfolio of ACTUS contracts, (2) an ACTUS
#'   serverURL,  and (3)  a scenarioID of a scenario stored on the risk server;
#'   the function constructs a JSON representation of the Portfolio contents and
#'   risk factors then calls out using https POST to the Actus server at URL
#'   serverURL to generate a list of cashflow event lists for each contract in
#'   the Portfolio using the provided the riskFactors stored in the scenario.
#'
#' @param ptf    Portfolio S4 object initialized with contract and risk factors
#' @param serverURL  character - identifies the ACTUS server to be called
#' @param scenarioID list - list of S4 class riskFactor objects
#' @return       List of generated cashflow results - one entry per contract
#' @export
#' @import    jsonlite
#' @import    httr

setMethod (f = "generateEvents",
           signature = c(host = "missing", ptf="Portfolio",
                         serverURL="character", riskFactors = "missing",
                         scenarioID = "character", simulateToDate = "missing",
                         monitoringTimes = "missing"),
           definition = function(ptf,serverURL,scenarioID){
             simulationRsp <- simulationRequest(ptf = ptf, url = serverURL, scenarioID = scenarioID)
             response_content <- content(simulationRsp)
             if (simulationRsp$status_code == 200) {
               evs <- response_content
             } else{
               evs <- list()
               print( paste0("Contract simulation error. status_code= ",
                             simulationRsp$status_code,
                             "Error info= ", response_content$error)
               )
             }
             #              return("returning from (ptf,severURL,rfs) version ")
             return(evs)
           })


# ************************************************************************
# generateEvents(<Portfolio>, ServerURL, scenarioID, simulateToDate, monitoringTimes)
#    -- instance of generic method, proto-scenario
# ************************************************************************
#' generateEvents(<portfolio>, <ACTUS-server-URL>, scenarioID, simulateToDate, monitoringTimes)
#'
#'   The generateEvents(Portfolio, character, list) function takes as
#'   input: (1) an initialized S4 Portfolio of ACTUS contracts, (2) an ACTUS
#'   serverURL, (3)  a scenarioID of a scenario stored on the risk server (4) a
#'   Date to simulate to, and (5) monitoringTimes - a list of dates to be monitored;
#'   the function constructs a JSON representation of the Portfolio contents and
#'   risk factors then calls out using https POST to the Actus server at URL
#'   serverURL to generate a list of cashflow event lists for each contract in
#'   the Portfolio using the provided the riskFactors stored in the scenario.
#'   Additionally, this function is run using pre payment models and the simulation
#'   is then run until the specified simulateToDate. The monitoringTimes are used to
#'   monitor the risk factors at the specified dates.
#'
#' @param ptf    Portfolio S4 object initialized with contract and risk factors
#' @param serverURL  character - identifies the ACTUS server to be called
#' @param scenarioID list - list of S4 class riskFactor objects
#' @param simulateToDate character - the date to which the simulation is to be run
#' @param monitoringTimes list - the dates to be monitored
#' @return       List of generated cashflow results - one entry per contract
#' @export
#' @import    jsonlite
#' @import    httr

setMethod (f = "generateEvents",
           signature = c(host = "missing",ptf="Portfolio",
                         serverURL="character", riskFactors = "missing",
                         scenarioID = "character", simulateToDate = "character",
                         monitoringTimes = "list"),
           definition = function(ptf,serverURL,scenarioID, simulateToDate, monitoringTimes){
             simulationRsp <- simulationRequest(ptf = ptf, url = serverURL,
                                                scenarioID = scenarioID, simulateToDate = simulateToDate,
                                                monitoringTimes = monitoringTimes)
             response_content <- content(simulationRsp)
             if (simulationRsp$status_code == 200) {
               evs <- response_content
             } else{
               evs <- list()
               print( paste0("Contract simulation error. status_code= ",
                             simulationRsp$status_code,
                             "Error info= ", response_content$error)
               )
             }
             #              return("returning from (ptf,severURL,rfs) version ")
             return(evs)
           })

# **********************************
# simulationRequest( portfolio, serverURL, list )
# *********************************
# this internal function ( not exported from FEMSdevPkg 0 does the work of
# generating a a JSON simulation request and sending it to the ACTUS server.
# It returns a simulationRsp message without analysis whether it succeeded or
# failed. Function simulationRequest is used in both generateEvents(ptf,...)
# and generateEvents(cfla), but these methods handle the results and error
# cases in different ways
setGeneric (name = "simulationRequest", def = function(ptf, url, rfs, scenarioID, simulateToDate, monitoringTimes){
  standardGeneric("simulationRequest")
})

setMethod(f = "simulationRequest",
          signature = c(ptf= "Portfolio", url= "character", rfs = "list",
                        scenarioID = "missing", simulateToDate = "missing", monitoringTimes = "missing"),
          definition = function(ptf,url,rfs){
            contractDefs <- lapply(ptf$contracts,preJcontract)
            riskFactors <-  preJSONrfxs(rfs)
            fin_list <- list(contracts = contractDefs,
                             riskFactors = riskFactors)
            # create final request body in json format
            request_body <- toJSON(fin_list, pretty = TRUE, auto_unbox = FALSE)

            # issue POST command to have server generate cashflows
            simulationRsp  <- POST(paste0(url, "eventsBatch"),
                                   body = request_body,
                                   content_type_json())
            return(simulationRsp)
          })


# method for no behavioural models
setMethod(f = "simulationRequest",
          signature = c(ptf= "Portfolio", url= "character", rfs = "missing",
                        scenarioID = "character", simulateToDate = "missing", monitoringTimes = "missing"),
          definition = function(ptf,url,scenarioID){
            contractDefs <- lapply(ptf$contracts,preJcontract)
            scenarioDef <-  list(scenarioID = scenarioID, scenarioType = "scenario")
            fin_list <- list(contracts = contractDefs,
                             scenarioDescriptor = scenarioDef)
            # create final request body in json format
            request_body <- toJSON(fin_list, pretty = TRUE, auto_unbox = TRUE)

            # issue POST command to have server generate cashflows
            simulationRsp  <- POST(paste0(url, "rf2/eventsBatch"),
                                   body = request_body,
                                   content_type_json())
            return(simulationRsp)
          })

# method for scenario simulation (with behavioural models)
setMethod(f = "simulationRequest",
          signature = c(ptf= "Portfolio", url= "character", rfs = "missing",
                        scenarioID = "character", simulateToDate = "character", monitoringTimes = "list"),
          definition = function(ptf,url,scenarioID, simulateToDate, monitoringTimes){
            contractDefs <- lapply(ptf$contracts,preJcontract)
            scenarioDef <-  list(scenarioID = scenarioID, scenarioType = "scenario")
            sim_date <- format(as.Date(simulateToDate), "%Y-%m-%dT%H:%M:%S")
            mon_times <- as.vector(lapply(monitoringTimes, function(x) format(as.Date(x), "%Y-%m-%dT%H:%M:%S")))
            fin_list <- list(contracts = contractDefs,
                             scenarioDescriptor = scenarioDef,
                             simulateTo = sim_date,
                             monitoringTimes = mon_times)
            # create final request body in json format
            request_body <- toJSON(fin_list, pretty = TRUE, auto_unbox = TRUE)

            # issue POST command to have server generate cashflows
            simulationRsp  <- POST(paste0(url, "rf2/scenarioSimulation"),
                                   body = request_body,
                                   content_type_json())
            return(simulationRsp)
          })

# CHANGE THIS FUNCTION TO REMOVE ALL RISK RELATED
# ************************************************************
# samplePortfolio(contractDataFileName)  cdfn  FNP 13 April 2022
# ************************************************************
#' samplePortFolio
#'
#' samplePortfolio ( cdfn) takes as input a contracts-data-filepath
#'     reads this data and returns an initialized Portfolio object with
#'     contracts from the csv file.
#' @param cdfn      character string -  a contract-data-filepath
#'
#' @return   Portfolio s4 object initialized with the data from the input files
#' @export
#' @include import.R
#' @importFrom utils read.csv
#' @examples {
#'   mydatadir <- tempdir()
#'   installSampleData(mydatadir)
#'   cdfn <- file.path(mydatadir, "BondPortfolio.csv")
#'   ptf   <-  samplePortfolio(cdfn)
#'    }
#'
samplePortfolio <- function(cdfn) {
  ptf <- Portfolio()            # create portfolio object no attributes set
                                # read in contract data from named file
                                # install in portfolio object
  ptf$contracts <-   contracts_df2list(contractFile2dataframe(cdfn))
                                # portfolio is now initialized and ready for
                                # cashflow generation
  return(ptf)
}

#' getContractIDs  <ptf>     Generic method definition
#'
#' Defines a generic method on S4 Class Portfolio. Returns a vector with the
#' contractIDs of all the contracts in the portfolio.
#'
#' @param ptf   S4 reference Class=Portfolio Portfolio with a list of contracts.
#' @return      A vector of character string contractIDs
setGeneric(name = "getContractIDs",
           def = function(ptf) standardGeneric("getContractIDs"))

#' getContractIDs
#'
#' getContractIDs(ptf) takes as input an S4 ref to a Class=Portfolio object
#'     containing a list of contracts. It returns a vector of character string
#'     contractID of the contracts in the portfolio.
#'
#' @param ptf    S4 ref to class=Portfolio object with list of contracts
#' @return       Vector of character string contractIDs -  one for each contract
#'               in the portfolio
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf <- samplePortfolio(cdfn)
#'    cids <- getContractIDs(ptf)
#'    }
#'
setMethod (f = "getContractIDs", signature = c("Portfolio") ,
           definition = function(ptf){
             cids <- sapply(ptf$contracts, getCIDfromContract)
             return(cids)
           })


#' getContract <ptf, cid >     Generic method definition
#'
#'   Defines a generic method on S4 Class Portfolio also taking character string
#'   contractID as its second input. Returns an S4 reference to an object of
#'   Class=ContractType if the input contractID matches a contract in the
#'   Portfolio and NULL if it does not.
#'
#' @param ptf   S4 reference Class=Portfolio Portfolio with a list of contracts
#' @param cid   A character string contractID.
#' @return      An S4 Reference to a portfolio Object if cid is matched or NULL
setGeneric(name = "getContract",
           def = function(ptf, cid ) standardGeneric("getContract"))


#' getContract(ptf, cid)
#'
#' getContract(ptf, cid) takes as input an S4 ref to a Class=Portfolio object
#'     containing a list of contracts and a character string contractID. The
#'     returns either an S4 ref to a Class=ContractType contract object whose
#'     contractID the input cid string OR NULL if there is no matching contract
#'     in the portfolio.
#'
#' @param ptf    S4 ref to class=Portfolio object with list of contracts
#' @param cid    character - a contractID string to be matched
#' @return       EITHER an S4 Ref to a class=ContractType object with this input
#'                      cid as its its contractID
#'               OR NULL if no such match exists
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf <- samplePortfolio(cdfn)
#'    cids <- getContractIDs(ptf)
#'    cntr <- getContract(ptf, cids[1])
#'    }
#'
setMethod ( f = "getContract",  signature = c("Portfolio", "character"),
            definition = function(ptf, cid) {
               cl <- ptf$contracts[
                     sapply(ptf$contracts, function(cntr){
                            getCIDfromContract(cntr) == cid
                     }) ]
               if (length(cl) == 1)  cntr_out <- cl[[1]]
               else  cntr_out = NULL
               return(cntr_out)
            } )

# ***********************************
# mergecfls(cfls) : internal function to convert lists of lists of
#   cashflow events from generateEvents(ptf) to a merged dataframe
# appends the contractID into each event row
mergecfls <- function(cfls) {
       dfout <- do.call(rbind, lapply(cfls, function(cfl){
          df1 <- as.data.frame(do.call(rbind,cfl$events))
          df1["contractID"]<- cfl$contractID
         return(df1)
         }))
       for (col in c("type","time","payoff","currency","nominalValue",
                        "nominalRate","nominalAccrued") )
         { dfout[col]<- unlist(dfout[col]) }
       return(dfout)
}


# ************************
#' eventsLoL2DF(cfls)
#'
#'  This functions takes as input (1) a List of Lists generated by the generateEvents
#'  function. It converts the list of lists into a merged dataframe and appends the
#'  contractID to each event row.
#'
#'
#' @param cfls    cashflow events list of lists from generateEvents
#' @return events dataframe
#' @export
eventsLoL2DF <- function(cfls) {
  if(all(unlist(lapply(cfls,
                       function(x){return(x$status)})) == "Success")){
    dfout <- do.call(rbind, lapply(cfls, function(cfl){
      df1 <- as.data.frame(do.call(rbind,cfl$events))
      df1["contractID"]<- cfl$contractID
      return(df1)
    }))
    for (col in c("type","time","payoff","currency","nominalValue",
                  "nominalRate","nominalAccrued") )
    { dfout[col]<- unlist(dfout[col]) }
  } else {
    dfout <- data.frame()
    print("Error in contract simulation during generateEvents()")
  }
  return(dfout)
}

# *************************
# monthlyAndCumulatedValue(indf) : internal function to convert a row-per-event
# dataframe into a row-per-month dataframe with monthly net payoffs and
# month-to-month cumulated value
# payoff and month to month cumulated payoff
# input=dataframe with "payoff", "month",  "Date" columns for event data
#  output= timebucketed dataframe with "Date", "value", "cumValue" for months (with flows)
monthlyAndCumulatedValue <- function(indf){
  dfout <- aggregate(indf$payoff, by=list(indf$month), FUN= sum)
  colnames(dfout) <- c("month","value")
  dfout["cumValue"]<- cumsum(dfout[,"value"])
  dfout["Date"]<- as.Date(paste0(dfout[,"month"],"-01"))
  dfout <- dfout [c("Date","value","cumValue","month")]
  return (dfout)
}

# ************************
#' simulatePortfolio (ptf, serverURL, riskFactors, scenarioName)
#'
#'  This functions takes as input (1) a portfolio of ACTUS contracts (2) a list
#'  of risk factors - the historical and projected future values for interest
#'  rates, etc  (3) a URL identifying an ACTUS server to generate cashflow
#'  events for the contracts and (4) a scenario name. The ACTUS server is
#'  invoked via an http POST with all required data for the cashflow simulation
#'  passes as JSON.
#'
#'  If the cashflow simulation is successful, the returned cashflow events are
#'  merged into a dataframe sorted by time. Income events and Net capital and
#'  interest flows are extracted and  aggregated into monthly time buckets.
#'  Function ggplot is used to generate graphics for Interest Income by Month,
#'  Cumulated Income Month by Month, Liquidity Change By Month, and Cumulative
#'  Liquidity Position. A vector with these four plots is returned.
#'
#'  There may also be riskFactors in the portfolio ptf but they are not used
#'
#' @param ptf    Portfolio of ACTUS contracts to be simulated class=Portfolio
#' @param serverURL    locates ACTUS server to generate cashflow events
#' @param riskFactors  list of riskFactors class RiskFactor - the scenario
#' @param scenarioName character   name for the scenario - used in plot title
#'
#' @return vector of plots: income and liquidity change, monthly and cumulative
#' @export
#' @include RiskFactor.R
#' @include Portfolio.R
#' @include ContractType.R
#' @import  ggplot2
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf <- samplePortfolio(cdfn)
#'    falling_fp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx_falling <- sampleReferenceIndex(falling_fp,"UST5Y_fallingRates",
#'                                    "YC_EA_AAA",100)
#'    serverURL <- "https://dadfir3-app.zhaw.ch/"
#'    plotlist <- simulatePortfolio(ptf, serverURL, list(rfx_falling),
#'                                rfx_falling$riskFactorID )
#'    plotlist[["monthly income"]]
#' }
simulatePortfolio <-function(ptf, serverURL, riskFactors, scenarioName){
  cfls <- generateEvents(ptf=ptf, serverURL=serverURL, riskFactors= riskFactors)
  # merge all cashflow events for the portfolio into one dataframe
  dfall <- eventsLoL2DF(cfls)

  # sort dataframe by date, add Date sortkey
  # add a  month charstring  column (for aggregation by month)
  dfall["Date"]<- as.Date(substr(dfall[,"time"],1,10))
  tsrtall <- dfall[order(dfall$Date),]
  tsrtall["month"] <- substr(tsrtall[,"time"],1,7)

  #  extract income/interestpayment and liquidity/all payoffs subsets
  ipall <- subset(tsrtall, type %in% c("IP","FP","OPS"))
  lqall <- subset(tsrtall, type %in% c("IP","IED","MD"))

  # build aggregated dataframes with row per month replacing row per event
  # columns for monthly sum, and month by month accumulated total
  ipMonthly <- monthlyAndCumulatedValue(ipall)
  lqMonthly <- monthlyAndCumulatedValue(lqall)

  # use ggplot to generate graphic plots for the four cases
  g_ipm <- ggplot(ipMonthly, aes(x=Date,y=value)) + geom_point(colour = "green") +
       labs(title= paste0("Monthly Interest Income ",scenarioName))
  g_ipc <- ggplot(ipMonthly, aes(x=Date,y=cumValue)) + geom_point(colour="blue") +
       labs(title="Cumulative Monthly Interest Income")
  g_lqm <- ggplot(lqMonthly, aes(x=Date,y=value)) + geom_point(colour = "brown") +
       labs(title="Monthly Liquidity", subtitle="net Interest + Capital flows")
  g_lqc <- ggplot(lqMonthly, aes(x=Date,y=cumValue)) + geom_point(colour="red") +
       labs(title="Cumulative Monthly Liquidity",
            subtitle="Net Interest + Capital Flows")

   #  assemble the plots into a named list and return
   plotlist <- list(g_ipm,g_ipc,g_lqm,g_lqc)
   names(plotlist)  <- c("monthly income", "cumulative income",
                          "monthly liquidity change", "accumulated liquidity" )

   return ( plotlist )
}
