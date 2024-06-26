% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ContractAnalysis.R
\name{events2dfByPeriod,ContractAnalysis,ANY-method}
\alias{events2dfByPeriod,ContractAnalysis,ANY-method}
\title{events2dfByPeriod(<ContractAnalysis>)}
\usage{
\S4method{events2dfByPeriod}{ContractAnalysis,ANY}(host)
}
\arguments{
\item{host}{ContractAnalysis S4 obj with portfolio, actusServer and risk data}
}
\value{
log msg reporting which contracts were successfully simulated
}
\description{
This method reorganizes a list(by contract) of lists of cashflow events
  into a data frame with columns for: contractID, period, and for each 
  ACTUS cashflow event field. The input ContractAnalysis object myst be in 
  the following state: (1) portfolio, and timeline fields must be initialized
  (2) statusDate of the timeline must be the same as statusdate of all 
  contracts in the portfolio (3) generateEvents(cfla) must have been run to 
  populate cfla$cashflowEventsLoL, and (4) the status of each contract 
  simulation must be "Success" . You can check this using: 
 > unlist(lapply(cfla$cashflowEventsLoL,function(x){return(x$status)})) 
 
  If these conditions are met, events2dfByPeriod() will reorganize the data
  in cfla$cashflowEventsLoL as a dataframe with columns: 
     TBD
  and save that as cfla$cashflowEventsByPeriod for use in subsequent analysis
  steps on the ContractAnalysis object. 
  
  A text message is returned reporting on any issues in this processing step.
  
  Processing steps: (0) check valid cfla$cashflowEventsLoL, (1) merge 
  eventsLOL into eventsDF, (2) add periodIndex column,  (3) sort by 
  (contractID, periodIndex), (4) save as cfla$cashFlowEventsByPeriod.
}
\examples{
{
   mydatadir <- "~/mydata"
   installSampleData(mydatadir)
   cdfn  <- "~/mydata/BondPortfolio.csv"
   ptf   <-  samplePortfolio(cdfn)
   ptfsd <- unlist(lapply(ptf$contracts,function(x){return(x$contractTerms["statusDate"])}))
   ptf2015 <- Portfolio(contractList = ptf$contracts[which(ptfsd == "2015-01-01")])
   serverURL <- "https://dadfir3-app.zhaw.ch/"
   rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
   rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
   tl1 <- Timeline("2015-01-01",3,4,8)
   cfla2015 <- initContractAnalysis( analysisID = "cfla001", 
                             analysisDescription = "this_analysis_descr",
                             enterpriseID = "entp001", yieldCurve = YieldCurve(),
                             portfolio =  ptf2015, currency = "USD", 
                             scenario = list(rfx), 
                             actusServerURL = serverURL, 
                             timeline = tl1)
   logMsgs1  <- generateEvents(host = cfla2015)
   logMsgs2  <- events2dfByPeriod(host = cfla2015)
} 
}
