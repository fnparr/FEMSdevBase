# RiskService.R  FEMS dev code by Gian-Andrin Tommasini Mar 2025
# Defines functions to make use of the risk service in ACTUS 2.0
# # Please see accompanying distribution file for license.
# **************************************************
# defines:
#    putReferenceIndex(<serverURL>,<riskFactor>)
#    findReferenceIndex(<serverURL>,<riskFactorID>)
#    findAllReferenceIndexes(<serverURL>)
#    deleteReferenceIndex(<serverURL>, <riskFactorID>)
#    putTwoDimensionalPrepaymentModel(<serverURL>,<riskFactor>)
#    findTwoDimensionalPrepaymentModel(<serverURL>,<riskFactorID>)
#    findAllTwoDimensionalPrepaymentModels(<serverURL>)
#    deleteTwoDimensionalPrepaymentModel(<serverURL>, <riskFactorID>)
#    putScenario(<serverURL>,<scenarioID>, <referenceIndexes>, <twoDimensionalPrepaymentModels>)
#    findScenario(<serverURL>,<scenarioID>)
#    findAllScenarios(<serverURL>)
#    deleteScenario(<serverURL>, <scenarioID>)

# *********************************************************************

# Reference Indexes

#' putReferenceIndex(url, riskFactor)
#'
#' The function puts a RiskFactor object of class RiskFactor to the server.
#'
#' @param url A string containing the server URL.
#' @param riskFactor A RiskFactor of class RiskFactor.
#' @return Prints the status of the operation and returns 1 if successful and 0 if not.
#' @include ReferenceIndex.R
#' @import httr
#' @export
#' @examples{
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' url <- "http://localhost:8082"
#' putReferenceIndex(url, rfx1)
#' }
putReferenceIndex <- function(url, riskFactor){
  endpoint <- paste0(url, "/addReferenceIndex")
  dat <- preJSONrfx_rf20(riskFactor)
  body <- jsonlite::toJSON(dat, pretty = TRUE, auto_unbox = FALSE)
  putResponse <- POST(
    endpoint,
    body = body,
    content_type_json()
  )

  if (status_code(putResponse) == 200) {
    print("Request was successful!")
    print(paste("Added Risk Factor ID:", riskFactor$riskFactorID))
    return(1)
  } else {
    print(paste("Error:", status_code(putResponse)))
    print(content(putResponse, "text"))
    return(0)
  }
}


#' findReferenceIndex(url, riskFactorID)
#'
#' The function looks up if a riskFactor is on the server.
#'
#' @param url A string containing the server URL.
#' @param riskFactorID A string containing the RiskFactorID.
#' @return Prints the status of the operation and returns 1 if the riskFactor was found and 0 if not.
#' @import httr
#' @export
#' @examples{
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' url <- "http://localhost:8082"
#' putReferenceIndex(url, rfx1)
#' findReferenceIndex(url, "UST5Y_fallingRates")
#' }
findReferenceIndex <- function(url, riskFactorID){
  endpoint <- paste0(url, "/findReferenceIndex/", riskFactorID)

  findResponse <- GET(
    endpoint
  )

  if (status_code(findResponse) == 200) {
    print("Request was successful!")
    output <- content(findResponse, "parsed")
    if(is.null(output)){
      print("Risk Factor ID not found.")
      return(0)
    }
    else{
      print(paste("Found Risk Factor ID:", riskFactorID))
      return(1)
    }
  } else {
    print(paste("Error:", status_code(findResponse)))
    print(content(findResponse, "text"))
    return(0)
  }
}


#' findAllReferenceIndexes(url)
#'
#' The function looks up all riskFactors on the server.
#'
#' @param url A string containing the server URL.
#' @return Prints the status of the operation and returns the dataframe of riskFactors or an empty
#' dataframe if there are none on the server or the request was not successful.
#' @import httr
#' @export
#' @examples{
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' url <- "http://localhost:8082"
#' putReferenceIndex(url, rfx1)
#' findAllReferenceIndexes(url)
#' }
findAllReferenceIndexes <- function(url){
  endpoint <- paste0(url, "/findAllReferenceIndexes")

  findResponse <- GET(
    endpoint
  )

  if (status_code(findResponse) == 200) {
    print("Request was successful!")
    output <- content(findResponse, "parsed")
    if(length(output) == 0){
      print("No Risk Factors found.")
      return(data.frame())
    }
    else{
      df <- do.call(rbind, lapply(output, function(x) {
        data.frame(
          riskFactorID = x$riskFactorID,
          marketObjectCode = x$marketObjectCode,
          base = x$base
        )
      }))
      print("Found Risk Factors:")
      print(df)
      return(df)
    }
  } else {
    print(paste("Error:", status_code(findResponse)))
    print(content(findResponse, "text"))
    return(data.frame())
  }
}


#' deleteReferenceIndex(url, riskFactorID)
#'
#' The function deletes a riskFactor from the server.
#'
#' @param url A string containing the server URL.
#' @param riskFactorID A string containing the RiskFactorID.
#' @return Prints the status of the operation and returns 1 if successful and 0 if not.
#' @import httr
#' @export
#' @examples{
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' url <- "http://localhost:8082"
#' putReferenceIndex(url, rfx1)
#' deleteReferenceIndex(url, "UST5Y_fallingRates")
#' }
deleteReferenceIndex <- function(url, riskFactorID){
  endpoint <- paste0(url, "/deleteReferenceIndex/", riskFactorID)

  if(findReferenceIndex(url, riskFactorID) == 0){
    print("No deletion was performed.")
  } else {

    deleteResponse <- DELETE(
      endpoint
    )

    if (status_code(deleteResponse) == 200) {
      print("Request was successful!")
      print(paste("Risk Factor ID:", riskFactorID, "was deleted."))
      return(1)
    } else {
      print(paste("Error:", status_code(deleteResponse)))
      print(content(deleteResponse, "text"))
      return(0)
    }
  }
}

# Two Dimensional Prepayment Models


#' putTwoDimensionalPrepaymentModel(url, riskFactorID, referenceRateID, prePaymentEventTimes, dimension1, dimension2, data)
#'
#' The function puts a TwoDimensionalPrepaymentModel on the server.
#'
#' @param url A string containing the server URL.
#' @param riskFactorID A string containing the riskFactorID.
#' @param referenceRateID A string containing the referenceRateID.
#' @param prePaymentEventTimes A list of prepayment event times.
#' @param dimension1 A list of dimension 1 values.
#' @param dimension2 A list of dimension 2 values.
#' @param data A matrix of data points.
#' @return Prints the status of the operation and returns 1 if successful and 0 if not.
#' @import httr
#' @export
#' @examples{
#' url <- "http://localhost:8082"
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' putReferenceIndex(url, rfx1)
#' evTimes <- c("2015-03-01T00:00:00", "2015-09-01T00:00:00", "2016-03-01T00:00:00")
#' d1 <- c(0.03, 0.025, 0.02, 0.015, 0.01, 0.0, -0.05)
#' d2 <- c(0,1,2,3,5,10)
#' data <- rbind(c(0.01, 0.05, 0.1, 0.07, 0.02, 0),
#'               c(0.01, 0.04, 0.8, 0.05, 0.01, 0),
#'               c(0, 0.02, 0.5, 0.03, 0.005, 0),
#'               c(0, 0.01, 0.3, 0.01, 0, 0),
#'               c(0, 0.01, 0.2, 0, 0, 0),
#'               c(0, 0, 0.1, 0, 0, 0),
#'               c(0, 0, 0, 0, 0, 0))
#' putTwoDimensionalPrepaymentModel(url, "ppm01", "Ust_5Yf", evTimes, d1, d2, data)
#' }
putTwoDimensionalPrepaymentModel <- function(url, riskFactorID, referenceRateID, prePaymentEventTimes, dimension1, dimension2, data) {
  endpoint <- paste0(url, "/addTwoDimensionalPrepaymentModel")

  margins <- list(
    list(dimension = 1, values = dimension1),
    list(dimension = 2, values = dimension2)
  )

  data_list <- list(
    riskFactorId = riskFactorID,
    referenceRateId = referenceRateID,
    prepaymentEventTimes = prePaymentEventTimes,
    surface = list(
      interpolationMethod = "linear",
      extrapolationMethod = "constant",
      margins = margins,
      data = data
    )
  )

  # Convert to JSON
  body <- jsonlite::toJSON(data_list, pretty = TRUE, auto_unbox = TRUE)

  postResponse <- POST(
    endpoint,
    body = body,
    content_type_json()
  )

  if (status_code(postResponse) == 200) {
    print("Request was successful!")
    print(paste("Risk Factor ID:", riskFactorID, "was added."))
  } else {
    print(paste("Error:", status_code(postResponse)))
    print(content(postResponse, "text"))
  }
}


#' findTwoDimensionalPrepaymentModel(url, riskFactorID)
#'
#' The function looks up if a riskFactor, in this case a TwoDimensionalPrepaymentModel
#'  is on the server.
#'
#' @param url A string containing the server URL.
#' @param riskFactorID A string containing the RiskFactorID.
#' @return Prints the status of the operation and returns 1 if the riskFactor was found and 0 if not.
#' @import httr
#' @export
#' @examples{
#' url <- "http://localhost:8082"
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' putReferenceIndex(url, rfx1)
#' evTimes <- c("2015-03-01T00:00:00", "2015-09-01T00:00:00", "2016-03-01T00:00:00")
#' d1 <- c(0.03, 0.025, 0.02, 0.015, 0.01, 0.0, -0.05)
#' d2 <- c(0,1,2,3,5,10)
#' data <- rbind(c(0.01, 0.05, 0.1, 0.07, 0.02, 0),
#'               c(0.01, 0.04, 0.8, 0.05, 0.01, 0),
#'               c(0, 0.02, 0.5, 0.03, 0.005, 0),
#'               c(0, 0.01, 0.3, 0.01, 0, 0),
#'               c(0, 0.01, 0.2, 0, 0, 0),
#'               c(0, 0, 0.1, 0, 0, 0),
#'               c(0, 0, 0, 0, 0, 0))
#' putTwoDimensionalPrepaymentModel(url, "ppm01", "Ust_5Yf", evTimes, d1, d2, data)
#' findTwoDimensionalPrepaymentModel(url, "ppm01")
#' }
findTwoDimensionalPrepaymentModel <- function(url, riskFactorID) {
  endpoint <- paste0(url, "/findTwoDimensionalPrepaymentModel/", riskFactorID)

  response <- GET(endpoint)

  if (status_code(response) == 200) {
    print("Request was successful!")
    output <- content(response, "parsed")
    if(is.null(output)){
      print("Two Dimensional Prepayment Model not found.")
      return(0)
    }
    else{
      print(paste("Found Two Dimensional Prepayment Model:", riskFactorID))
      return(1)
    }
  } else {
    print(paste("Error:", status_code(findResponse)))
    print(content(findResponse, "text"))
    return(0)
  }
}


#' findAllTwoDimensionalPrepaymentModels(url)
#'
#' The function looks up all TwoDimensionalPrepaymentModels on the server.
#'
#' @param url A string containing the server URL.
#' @return Prints the status of the operation and returns the dataframe of riskFactors or an empty
#' dataframe if there are none on the server or the request was not successful.
#' @import httr
#' @export
#' @examples{
#' url <- "http://localhost:8082"
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' putReferenceIndex(url, rfx1)
#' evTimes <- c("2015-03-01T00:00:00", "2015-09-01T00:00:00", "2016-03-01T00:00:00")
#' d1 <- c(0.03, 0.025, 0.02, 0.015, 0.01, 0.0, -0.05)
#' d2 <- c(0,1,2,3,5,10)
#' data <- rbind(c(0.01, 0.05, 0.1, 0.07, 0.02, 0),
#'               c(0.01, 0.04, 0.8, 0.05, 0.01, 0),
#'               c(0, 0.02, 0.5, 0.03, 0.005, 0),
#'               c(0, 0.01, 0.3, 0.01, 0, 0),
#'               c(0, 0.01, 0.2, 0, 0, 0),
#'               c(0, 0, 0.1, 0, 0, 0),
#'               c(0, 0, 0, 0, 0, 0))
#' putTwoDimensionalPrepaymentModel(url, "ppm01", "Ust_5Yf", evTimes, d1, d2, data)
#' findAllTwoDimensionalPrepaymentModels(url)
#' }
findAllTwoDimensionalPrepaymentModels <- function(url) {
  endpoint <- paste0(url, "/findAllTwoDimensionalPrepaymentModels")

  response <- GET(endpoint)

  if (status_code(response) == 200) {
    print("Request was successful!")
    output <- content(response, "parsed")
    if(length(output) == 0){
      print("No Risk Factors found.")
      return(data.frame())
    }
    else{
      df <- do.call(rbind, lapply(output, function(x) {
        data.frame(
          riskFactorID = x$riskFactorId,
          referenceRateID = x$referenceRateId
        )
      }))
      print("Found Two Dimensional Prepayment Models:")
      print(df)
      return(df)
    }
  } else {
    print(paste("Error:", status_code(response)))
    print(content(response, "text"))
    return(data.frame())
  }
}


#' deleteTwoDimensionalPrepaymentModel(url, riskFactorID)
#'
#' The function deletes a riskFactor, in this case a TwoDimensionalPrepaymentModel
#'  from the server.
#'
#' @param url A string containing the server URL.
#' @param riskFactorID A string containing the RiskFactorID.
#' @return Prints the status of the operation and returns 1 if successful and 0 if not.
#' @import httr
#' @export
#' @examples{
#' url <- "http://localhost:8082"
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' putReferenceIndex(url, rfx1)
#' evTimes <- c("2015-03-01T00:00:00", "2015-09-01T00:00:00", "2016-03-01T00:00:00")
#' d1 <- c(0.03, 0.025, 0.02, 0.015, 0.01, 0.0, -0.05)
#' d2 <- c(0,1,2,3,5,10)
#' data <- rbind(c(0.01, 0.05, 0.1, 0.07, 0.02, 0),
#'               c(0.01, 0.04, 0.8, 0.05, 0.01, 0),
#'               c(0, 0.02, 0.5, 0.03, 0.005, 0),
#'               c(0, 0.01, 0.3, 0.01, 0, 0),
#'               c(0, 0.01, 0.2, 0, 0, 0),
#'               c(0, 0, 0.1, 0, 0, 0),
#'               c(0, 0, 0, 0, 0, 0))
#' putTwoDimensionalPrepaymentModel(url, "ppm01", "Ust_5Yf", evTimes, d1, d2, data)
#' deleteTwoDimensionalPrepaymentModel(url, "ppm01")
#' }
deleteTwoDimensionalPrepaymentModel <- function(url, riskFactorID) {
  endpoint <- paste0(url, "/deleteTwoDimensionalPrepaymentModel/", riskFactorID)

  response <- DELETE(endpoint)

  if (status_code(response) == 200) {
    print("Request was successful!")
    print(paste("Risk Factor ID:", riskFactorID, "was deleted."))
    return(1)
  } else {
    print(paste("Error:", status_code(response)))
    print(content(response, "text"))
    return(0)
  }
}


# Scenarios


#' putScenario(url, scenarioID, referenceIndexes, prePayments2d)
#'
#' The function puts a Scenario containing ReferenceIndexes and TwoDimensionalPrepaymentModels on the server.
#'
#' @param url A string containing the server URL.
#' @param scenarioID A string containing the scenarioID.
#' @param referenceIndexes A list of strings containing the ReferenceIndexes names.
#' @param prePayments2d A list of strings containing the TwoDimensionalPrepaymentModels names.
#' @return Prints the status of the operation and returns 1 if successful and 0 if not.
#' @import httr
#' @export
#' @examples{
#' url <- "http://localhost:8082"
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' putReferenceIndex(url, rfx1)
#' evTimes <- c("2015-03-01T00:00:00", "2015-09-01T00:00:00", "2016-03-01T00:00:00")
#' d1 <- c(0.03, 0.025, 0.02, 0.015, 0.01, 0.0, -0.05)
#' d2 <- c(0,1,2,3,5,10)
#' data <- rbind(c(0.01, 0.05, 0.1, 0.07, 0.02, 0),
#'               c(0.01, 0.04, 0.8, 0.05, 0.01, 0),
#'               c(0, 0.02, 0.5, 0.03, 0.005, 0),
#'               c(0, 0.01, 0.3, 0.01, 0, 0),
#'               c(0, 0.01, 0.2, 0, 0, 0),
#'               c(0, 0, 0.1, 0, 0, 0),
#'               c(0, 0, 0, 0, 0, 0))
#' putTwoDimensionalPrepaymentModel(url, "ppm01", "Ust_5Yf", evTimes, d1, d2, data)
#' referenceIndexes <- c("UST5Y_fallingRates")
#' prePayments2d <- c("ppm01")
#' putScenario(url, "scn01", referenceIndexes, prePayments2d)
#' }
putScenario <- function(url, scenarioID, referenceIndexes, prePayments2d){
  # check availability of riskFactors
  for (rfx in referenceIndexes){
    if(findReferenceIndex(url, rfx) == 0){
      print(paste("Risk Factor ID:", rfx, "not found."))
      return(0)
    }
  }

  # prepare riskFactors list
  riskFactorDescriptors <- lapply(referenceIndexes, function(index) {
    list(
      riskFactorID = index,
      riskFactorType = "ReferenceIndex"
    )
  })

  if (!is.null(prePayments2d)){
    # check availability of prePayments2d
    for (ppm in prePayments2d){
      if(findTwoDimensionalPrepaymentModel(url, ppm) == 0){
        print(paste("Two Dimensional Prepayment Model ID:", ppm, "not found."))
        return(0)
      }
    }
    # prepare prePayments2d list
    prePayments2dDescriptors <- lapply(prePayments2d, function(index) {
      list(
        riskFactorID = index,
        riskFactorType = "TwoDimensionalPrepaymentModel"
      )
    })
  } else {
    prePayments2dDescriptors <- list()
  }

  data_list <- list(
    scenarioID = scenarioID,
    riskFactorDescriptors = append(riskFactorDescriptors, prePayments2dDescriptors)
  )

  endpoint <- paste0(url, "/addScenario")
  body <- jsonlite::toJSON(data_list, pretty = TRUE, auto_unbox = TRUE)

  postResponse <- POST(
    endpoint,
    body = body,
    content_type_json()
  )

  if (status_code(postResponse) == 200) {
    print("Request was successful!")
    print(paste("Scenario ID:", data_list$scenarioID, "was added."))
    return(1)
  } else {
    print(paste("Error:", status_code(postResponse)))
    print(content(postResponse, "text"))
    return(0)
  }
}


#' findScenario(url, scenarioID)
#'
#' The function looks up if a scenario is on the server.
#'
#' @param url A string containing the server URL.
#' @param scenarioID A string containing the scenarioID.
#' @return Prints the status of the operation and returns 1 if the riskFactor was found and 0 if not.
#' @import httr
#' @export
#' @examples{
#' url <- "http://localhost:8082"
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' putReferenceIndex(url, rfx1)
#' evTimes <- c("2015-03-01T00:00:00", "2015-09-01T00:00:00", "2016-03-01T00:00:00")
#' d1 <- c(0.03, 0.025, 0.02, 0.015, 0.01, 0.0, -0.05)
#' d2 <- c(0,1,2,3,5,10)
#' data <- rbind(c(0.01, 0.05, 0.1, 0.07, 0.02, 0),
#'               c(0.01, 0.04, 0.8, 0.05, 0.01, 0),
#'               c(0, 0.02, 0.5, 0.03, 0.005, 0),
#'               c(0, 0.01, 0.3, 0.01, 0, 0),
#'               c(0, 0.01, 0.2, 0, 0, 0),
#'               c(0, 0, 0.1, 0, 0, 0),
#'               c(0, 0, 0, 0, 0, 0))
#' putTwoDimensionalPrepaymentModel(url, "ppm01", "Ust_5Yf", evTimes, d1, d2, data)
#' referenceIndexes <- c("UST5Y_fallingRates")
#' prePayments2d <- c("ppm01")
#' putScenario(url, "scn01", referenceIndexes, prePayments2d)
#' findScenario(url, "scn01")
#' }
findScenario <- function(url, scenarioID) {
  endpoint <- paste0(url, "/findScenario/", scenarioID)

  response <- GET(endpoint)

  if (status_code(response) == 200) {
    print("Request was successful!")
    output <- content(response, "parsed")
    if(is.null(output)){
      print("Scenario not found.")
      return(0)
    }
    else{
      print(paste("Found Scenario:", scenarioID))
      return(1)
    }
  } else {
    print(paste("Error:", status_code(response)))
    print(content(response, "text"))
    return(0)
  }
}


#' findAllScenarios(url)
#'
#' The function looks up all Scenarios on the server.
#'
#' @param url A string containing the server URL.
#' @return Prints the status of the operation and returns the dataframe of Scenarios or an empty
#' dataframe if there are none on the server or the request was not successful.
#' @import httr
#' @export
#' @examples{
#' url <- "http://localhost:8082"
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' putReferenceIndex(url, rfx1)
#' evTimes <- c("2015-03-01T00:00:00", "2015-09-01T00:00:00", "2016-03-01T00:00:00")
#' d1 <- c(0.03, 0.025, 0.02, 0.015, 0.01, 0.0, -0.05)
#' d2 <- c(0,1,2,3,5,10)
#' data <- rbind(c(0.01, 0.05, 0.1, 0.07, 0.02, 0),
#'               c(0.01, 0.04, 0.8, 0.05, 0.01, 0),
#'               c(0, 0.02, 0.5, 0.03, 0.005, 0),
#'               c(0, 0.01, 0.3, 0.01, 0, 0),
#'               c(0, 0.01, 0.2, 0, 0, 0),
#'               c(0, 0, 0.1, 0, 0, 0),
#'               c(0, 0, 0, 0, 0, 0))
#' putTwoDimensionalPrepaymentModel(url, "ppm01", "Ust_5Yf", evTimes, d1, d2, data)
#' referenceIndexes <- c("UST5Y_fallingRates")
#' prePayments2d <- c("ppm01")
#' putScenario(url, "scn01", referenceIndexes, prePayments2d)
#' findAllScenarios(url)
#' }
findAllScenarios <- function(url) {
  endpoint <- paste0(url, "/findAllScenarios")

  response <- GET(endpoint)

  if (status_code(response) == 200) {
    print("Request was successful!")
    output <- (content(response, "parsed"))
    if(length(output) == 0){
      print("No Scenarios found.")
      return(data.frame())
    }
    else {
      df <- do.call(rbind, lapply(output, function(x) {
        # Extract riskFactorIDs and combine them into a single string
        riskFactors <- sapply(x$riskFactorDescriptors, function(y) y$riskFactorID)
        riskFactorsString <- paste(riskFactors, collapse = ", ")

        # Create a single-row data frame for the scenario
        data.frame(
          scenarioID = x$scenarioID,
          riskFactors = riskFactorsString,
          stringsAsFactors = FALSE
        )
      }))
      print("Found Scenarios:")
      print(df)
      return(df)
    }
  } else {
    print(paste("Error:", status_code(response)))
    print(content(response, "text"))
    return(data.frame())
  }
}


#' deleteScenario(url, scenarioID)
#'
#' The function deletes a Scenario from the server.
#'
#' @param url A string containing the server URL.
#' @param scenarioID A string containing the scenarioID.
#' @return Prints the status of the operation and returns 1 if successful and 0 if not.
#' @import httr
#' @export
#' @examples{
#' url <- "http://localhost:8082"
#' rfx1 <- sampleReferenceIndex("./inst/extdata/UST5Y_fallingRates.csv","UST5Y_fallingRates", "Ust_5Yf",100)
#' putReferenceIndex(url, rfx1)
#' evTimes <- c("2015-03-01T00:00:00", "2015-09-01T00:00:00", "2016-03-01T00:00:00")
#' d1 <- c(0.03, 0.025, 0.02, 0.015, 0.01, 0.0, -0.05)
#' d2 <- c(0,1,2,3,5,10)
#' data <- rbind(c(0.01, 0.05, 0.1, 0.07, 0.02, 0),
#'               c(0.01, 0.04, 0.8, 0.05, 0.01, 0),
#'               c(0, 0.02, 0.5, 0.03, 0.005, 0),
#'               c(0, 0.01, 0.3, 0.01, 0, 0),
#'               c(0, 0.01, 0.2, 0, 0, 0),
#'               c(0, 0, 0.1, 0, 0, 0),
#'               c(0, 0, 0, 0, 0, 0))
#' putTwoDimensionalPrepaymentModel(url, "ppm01", "Ust_5Yf", evTimes, d1, d2, data)
#' referenceIndexes <- c("UST5Y_fallingRates")
#' prePayments2d <- c("ppm01")
#' putScenario(url, "scn01", referenceIndexes, prePayments2d)
#' deleteScenario(url, "scn01")
#' }
deleteScenario <- function(url, scenarioID) {
  endpoint <- paste0(url, "/deleteScenario/", scenarioID)

  response <- DELETE(endpoint)

  if (status_code(response) == 200) {
    print("Request was successful!")
    print(paste("Scenario ID:", scenarioID, "was deleted."))
    return(1)
  } else {
    print(paste("Error:", status_code(response)))
    print(content(response, "text"))
    return(0)
  }
}
