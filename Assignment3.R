outcomeDataFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
validateOutcomeAndGetString <- function(outcome) {
  if (outcome == "heart attack") {
    column.name.addition <- "Heart.Attack"
  } else if (outcome == "heart failure") {
  column.name.addition <- "Heart.Failure"
  } else if (outcome == "pneumonia") {
    column.name.addition <- "Pneumonia"
  } else {
    stop("invalid outcome")
  } 
  column.name.addition
}
  
  validateState <- function (state) {
    if(!(state %in% distinct(outcomeDataFrame["State"])[["State"]])) {
      stop("invalid state")
    }
  }
  best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    column.name <- "Hospital.30.Day.Death..Mortality..Rates.from."
    validateState(state)
    column.name.addition <- validateOutcomeAndGetString(outcome)
    column.name <- paste(column.name, column.name.addition, sep =  "")
    #print(column.name)
    minimum.value <- min(as.numeric(filter(outcomeDataFrame, State == state)
                                    [[column.name]]), na.rm = TRUE)
    #print(minimum.value)
    min(filter(outcomeDataFrame, State == state, 
               as.numeric(UQ(as.name(column.name))) == minimum.value) [["Hospital.Name"]])
  }
  
  rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    column.name <- "Hospital.30.Day.Death..Mortality..Rates.from."
    validateState(state)
    column.name.addition <- validateOutcomeAndGetString(outcome)
    column.name <- paste(column.name, column.name.addition, sep =  "")
    #print(column.name)
    #arrange(transmute(filter(outcomeDataFrame,  State == state), Hospital.Name,
    #       value = as.numeric(UQ(as.name(column.name))), od = rank(value)),  od)#[num, "Hospital.Name"]

    df <- filter(outcomeDataFrame, State == state) %>% 
      transmute (Hospital.Name, value = as.numeric(UQ(as.name(column.name))), rank.value = rank(value)) %>%
      arrange(rank.value, Hospital.Name) 
    df <- df[complete.cases(df[["value"]]),]
    nrow.df <- nrow(df)
    if (num == "best") {
      num <- 1
    } else if (num == "worst") {
      num <- nrow.df
    } else if (num > nrow.df) {
      return(NA)
    }
    df[num, "Hospital.Name"]
  }
  
  rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    state.vector =  sort(distinct(outcomeDataFrame["State"])[["State"]])
    data.frame("hospital" = sapply(state.vector, rankhospital, outcome, num), "state" = state.vector)
  }
  
  
