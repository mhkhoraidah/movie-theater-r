# Potential Questions to Answer:
# 1. Create snacks that the customers can buy and randomize who buys which snack

#Create snacks data frame to store the sales
snacksDF<-data.frame(Branch=factor(),Theater=integer(), Small.Popcorn=integer(), 
                   Medium.Popcorn=integer(), Large.Popcorn=integer(),
                   Icecream=integer(),Soft.Drink=integer(), Frozen=integer())
snacksDF<-data.frame(Branch=0,Theater=0,Small.Popcorn=0L, 
                   Medium.Popcorn=0L, Large.Popcorn=0L,
                   Icecream=0L,Soft.drink=0L, Frozen=0L)

# the function takes snacks data frame to store the sales on it, Branch name, theater number
# and takes number of visitors.It is return the total revenue of the day for a theater.
snacksF<-function(snacksDF, BranchName, TheaterN, visitorsN){
  #snacks prices
  prices<-c(6, 8, 10, 8, 7, 9)
  #create a vector of sold snacks
  snack<-sample(x = 1:visitorsN, size = 6, replace = TRUE)
  #calculate the sales of snacks for a screen
  snacksSales<- t(snack)%*%prices
  snacksSales<-snacksSales[1]
  #bind it with the branch and theater
  snack<-c(BranchName, TheaterN,snack)
  #print(snack)
  #add it to the dataframe
  snacksDF<-rbind.data.frame(snacksDF,snack)
  #Assign it to golbal var
  snacksDF<<-snacksDF
  return (snacksSales)
}
#snacksF(snacksDF,'R',1,100)
#print(snacksDF)


# 2. Pretend you own multiple theaters and run two simulations to represent each theater and plot the results
# 3. Create conditional statements for movies that may be PG-13 and children are not allowed to watch
# 4. Discount Student
# 5. Create function
# 6. theaters types (VIP, St)
# 7. Random number of seats will start form seats/2:seats
# 8. Scheduling movie function




#'''
# Object: theater
#          -type
#          -seats
#          -adult_cost
#          -child_cost
#'''




# Cost for adults and children
ticket_cost <- 12
ticket_cost_child <- 7 

# Movie List
# DATA:         Movie Name  | Age Rating  | Number of Shows each day
movies <- rbind(c("Limitless", "PG-13", 3), 
                c("Taken", "PG-13", 1), 
                c("The Old Guard", "R", 2),
                c("Mr. & Mrs. Smith", "PG-13", 3),
                c("Focus", "+16", 2),
                c("Raya and the Last Dragon", "PG", 4),
                c("Rush Hour 3", "PG-13", 3))

# Branchs and size of the each branch based on the number of screens have
#  DATA:       Branch Location  | VIP | Standard | MAX
branchs <-  rbind(c("Riyadh", 2, 5, 2), 
                  c("Dammam", 1, 3, 1), 
                  c("Jeddah", 1, 4, 1))
# Theater Types based on capacity and ticket cost
# DATA:         Type Name  | Seats  | Adult Cost  | Child Cost
theatersType <- rbind(c("VIP",      60,  40, 30),
                      c("Standard", 120, 20, 15),
                      c("MAX",      180, 25, 20))

# This function generate a theaters data set for multiple branch by passing branchs and theatersTypes
generateDataSet <- function(branchs, theatersType){
  # Initialize the dataset columns to empty state
  branch_col <- NULL
  theater_num_col <- NULL
  type_col <- NULL
  seats_col <- NULL
  adult_cost_col <- NULL
  child_cost_col <- NULL
  
  # Loop over branchs and theatersType to create dataset
  for(row in 1:nrow(branchs)) {
    
    # Count theater number for each branch in total including all types of theater (VIP, Standard, Max)
    countTheater <- 0
    
    for(col in 2:ncol(branchs)) {
      # total screen for each theater type and covert the string digit to integer number
      screensCount <- as.numeric(branchs[row, col])
      # calculate the total of all screens for each branch
      countTheater <- countTheater + screensCount
      
      # Loading the data for each column
      branch_col     <- c(branch_col, rep(branchs[row], screensCount))
      type_col       <- c(type_col, rep(theatersType[col-1], screensCount))
      seats_col      <- c(seats_col, rep(theatersType[col+2], screensCount))
      adult_cost_col <- c(adult_cost_col, rep(theatersType[col+5], screensCount))
      child_cost_col <- c(child_cost_col, rep(theatersType[col+8], screensCount))
    }
    
    # List of theaters number for all branch
    theater_num_col <- c(theater_num_col, c(1:countTheater))
  }
  
  # create the data frame 
  dataset = data.frame(Branch         = branch_col,
                       Theater_Number = theater_num_col,
                       Type           = type_col,
                       Sates          = seats_col,
                       Adult_Cost     = adult_cost_col,
                       Child_Cost     = child_cost_col)

  return(dataset)
}

# call generateDataSet function inside the View()
View(generateDataSet(branchs, theatersType))

  

week_days <- rep(0, 7)  # Store totals for each day
  

runTheaaters <- function(week_days, theaters) {
  # iterate through the week
  for (day_num in 1:length(week_days)) {
    # Keep track of total revenue for the day
    daily_revenue <- 0
    
    # iterate through the amount of screens on a particular day
    for (theater_number in 1:theaters) {
      
      # Calculate  how many adults and children are watching the movie
      visitors_adults <- sample(1:seats, 1)
      avalibale_seats <- seats - visitors_adults
      visitors_children <- 0
      
      # conditional statements for movies that may be PG-13 and children are not allowed to watch
      if(movies[theater_number,2] == "PG" || movies[theater_number,2] == "PG-13") {
        visitors_children <- sample(1:avalibale_seats,1) # this is should be the the rest of available seats or less
        avalibale_seats <- avalibale_seats - visitors_children
      } 
      cat("\nThe theater #", theater_number, "\n\tMovie : ", movies[theater_number], "(", movies[theater_number, 2] , ")", "\n\tAduilt booked: ", visitors_adults,
          "\n\tChildren booked: ", visitors_children, "\n\tAbalibale seats: ", avalibale_seats, "/", seats ,"\n")
      if(movies[theater_number,2] != "PG" && movies[theater_number,2] != "PG-13") {
        cat("**children are not allowed because movie is ", movies[theater_number,2], "**\n")
      }
      
      
      # Calculate the revenue for adults and children
      movie_revenue <- (visitors_adults * ticket_cost) + (visitors_children * ticket_cost_child)
      
      # Calculate revenue, and add to running total for the day
      week_days[day_num] <- week_days[day_num] + movie_revenue
      
      # ------> Need to add Snacks revenue
      # Calculate snacks revenue
      
    }
    # Save total to the corresponding day
    cat("------------------------------------------\n")
    cat("Daliy total revenue: $", week_days[day_num], "\n")
    cat("------------------------------------------\n")
    
  }
}


    
    # Make a barchart showing total revenue per day
    day_name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    barplot(week_days, names.arg = day_name, horiz = FALSE)

    # Make any other chart
    pie(week_days, label = day_name, main = "Revenue")
    
    # Which day had the highest revenue? 
    cat("The highest revenue is $", max(week_days), "in", day_name[which(week_days == max(week_days))])
