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

# Theater types based on capacity and ticket cost
# DATA:         Type Name  | Seats  | Adult Cost  | Child Cost
theatersType <- rbind(c("VIP",      60,  20, 15),
                      c("Standard", 120, 20, 15),
                      c("MAX",      180, 20, 15))

# Branch and size of the each branch based on the number of screens have
#  DATA:       Branch Location  | VIP | Standard | MAX
branch <-  rbind(c("Riyadh", 2, 5, 2), 
                 c("Dammam", 1, 3, 1), 
                 c("Jeddah", 1, 4, 1))


seats <-  120 # How many seats does each theater hold
week_days <- rep(0, 7)  # Store totals for each day


runTheaaters <- function(week_days, theaters) {
  # iterate through the week
    for (day_num in 1:length(week_days)) {
      # Keep track of total revenue for the day
      if (week_days[day_num]== "Saturday" | week_days[day_num]=="Sunday"){
      daily_revenue <- 0
      
      # iterate through the amount of screens on a particular day
      for (theater_number in 1:theaters) {
        
        # Calculate  how many adults and children are watching the movie
        visitors_adults <- sample(60:seats, 1)
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
      else{
        for (day_num in 1:week_days[5]) {
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
  }
}



# Make a barchart showing total revenue per day
day_name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
barplot(week_days, names.arg = day_name, horiz = FALSE)

# Make any other chart
pie(week_days, label = day_name, main = "Revenue")

# Which day had the highest revenue? 
cat("The highest revenue is $", max(week_days), "in", day_name[which(week_days == max(week_days))])

