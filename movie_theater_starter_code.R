
################### Libraries ###############
#install.packages(RCurl)
#library(RCurl)


# URL<-'https://www.dropbox.com/s/wlpn9lf5dz1d7dj/Theaters.csv?dl=0'
# url_content <- getURL(URL)
# Theaters<-read.csv('https://www.dropbox.com/s/wlpn9lf5dz1d7dj/Theaters.csv?dl=0',...)

Theaters<-read.csv('Data governance bootcamp\\R\\Week 6\\day5\\movie-theater-r\\Theaters.csv')
#print(Theaters)



# 1. Create snacks that the customers can buy and randomize who buys which snack

###################################### Snacks ##########################################

#Create snacks dataframe to store the sales
snacksDF<-data.frame(Branch=factor(),Theater=integer(), Small.Popcorn=integer(), 
                     Medium.Popcorn=integer(), Large.Popcorn=integer(),
                     Icecream=integer(),Soft.Drink=integer(), Frozen=integer())
snacksDF<-data.frame(Branch=0,Theater=0,Small.Popcorn=0L, 
                     Medium.Popcorn=0L, Large.Popcorn=0L,
                     Icecream=0L,Soft.drink=0L, Frozen=0L)

################################## Snacks Function #####################################

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
#snacksF(snacksDF,'S',1,200)
#print(snacksDF)

###############################################################################################

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


##################################### Theaters Function ######################################
runTheaters<-function(theatersDF, moviesDF){

  #Get branches names
  uniqueBranch <- unique(theatersDF[c('Branch')])
  uniqueBranch<-uniqueBranch[,1]
  branchesRevenue<-data.frame(uniqueBranch,stringsAsFactors = TRUE)
  
  #create revenues dataset
  for (i in 1:7){
    d <- rep(0, length(uniqueBranch))  
    branchesRevenue<- cbind(branchesRevenue,d)
  }
  
  print((branchesRevenue))
  #iterate through theaters
  for (i in 1:nrow(theatersDF)){

    # iterate through the week
    for (day_num in 1:length(week_days)) {
      # Keep track of total revenue for the day
      daily_revenue <- 0
      
      #get theater info
      seats<-as.numeric(theatersDF[i, ]['seats'])
      branchName<-as.character(theatersDF[i, ]['Branch'])
      theaterN<-as.numeric(theatersDF[i, ]['Theater'])
      AdultCost<-as.numeric(theatersDF[i, ]['Adult.Cost'])
      ChildCost<-as.numeric(theatersDF[i, ]['Child.Cost'])
      
        # Calculate  how many adults and children are watching the movie
        visitors_adults <- sample(1:seats, 1)
        avalibale_seats <- seats - visitors_adults
        visitors_children <- 0
        
        # conditional statements for movies that may be PG-13 and children are not allowed to watch
        if(movies[theater_number,2] == "PG" || movies[theater_number,2] == "PG-13") {
          visitors_children <- sample(1:avalibale_seats,1) # this is should be the the rest of available seats or less
          avalibale_seats <- avalibale_seats - visitors_children
        } 
        # cat("\nThe theater #", theater_number, "\n\tMovie : ", movies[theater_number], "(", movies[theater_number, 2] , ")", "\n\tAduilt booked: ", visitors_adults,
        #     "\n\tChildren booked: ", visitors_children, "\n\tAbalibale seats: ", avalibale_seats, "/", seats ,"\n")
        # if(movies[theater_number,2] != "PG" && movies[theater_number,2] != "PG-13") {
        #   cat("**children are not allowed because movie is ", movies[theater_number,2], "**\n")
        # }
        
        
        # Calculate the revenue for adults and children
        movie_revenue <- (visitors_adults * AdultCost) + (visitors_children * ChildCost)
        
        # Calculate snacks revenue
        snakcsRevenue<-snacksF(snacksDF, branchName, theaterN, seats)
        
        # Calculate revenue, and add to running total for the day
        x<-as.numeric(which(branchesRevenue$uniqueBranch == branchName))
        print(x)
        branchesRevenue[x,day_num+1] <- 
          branchesRevenue[x ,day_num+1] 
        + movie_revenue + snakcsRevenue
        branchesRevenue[1,3]<-branchesRevenue[1,3]+40
        #print(branchesRevenue[uniqueBranch== branchName,day_num+1])
        # ------> Need to add Snacks revenue
        # Calculate snacks revenue
        
      
      # # Save total to the corresponding day
      # cat("------------------------------------------\n")
      # cat("Daliy total revenue: $", week_days[day_num], "\n")
      # cat("------------------------------------------\n")
      
    }
    
  }
  
  print((branchesRevenue))
}

# Cost for adults and children
#ticket_cost <- 12
#ticket_cost_child <- 7 

# List 7 of your favorite movies
movies <- rbind(c("Limitless", "PG-13", 3), 
                c("Taken", "PG-13", 1), 
                c("The Old Guard", "R", 2),
                c("Mr. & Mrs. Smith", "PG-13", 3),
                c("Focus", "+16", 2),
                c("Raya and the Last Dragon", "PG", 4),
                c("Rush Hour 3", "PG-13", 3))

show_time = c("13:30", "18:00", "22:40")

runTheaters(Theaters, movies)

theaters <-  6 # How many screens does the theater have? (assume 1 per movie)
seats <-  120 # How many seats does each theater hold

    
    # Make a barchart showing total revenue per day
    day_name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    barplot(week_days, names.arg = day_name, horiz = FALSE)

    # Make any other chart
    pie(week_days, label = day_name, main = "Revenue")
    
    # Which day had the highest revenue? 
    cat("The highest revenue is $", max(week_days), "in", day_name[which(week_days == max(week_days))])
