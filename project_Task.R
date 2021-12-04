


new.cost <- function (Salaries)
{
  
  # each employee has insurance cost 3000 
  
  Insurance = 3000
  
  result <- Salaries + Insurance
  
  
}

#cost for Rent Location , Furnishing Cost , Licenses to open the Cinema and Electricity costs  

Rent = 80000
Furnishing_Cost = 100000
Licenses = 10000
Electricity = 4000
Equipments = 50000

#The Cinema has 10 employees and the salary for each employee cost 5000 

Employee1 = new.cost(5000)
Employee2 = new.cost(5000)
Employee3 = new.cost(5000)
Employee4 = new.cost(5000)
Employee5 = new.cost(5000)
Employee6 = new.cost(5000)
Employee7 = new.cost(5000)
Employee8 = new.cost(5000)
Employee9 = new.cost(5000)
Employee10 = new.cost(5000)

Total_Salaries_Cost= Employee1 + Employee2 + Employee3+ Employee4 + Employee5 + Employee6 + Employee7 + Employee8 + Employee9 + Employee10  

Total_Cost = Total_Salaries_Cost + Rent + Furniture_Cost + Licenses + Electricity + Equipments 

print ('Total Cost is')

print(Total_Cost)
