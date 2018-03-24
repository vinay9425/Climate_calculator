# questions.r is a subroutine of indicator.projections.r 
# containing the interactive questions to ask the user
#
# This code was created by Katharine Hayhoe from Texas Tech University
# For questions on the indicator code contact: katharine.hayhoe@ttu.edu
# For questions on the observations contact: ranjini.swaminathan@ttu.edu
# For questions on ARRM downscaling contact: anne.stoner@ttu.edu
# For questions on NEX downscaling contact: bthrasher@gmail.com
#
# March 2017
#

# QUESTION 1
#

read.q0c <- function()
{
 n <- readline(prompt="Please enter the name of the directory containing the code. Make sure to include the final slash at the end:")
 return(as.character(n))
}

read.q0a <- function()
{
 n <- readline(prompt="Please enter the name of the input directory. Make sure to include the final slash at the end:")
 return(as.character(n))
}

read.q0b <- function()
{
 n <- readline(prompt="Please enter the name of the output directory:")
 return(as.character(n))
}


read.q1 <- function()
{ 
  cat("Do you want to generate the data to create a map of India or a time series for an individual weather station?")
  n <- readline(prompt=" Enter 1 for a map, 2 for a weather station: ")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>2)|(n<0))
  {
    return(read.q1())
  }
  
  return(as.integer(n))
}

# QUESTION 1A
#

read.q1a <- function()
{ 
  cat("Do you want to map the results from one climate model, or from an ensemble average of multiple models?")
  cat(" Warning: the ensemble average may take over an hour to calculate, depending on the speed of your computer - don't do it unless you have the time!",sep="\n\n")
  n <- readline(prompt=" Enter 1 for one model, and 2 for all the models:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>2)|(n<0))
  {
    return(read.q1a())
  }
  
  return(as.integer(n))
}

# QUESTION 1B
#

read.q1b <- function()
{ 
  cat("Which weather station do you want to use?",sep="\n\n")
  for (i in 1:length(stations)) {
   cat(paste(i,' - ',stations[i]),sep='')
  }
  n <- readline(prompt=" Enter the number of the weather station: ")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>length(stations))|(n<0))
  {
    return(read.q1b())
  }
  
  return(as.integer(n))
}

# QUESTION 2
#

read.q2 <- function()
{ 
  cat("Which variable to you want to work with?",sep="\n\n")
  for (i in 1:length(vars)) {
   cat(paste(i,' - ',vars[i]),sep="\n\n")
  }
  n <- readline(prompt=" Enter the variable number:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>length(vars))|(n<0))
  {
    return(read.q2())
  }
  
  return(as.integer(n))
}

# QUESTION 3
#

read.q3 <- function()
{
  cat("What type of indicator do you want to calculate?",sep="\n")
  cat("1 - an annual or seasonal average",sep="\n")
  cat("2 - the number of days above or below a threshold value ",sep="\n")
  cat("3 - an extreme or record value for each year",sep="\n")

  n <- readline(prompt=" Enter the indicator type:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>3)|(n<0))
  {
    return(read.q3())
  }

  return(as.integer(n))
}

# QUESTION 4A
#

read.q4a <- function()
{
  cat("In what month does your season begin?",sep="\n\n")
  n <- readline(prompt=" Enter the first month here, using numbers from 1 (Jan) to 12 (Dec):")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>12)|(n<0))
  {
    return(read.q4a())
  }

  return(as.integer(n))
}

# QUESTION 4B
#

read.q4b <- function()
{
  cat("In what month does your season end?",sep="\n\n")
  n <- readline(prompt=" Enter the last month here, using numbers from 1 (Jan) to 12 (Dec):")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>12)|(n<0))
  {
    return(read.q4b())
  }

  return(as.integer(n))
}

# QUESTION 4C
#

read.q4c <- function()
{
  n <- readline(prompt=" Enter the threshold value in degrees C for temperature or mm for precipitation:")
  n <- as.numeric(n)
  if((n>65)|(n<(-50)))
  {
    return(read.q4d())
  }
  return(as.numeric(n))
}

# QUESTION 4D
#

read.q4d <- function()
{
  cat("Would you like to calculate the number of days per year above or below that threshold?",sep="\n\n")
  n <- readline(prompt=" Enter 1 for above, 2 for below:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>2)|(n<0))
  {
    return(read.q4d())
  }

  return(as.integer(n))
}

# QUESTION 4E
#

read.q4e <- function()
{
  cat("For how many days in a row do you want to calculate an extreme? For example, if you want one day, enter 1; a week, enter 7; two weeks, enter 14.",sep="\n\n")
  n <- readline(prompt=" Enter your answer in number of days from 1 to 14:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>14)|(n<0))
  {
    return(read.q4e())
  }

  return(as.integer(n))
}

# QUESTION 4F
#

read.q4f <- function()
{
  if (variable.to.use<4) cat(paste("Do you want the coldest or hottest day(s)?"),sep="\n\n")
  if (variable.to.use==4) cat(paste("Do you want the driest or wettest day(s)?"),sep="\n\n")
  if (variable.to.use<4) n <- readline(prompt=" Enter 1 for coldest, 2 for hottest:")
  if (variable.to.use==4) n <- readline(prompt=" Enter 1 for driest, 2 for wettest:")
  n <- as.integer(n)
  if((!grepl("^[0-9]+$",n))|(n>2)|(n<0))
  {
    return(read.q4f())
  }

  return(as.integer(n))
}

# QUESTION 5
#

read.q5 <- function()
{
  cat("What threshold would you like to use to calculate Cooling Degree-Days? Values typically range from 18 to 25oC.",sep="\n\n")
  n <- readline(prompt=" Enter the temperature at which people turn on their air conditioning, in degrees C:")
  n <- as.numeric(n)
  if((n>35)|(n<15))
  {
    return(read.q5())
  }

  return(as.numeric(n))
}


