library(deSolve)
library(reshape2)
library(ggplot2)

initial_number_susceptible <- 999999
initial_number_infected <- 1 
initial_number_recovered <- 0

force_of_infection <- 0.2
recovery_rate <- 0.1

follow_up_duration <- 60
times <- seq(from = 0, to = follow_up_duration, by = 1)

initial_state_values <- c(S = initial_number_susceptible,
                          I = initial_number_infected,
                          R = initial_number_recovered)

parameters <- c(lambda = force_of_infection, gamma = recovery_rate)

SIR_model <- function(time, state, parameters) { 
  
  with(as.list(c(state, parameters)), {
    
    dS <- (-1) * (lambda) * S
    dI <- (lambda) * (S) - (gamma) * (I)
    dR <- (gamma) * (I)
    
    return(list(c(dS, dI, dR)))
  })
}

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = SIR_model,
                            parms = parameters))

output_long <- melt(as.data.frame(output), id = "time")

ggplot(data = output_long,
       aes(x = time, y = value, group=variable, color=variable)) +
  geom_line() +
  xlab("Time (days)") +
  ylab("Number of people") + 
  ggtitle("SIR Model")

