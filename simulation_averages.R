## function to generate table of average values in time

simulation_averages <- function(simulation) {

  ## check that input is of the class simulation
  if(!is.simulation(simulation)) {
    print("Error: input must be class simulations")
    return()
  }

  ## create list of potential variable names, initialize matrix to populate with averages
  varnames <- c("diameter", "diameter.growth", "height", "crown_area", "struct_biomass")
  cnames <- c("timestep","crown.class", varnames[varnames %in% names(simulation)])
  avgs <- matrix(1, nrow = 2*((simulation[["max_t"]]/simulation[["timestep"]])+1), ncol = length(cnames))
  colnames(avgs) <- cnames

  avgs[,2] <- c(rep(1, times = (simulation[["max_t"]]/simulation[["timestep"]])+1), rep(2, times = (simulation[["max_t"]]/simulation[["timestep"]])+1))
  avgs[,1] <- rep(seq(1, simulation[["max_t"]]+1, by = simulation[["timestep"]]), times = 2)
  for (i in 3:length(cnames)) {
    for(j in 1:(nrow(avgs)/2)) {
      ## no diameter growth in last timestep
      if(cnames[i]=="diameter.growth" & j == nrow(avgs)/2) {
        avgs[j,i] <- NA
        avgs[j+(nrow(avgs)/2),i] <- NA
      }
      else {
        avgs[j,i] <- mean(simulation[[cnames[i]]][simulation[["crown.class"]][,j]==1,j])

        if (any(simulation[["crown.class"]][,j]==2)) {
          avgs[j+(nrow(avgs)/2),i] <- mean(simulation[[cnames[i]]][simulation[["crown.class"]][,j]==2,j])
        }
        else avgs[j+(nrow(avgs)/2),i] <- NA
      }
    }
  }
  return(avgs)
}
