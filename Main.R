## Main file for running annual plant models including model compiling and output visualizations.


## package calls




#### simulation variables
max_t <- 1000        ## length of simulation in days
dnot <- 0.0001       ## starting diameter
timestep <- 1        ## timestep in day o
cohort_density <- 5 ## initial pop. size in #ind./m^2
plot_area <- 500     ## plot area in m^2

## 2.function definition

## 2.1 general purpose functions
calc.A <- function(allometry, n.crown.class = 2, spp) {

  if(!is.allometry(allometry)) {
    print("Error: allometry must be an allometry object created using the create_allometry function")
    return()
  }

  spp <- as.character(spp)

  out <- data.frame(nrow = length(spp), ncol = n.crown.class+1)
  colnames(out) <- c("spp","A_c", "A_u")[1:(n.crown.class+1)]
  out[, "spp"] <- spp

  ## calculate maximum light level for understory trees
  L_u <- allometry[var == "L_0", spp]*(exp(-(allometry[var == "k", spp])*allometry[var = "l_c", spp]))

  out[, "A_c"] <- 


  if (crown.class == 1) {
    A <- (V/k)*(1 + log((a_f*L_0)/V) - ((a_f*L_0)/V)*exp(-k*LAI.c))
    return(A)
  }
  else if (crown.class == 2) {
    A <- ((a_f*L_u)/V)*exp(-k*LAI.u)
    return(A)
  }
  else {
    print("error: only 2 crown classes currently supported.")
    return()
  }
}

## function to calculate diameter growth
calc.dd <- function(diameter, l, r, A) {
  dd <- 1/((alpha_s*(gamma+1)*(1+c_bg)*(1/alpha_w))+((gamma/diameter)*(l*c_lb + r*c_rb)))*(A-(l*c_l)-(r*c_r))
  return(dd)
}

## 2.2 simulator functions

## demo
system.time({
  x <- simulate_stand(max_t <- 50, timestep = 1, dnot=dnot, plot.area=800, n_start = 200)
})

plot(x)
