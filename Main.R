## Main file for running annual plant models including model compiling and output visualizations.


## package calls


## ********* model for single tree *********

## variable definition

#### allometric variables
l_c <- 4               ## leaf area index of tree canopy tree
l_u <- 1               ## leaf are index of understory tree
r_c <- 6.5
r_u <- 2
r <- 6.5             ## fine-root surface area per unit crown area
H <- 3.6             ## allometric constant for height
alpha_s <- 0.0815        ## allometric constant for sapwood
alpha_w <- 0.20          ## allometric constant for crown area
gamma <- 1.5         ## allometric exponent

#### carbon accumulation and allocation variables
c_lb <- 0.07656      ## cost of building leaf biomass per LAI
c_rb <- 0.02933      ## cost of builing root biomass per LAI
c_bg <- 0.2          ## building cost of structural biomass
tau_l <- 1           ## average lifetime of a unit carbon in leaf
tau_r <- 2           ## average lifetime of a unit carbon in roots
p_r <- 0.02933       ## respiration rate of roots per unit surface area
p_l <- 0.0638        ## respiration rate of leaves per unit surface area
p_sw <- 0.0466       ## respiration rate of sapwood
c_l <- (c_lb/tau_l) + p_l + p_sw ## cost of building and maintaining leaf biomass per unit LAI
c_r <- c_rb/tau_r + p_r       ## cost of building and maintaining root biomass per unit RAI
V <- 0.6             ## maximum rate of carbon fixation - kgC/m^2/day
k <- 0.33            ## light extinction coefficient
L_0 <- 1200          ## light level above highest canopy
a_f <- 0.001         ## conversion rate from photons to carbohydrates

#### simulation variables
max_t <- 1000        ## length of simulation in days
dnot <- 0.0001       ## starting diameter
timestep <- 1        ## timestep in day o
cohort_density <- 20 ## initial pop. size in #ind./m^2
plot_area <- 500     ## plot area in m^2

## 2.function definition

## 2.1 general purpose functions

## function to generate plots of simulated data:
plot.simulations <- function(df) {
  par(mfrow=c(4,2))
  plot(df$diameter,df$dd, pch=".", main="diameter vs diameter growth rate")
  plot(df$timestep,df$dd, pch=".", main="day vs. diameter growth rate")
  plot(df$timestep,df$diameter, pch=".", main="day vs. diameter")
  plot(df$timestep,df$height,pch=".", main="day vs. height")
  plot(df$timestep,df$crown_area,pch=".", main="day vs. crown_area")
  plot(df$timestep,df$struct_biomass,pch=".", main="day vs. structural biomass")
  plot(df$timestep,df$root_surface_area,pch=".", main = "day vs. root surface area")
}

calc.A <- function(LAI.c = 4, LAI.u = 1, crown.class = 1) {

  ## calculate maximum light level for understory trees
  L_u <- L_0*(exp(-k*LAI.c))
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

## function to calculate height, crown area, structural biomass and fine-root surface area from vector of diameter values
calc.allom <- function(diam.data) {
  data <- data.frame(diameter = diam.data,
                     height = H*(diam.data^(gamma-1)),
                     crown_area = a_w*(diam.data^gamma),
                     struct_biomass = a_s*(diam.data^(gamma+1)),
                     root_surface_area = r*(a_w*(diam.data^gamma)))
  plot.simulations(df = data)
  return(data)
}

## 2.2 simulator functions


system.time({
  x <- simulate.stand(max_t <- 100, timestep = 1, dnot=dnot, plot.area=100)
})













## run function
df <- simulate.tree(max_t = max_t, timestep = timestep, dnot = dnot)
df <- cbind(timestep = df[,1], dd = df[,3], calc.allom(df[,2]))
plot.simulations(df)

## does it work?

## I think so !
