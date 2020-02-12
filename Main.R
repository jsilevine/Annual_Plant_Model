## Main file for running annual plant models including model compiling and output visualizations.


## package calls


## ********* model for single tree *********

## variable definition

#### allometric variables
l <- 4           ## leaf area index of tree
r <- 6.5         ## fine-root surface area per unit crown area
H <- 3.6         ## allometric constant for height
a_s <- 0.0815    ## allometric constant for sapwood
a_w <- 0.20      ## allometric constant for crown area
gamma <- 1.5     ## allometric exponent


#### carbon accumulation and allocation variables
c_l <- 0.187/365  ## cost of building and maintaining leaf biomass including sapwood respiration
c_r <- 0.044/365  ## cost of builidng and maintaining fine-root biomass
c_bg <- 0.2       ## building cost of structural biomass
V <- 0.6/365      ## maximum rate of carbon fixation - kgC/m^2/day
k <- 0.33         ## light extinction coefficient
A <- (V/k)*(1-exp(-k*l)) ## carbon assimilation rate per unit crown area

#### simulation variables
max_t <- 1000      ## length of simulation in days
dnot <- 0.0001    ## starting diameter
timestep <- 1

## 2.function definition

## 2.1 general purpose functions
## function to calculate diameter growth
calc.dd <- function(data, i) {
  dd <- 1/((a_s*(gamma+1)*(1+c_bg)*(1/a_w))+((gamma/data[i,2])*(l*c_l + r*c_r)))*(A-(l*c_l)-(r*c_r))
  return(dd)
}

## function to calculate height, crown area, structural biomass and fine-root surface area from vector of diameter values
calc.allom <- function(diam.data) {
  data <- data.frame(diameter = diam.data,
                     height = H*(diam.data^(gamma-1)),
                     crown_area = a_w*(diam.data^gamma),
                     struct_biomass = a_s*(diam.data^(gamma+1)),
                     root_surface_area = r*(a_w*(diam.data^gamma)))
  return(data)
}

## function to generate plots of simulated data:
plot.simulations <- function() {

}

## 2.2 simulator functions

## 2.2.1 single-tree simulator
simulate.tree <- function(max_t, timestep, dnot) {

  ## create dataframe with initial tree
  data <- data.frame(timestep = 0, diameter = dnot, dd = NA)

  ## loop over days in simulation
  for (i in seq(1, max_t, by = timestep)) {

    ## calculate diameter growth
    dd <- calc.dd(data=data, i=i)
    d_n <- data[i,2]+dd
    data <- rbind(data, c(i, d_n, dd))
  }
  return(data)
}

## 2.2.2 single-species simulator





## run function
df <- simulate.tree(max_t = max_t, timestep = timestep, dnot = dnot)
df <- cbind(timestep = df[,"timestep"], calc.allom(df[,2]))


## does it work?
plot(df$diameter,df$dd, pch=".")
plot(df$timestep,df$diameter, pch=".")
plot(df$timestep,df$height,pch=".")
plot(df$timestep,df$crown_area,pch=".")
plot(df$timestep,df$struct_biomass,pch=".")
plot(df$timestep,df$root_surface_area,pch=".")

## I think so !
