# -----------------------------------------------------------------------------------------------------#
# Codes for replicating the paper Going Beyong the Gender Gap
# authors: Vanessa di Lego, Marília Nepomuceno, Cássio M. Turra
# Wittgenstein Centre for Demography and Global Human Capital(IIASA/OeAW/UniWien)
# Vienna Institute of Demography at the Austrian Academy of Sciences
# Max Planck Institute for Demographic Research
# Federal University of Minas Gerais (Cedeplar)
# -----------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------#
# Functions used in this project
#-----------------------------------------------------------------------------------------------------#
start.age = 60
open.age = 80

Sullivan.fun = function (rates,age=seq(start.age,open.age,5)) {
  # 1) First, we split from our single vector 'rates' the set of age-specific death rates (mx) 
  # and age-specific prevalence of disability/chronic (wx)
  lengthvec <- length(rates)
  mx <- rates[1:(lengthvec / 2)]
  wx <- rates[(lengthvec / 2 + 1):lengthvec]
  # 2) Calculating period life table functions
  # ax
  n <- c(diff(age), 5)
  ax <- 0.5 * n
  # probability of dying (qx) and surviving (px)
  qx <- (n * mx)/(1 + (n - ax) * mx)
  qx <- c(qx[-(length(qx))], 1)
  qx[qx > 1] <- 1
  px <- 1 - qx
  # survivors at age x (lx)
  lx <- c(100000,rep(0,(length(mx)-1)))
  for (i in 1:(length(mx) -1)){
    lx[i+1] <- lx[i]*px[i] }
  # deaths between ages x and x+n (dx)
  dx <- lx * qx
  # person-years lived between ages x and x+n (Lx)
  Lx <- rep(0,length(mx))
  for (i in 1:length(mx) -1){
    Lx[i] <- lx[i+1]*n[i] + ax[i]*dx[i] }
  Lx[length(mx)] <- lx[length(mx)]/mx[length(mx)]
  Tx  <- rev(cumsum(rev(Lx)))
  ex = Tx/lx
  # 3) getting the person-years lived with health (without disability or chronic conditions) between ages x and x+n
  Lx.health <-  Lx*(1-wx)
  Tx.health  <- rev(cumsum(rev(Lx.health)))
  # 4) healthy life expectancy 
  ex.health <- Tx.health/lx
  return.df <- data.frame(age, n, ax, qx, px, lx, dx, Lx, Tx,ex,Lx.health, Tx.health, ex.health)
  return(ex.health[1])
} 

### Life table

life.table <- function(mx){
  ax <- c(0.14, rep(0.5, length(mx)-1))
  qx <- mx/(1+(1-ax)*mx)
  qx[length(qx)] <- 1
  qx[qx > 1] <- 1
  px <- 1-qx
  lx <- c(100000, (cumprod(px)*100000)[1:(length(px)-1)])
  dx <- c(-diff(lx), lx[length(lx)])
  Lx1 <- lx[-1]+ax[-length(ax)]*dx[-length(dx)]
  open.Lx <-  ifelse( mx[length(mx)] == 0, 0, dx[length(dx)]/mx[length(mx)])
  Lx <- c(Lx1, open.Lx)
  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  
  return(data.frame(qx=qx, px = px, ax = ax, lx = lx , dx = dx, Lx= Lx,
                    Tx = Tx, ex = ex))
}


my_theme <- function() {
  # Colors
  color.background = "white"
  color.text = "#22211d"
  # Begin construction of chart
  theme_bw(base_size=15) +
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    # Format the legend
    theme(legend.position = "none") +
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
