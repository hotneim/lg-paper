
# Load packages ------
library(lg)
library(ggplot2)
library(lubridate)
library(tidyr)
library(fGarch)
library(dplyr)

# Font package for exact replication of the plots. You need to install the font
# CM Roman for this to work, as well as the pdf export commands surrounding the
# plotting in the code below. 

# library(extrafont) 
# loadfonts(quiet = TRUE)

# Initial example 1 --------

# Estimated local Gaussian correlation between two jointly normal variables 
# having correlation equal to 0.5, using 1000 observations.

set.seed(1)
n   <- 1000
rho <- .5

# Simulated Gaussian data
grid1 <- expand.grid(seq(-3, 3, length.out = 7),
                     seq(-3, 3, length.out = 7))

x1 <- mvtnorm::rmvnorm(500, sigma = matrix(c(1, rho, rho, 1), 2))
lg_object1 <- lg_main(x1, 
                      est_method = "5par", 
                      transform_to_marginal_normality = FALSE,
                      plugin_constant_joint = 4)
dlg_object1 <- dlg(lg_object1, grid = grid1)

# pdf(file = "gaussian-example.pdf", height = 4, width = 5, family = "CM Roman")
corplot(dlg_object1, plot_thres = .01,
        xlab = "X1", ylab = "X2", 
        main = "",
        plot_obs = TRUE,
        alpha_point = .1, 
        plot_legend = FALSE,
        label_size = 4) +
    theme_classic() +
    theme(legend.position = "none")
# dev.off()
# embed_fonts("gaussian-example.pdf")

# Initial example 2 --------

# Plotting the local dependence maps for the returns data
x2 <- read.csv2("G5_daily_r.csv") %>% 
    as_tibble %>% 
    tail(n) %>% 
    select(CAC40, FTSE100) 

grid2 <- expand.grid(seq(-3.5, 3.5, length.out = 7),
                     seq(-3, 3, length.out = 7))

lg_object2 <- lg_main(x2, 
                      est_method = "5par", 
                      transform_to_marginal_normality = FALSE,
                      plugin_constant_joint = 2)
dlg_object2 <- dlg(lg_object2, grid = grid2)

# pdf(file = "returns-example.pdf", height = 4, width = 5, family = "CM Roman")
corplot(dlg_object2, plot_thres = .0001,
        xlab = "CAC40", ylab = "FTSE100", 
        main = "",
        plot_obs = TRUE,
        alpha_point = .1, 
        plot_legend = FALSE,
        xlim = c(-4.5,4.5),
        ylim = c(-3.5,3.5),
        label_size = 3.5) +
    theme_classic() +
    theme(legend.position = "none")
# dev.off()
# embed_fonts("returns-example.pdf")

# The lg-object ---------

x <- faithful
library(lg)

# Creating the various lg-objects
lg_object <- lg_main(x, est_method = "5par")
lg_object$est_method

lg_object2 <- lg_main(x, est_method = "5par_marginals_fixed")

lg_object3 <- lg_main(x, est_method = "1par")

# Throws error here because x is the bivariate faithful data. For trivariate 
# estimation the data set x must have exactly three columns.
# lg_object4 <- lg_main(x, est_method = "trivariate")

lg_object <- lg_main(x, transform_to_marginal_normality = TRUE)

# The transformation plots
# pdf(file = "faithful.pdf", height = 4, width = 5, family = "CM Roman")
x %>% ggplot + 
    geom_point(aes(x = eruptions, y = waiting)) +
    theme_classic()
# dev.off()
# embed_fonts("faithful.pdf")

# pdf(file = "faithful-transformed.pdf", height = 4, width = 5, family = "CM Roman")
lg_object$transformed_data %>% 
    as.data.frame %>% 
    ggplot +
    geom_point(aes(x = V1, y = V2)) +
    xlab("eruptions") +
    ylab("waiting") +
    theme_classic()
# dev.off()
# embed_fonts("faithful-transformed.pdf")

# Bandwidth selection
set.seed(1)
lg_object <- lg_main(x,
                     est_method = "5par_marginals_fixed",
                     transform_to_marginal_normality = TRUE,
                     bw_method = "cv")
lg_object$bw

lg_object <- lg_main(x,
                     est_method = "5par_marginals_fixed",
                     transform_to_marginal_normality = TRUE,
                     bw_method = "plugin")
lg_object$bw

# Statistical inference ------------

# Create the stock data
monthly <- function(x) 100*(prod((x/100 + 1)) - 1)

stock_data <- read.csv2("G5_daily_r.csv") %>% 
    as_tibble %>% 
    mutate(Date = dmy(Date)) %>% 
    select(-CAC40) %>% 
    mutate(year = year(Date),
           month = month(Date)) %>% 
    group_by(year, month) %>% 
    summarize(SP500 = monthly(SP500),
              FTSE100 = monthly(FTSE100),
              DAX30 = monthly(DAX30),
              TOPIX = monthly(TOPIX)) %>% 
    mutate(day = 1) %>% 
    unite(Date, year, month, day, sep = "-") %>% 
    mutate(Date = ymd(Date)) %>% 
    dplyr::filter(Date > ymd("1984-12-31"))

# Create an lg-object
lg_object <- lg_main(x = stock_data %>% select(-Date),
                     est_method = "1par",
                     bw_method = "plugin",
                     transform_to_marginal_normality = TRUE)

# Construct a grid diagonally through the data.
grid_size <- 100
x0 <- stock_data %>% 
    select(-Date) %>% 
    apply(2, function(y) seq(from = -7,
                             to = 7,
                             length.out = grid_size))

# Estimate the local Gaussian correlation on the grid
density_object <- dlg(lg_object, grid = x0)

# Make the plots
var_pairs <- density_object$bw$joint %>% 
    select(x1, x2) %>% 
    mutate(var1 = colnames(stock_data %>% select(-Date))[x1]) %>% 
    mutate(var2 = colnames(stock_data %>% select(-Date))[x2]) %>% 
    select(-x1, -x2) %>% 
    unite(label, var1, var2, sep = "-")

# pdf(file = "loccor-example.pdf", height = 3, width = 5, family = "CM Roman")
density_object$loc_cor %>% 
    as_tibble %>% 
    (function(x) {colnames(x) <- (var_pairs %>% pull); x}) %>% 
    mutate(x0 = x0[,1]) %>% 
    gather(pair, cor, -x0) %>% 
    ggplot() +
    geom_line(aes(x = x0, y = cor, linetype = pair)) +
    labs(linetype = "Variable pair") +
    xlab("x") + 
    ylab("LGC") + 
    ylim(c(0,1)) + 
    theme_classic()
# dev.off()
# embed_fonts("loccor-example.pdf")

# pdf(file = "density-example.pdf", height = 3, width = 5, family = "CM Roman")
density_object$f_est %>%
    as_tibble %>% 
    mutate(x0 = x0[,1]) %>% 
    ggplot() +
    geom_line(aes(x = x0, y = value)) + 
    xlab("x") + 
    ylab("Density estimate") + 
    theme_classic()
# dev.off()
# embed_fonts("density-example.pdf")

# The conditional density

# We must make sure that the free variables come first
returns1 <- stock_data %>% select(SP500, FTSE100, DAX30, TOPIX)

# Create the lg-object
lg_object <- lg_main(returns1,
                     est_method = "1par",
                     bw_method = "plugin",
                     transform_to_marginal_normality = TRUE)

# Create a grid
x0 <- returns1 %>% 
    select(SP500, FTSE100) %>% 
    apply(2, function(y) seq(from = -7,
                             to = 7,
                             length.out = grid_size))

# Calculate the conditional density
cond_density <- clg(lg_object, grid = x0, cond = c(0, 0))

# Independence tests ------
set.seed(1)
n <- 500
x1 <- rnorm(n)
x2 <- x1^2 + rnorm(n)
test_x <- tibble(x1 = x1, x2 = x2)

lg_object <- lg_main(test_x, 
                     est_method = "5par",
                     transform_to_marginal_normality = TRUE)
test_result <- ind_test(lg_object, n_rep = 100)
test_result$p_value

returns2 <- stock_data %>% select(SP500) %>% 
    mutate(sp500_lagged = lag(SP500))

# Contaigion test ------------

# Read the daily data
returns3 <- read.csv2("G5_daily_r.csv") %>%     
    as_tibble  %>% 
    dplyr::select(Date, SP500, FTSE100) %>%
    drop_na %>% 
    mutate(Date = dmy(Date)) %>%  
    gather(exchange, return, -Date, factor_key = TRUE)

# Example: Contagion 1987 US Stock market crash ------
stable_start <- ymd("1985-01-02")
stable_end   <- ymd("1987-10-16")

crisis_start <- ymd("1987-10-18")
crisis_end   <- ymd("1988-04-29")

garch_filtrate <- function(y) {
    invisible(capture.output(fit <- garchFit(~ garch(1, 1), 
                                             data = y, 
                                             cond.dist = "std")))
    fit@residuals/fit@sigma.t
}

# The following sequence generates a warning about the use of formula(x). This
# is a bug in the fGarch-package that appeared after the 4.0 R release. Please
# ignore, the results are the same.
crash_data <- returns3 %>% 
    dplyr::filter(Date >= stable_start) %>% 
    dplyr::filter(Date <= crisis_end) %>% 
    mutate(crisis = ifelse(Date <= stable_end,
                           yes = "BEFORE",
                           no = "AFTER")) %>% 
    mutate(crisis = as.factor(crisis)) %>% 
    group_by(exchange) %>% 
    mutate(return_filtered = garch_filtrate(return)) %>% 
    ungroup

x_nc <- data.frame(US = crash_data %>% 
                       dplyr::filter(exchange == "SP500") %>%
                       dplyr::filter(crisis == "BEFORE") %>% 
                       dplyr::select(return_filtered) %>% 
                       pull,
                   UK = crash_data %>% 
                       dplyr::filter(exchange == "FTSE100") %>%
                       dplyr::filter(crisis == "BEFORE") %>% 
                       dplyr::select(return_filtered) %>% 
                       pull) %>% 
    as_tibble

x_c <- data.frame(US = crash_data %>% 
                      dplyr::filter(exchange == "SP500") %>%
                      dplyr::filter(crisis == "AFTER") %>% 
                      dplyr::select(return_filtered) %>% 
                      pull,
                  UK = crash_data %>% 
                      dplyr::filter(exchange == "FTSE100") %>%
                      dplyr::filter(crisis == "AFTER") %>% 
                      dplyr::select(return_filtered) %>% 
                      pull) %>% 
    as_tibble

# Create the two lg-objects
lg_object_nc <- lg_main(x_nc, 
                        est_method = "5par",
                        transform_to_marginal_normality = FALSE)

lg_object_c <- lg_main(x_c, 
                       est_method = "5par",
                       transform_to_marginal_normality = FALSE)

# Run the test with a limited number of bootstrap replicates for 
# demonstration purposes.
set.seed(1)
result <- cont_test(lg_object_nc, lg_object_c, n_rep = 100)

# Print out the p-value
result$p_value

# Local partial correlation ----------
set.seed(1)
returns4 <- stock_data %>%
    dplyr::filter(year(Date) > 2000) %>% 
    mutate(FTSE100_lagged = lag(FTSE100)) %>% 
    mutate(SP500_lagged = lag(SP500)) %>% 
    select(FTSE100, SP500_lagged, FTSE100_lagged)

# Create the lg-object
lg_object <- lg_main(returns4)

# Perform the test
# set.seed(1)
test_result <- ci_test(lg_object, n_rep = 100)

# Print out result
test_result$p_value

# Graphics ----------
# Simulated Gaussian data
set.seed(1)
grid <- expand.grid(seq(-3, 3, length.out = 7),
                    seq(-3, 3, length.out = 7))

x <- mvtnorm::rmvnorm(500, sigma = matrix(c(1, rho, rho, 1), 2))
lg_object <- lg_main(x, 
                     est_method = "5par", 
                     transform_to_marginal_normality = FALSE,
                     plugin_constant_joint = 4)
dlg_object <- dlg(lg_object, grid = grid)

# Make a dependence map using default setup
pdf("graphics1.pdf", height = 5, width = 7, family = "CM Roman")
corplot(dlg_object1)
dev.off()
embed_fonts("graphics1.pdf")

pdf("graphics2.pdf", height = 5, width = 7, family = "CM Roman")
corplot(dlg_object1, 
        plot_obs = TRUE, 
        plot_thres = 0.01,
        plot_labels = FALSE,
        alpha_point = 0.3,
        main = "",
        xlab = "",
        ylab = "") +
    theme_classic()
dev.off()
embed_fonts("graphics2.pdf")