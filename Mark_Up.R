#***********************************************
#* Real wages vs productivity - EPI chart
#***********************************************
library(readabs)
library(dplyr)
library(tidyverse)
library(KFAS)
library(tempdisagg)


#**********************
#* Data
#**********************

# National Accounts
NA_DATA <- readabs::read_abs("5206")

# Labour Accounts
LA_DATA <- readabs::read_abs("6150.0.55.003")

# Labour Force
LF_DATA <- readabs::read_abs("6202")

# Annual national accounts
KT_DATA <- readabs::read_abs("5204.0")

#  Payments; Average hourly income per Labour Account employed person ;  Australia ;  Agriculture, forestry and fishing (A) ;  

# Wages (Employee Earnings)
show_available_files("employee-earnings")

ee_T01 <- download_abs_data_cube("employee-earnings", "Table01")

ee1 <- ee_T01 %>%
  readxl::read_excel(sheet = "Data 1",skip = 5) %>% 
  select(1,2,3,4,5,22,23)

Median_E <- ee1 %>%
  filter(`...1` == "Median hourly earnings" & `...2` == "Persons" & `...3` == "Total" & `...4` == "Total") 

Median_E$...5 <- as.Date(as.numeric(Median_E$...5), origin = "1899-12-30")
Median_E$Australia <- as.numeric(Median_E$Australia)

Median_E <- tibble(Median_HE = Median_E$Australia,
                   date = Median_E$...5) %>% 
  mutate(date = as.Date(gsub("-08-","-09-",.$date)))


#**********************
#* Aggregate
#**********************

Y_L_W_P <- NA_DATA %>%
  filter(table_title == "Table 1. Key National Accounts Aggregates") %>% 
  filter(grepl("Gross domestic product: Chain volume measures ;",series)) %>% 
  filter(grepl("Seasonally", series_type)) %>% 
  select(date,value) %>% 
  rename(Output = value) %>% 
  left_join(NA_DATA %>% 
              filter(table_title == "Table 1. Key National Accounts Aggregates") %>% 
              filter(grepl("Hours worked: Index ;",series)) %>% 
              filter(grepl("Seasonally", series_type)) %>%
              select(date,value) %>% 
              rename(Hours = value)) %>%
  left_join(NA_DATA %>% 
                        filter(table_title == "Table 7. Income from Gross Domestic Product (GDP), Current prices") %>% 
                        filter(series == "Compensation of employees ;") %>% 
                        filter(grepl("Seasonally", series_type)) %>%
                        select(date,value) %>% 
                        rename(COE = value)) %>%
  left_join(NA_DATA %>% 
                        filter(table_title == "Table 5. Expenditure on Gross Domestic Product (GDP), Implicit price deflators") %>% 
                        filter(series == "Households ;  Final consumption expenditure ;") %>% 
                        filter(grepl("Seasonally", series_type)) %>%
                        select(date,value) %>% 
                        rename(PCONH = value))  %>%
  left_join(NA_DATA %>% 
              filter(table_title == "Table 5. Expenditure on Gross Domestic Product (GDP), Implicit price deflators") %>% 
              filter(series == "GROSS DOMESTIC PRODUCT ;") %>% 
              filter(grepl("Seasonally", series_type)) %>%
              select(date,value) %>% 
              rename(PGDP = value)) %>%
  left_join(LF_DATA %>% 
             
              filter(series_id %in% ("A84423091W")) %>% #c("A84426277X" 
              mutate(year = year(date),
                     quarter = quarter(date)) %>%
              group_by(year,quarter,series) %>% 
              summarise(value = mean(value)) %>%
              spread(series,value) %>% 
              rename(POP15 = 3) %>% 
              mutate(date = as.Date(paste0(year,"-",quarter*3,"-01"))) %>% 
              ungroup() %>% 
              select(date,POP15)) %>%
  left_join(LF_DATA %>% 
  filter(series_id %in% ("A84426277X")) %>% #c("A84426277X" 
  mutate(year = year(date),
         quarter = quarter(date)) %>%
  group_by(year,quarter,series) %>% 
  summarise(value = sum(value)) %>%
  spread(series,value) %>% 
  rename(THW = 3) %>% 
  mutate(date = as.Date(paste0(year,"-",quarter*3,"-01"))) %>% 
  ungroup() %>% 
  select(date,THW)) %>%
  
  mutate(INFL = PCONH/lag(PCONH,4)*100-100) %>% 
  mutate(PROD = Output/THW*1000) %>%
  mutate(THW_POP = THW/POP15,
         AENA_H = COE/THW*1000) %>% 
  
  filter(!is.na(PROD)) %>% 

  mutate(O_remove = forecast::tsclean(ts(.$PROD, f = 4)) ) %>% 
  mutate(PROD_T = stl(ts(O_remove, f = 4), s.window = "per", t.window = 60)$time.series[,2]) %>%
  mutate(O_remove = forecast::tsclean(ts(.$INFL, f = 4)) ) %>% 
  filter(!is.na(O_remove)) %>% 
  mutate(INFE = stl(ts(O_remove, f = 4), s.window = "per", t.window = 120)$time.series[,2])
  
  
  
dat <- Y_L_W_P %>% 
  select(date, INFL,INFE, PROD, PROD_T, AENA_H, THW_POP, Output, PGDP, Hours, COE) %>% 
  mutate(AENA_H = AENA_H/lag(AENA_H)*100-100,
         PROD_T = PROD_T/lag(PROD_T)*100-100,
         PROD = PROD/lag(PROD)*100-100,
         INFE = (1+INFE/100)^(1/4)*100-100,
         INFL = (1+INFL/100)^(1/4)*100-100,
         N_SHARE = mean(COE[.$date>="2022-09-01"&.$date<="2023-06-01"]/(Output[.$date>="2022-09-01"&.$date<="2023-06-01"]*(PGDP[.$date>="2022-09-01"&.$date<="2023-06-01"]/100)))) 

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# State space model for wages - trend input
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# W = P* + INFE + GAP
# N = -P* + Y +sigma*(W/P)

# State space model is of the form:
#    Y(t) = Z(t)*A(t) + H
#    A(t+1) = T*A(t) +  Q

# A = [hgap, H*, a1, a2]  = 5x1
# n = T x 2

# Y = [Ti x n]
# Z = [n x m]
# A = [m x 1]
# T = [m x m]
# H = [n x n]
# Q = [m x m]

# T = [a, 0, 0, 0, 
#      0, 1, 0, 0, 
#      0, 0, 1, 0, 
#      0, 0, 0, 1,]



Ti <- dim(dat)[1]
Y <- matrix(NA,Ti,2)
Int <- matrix(NA,Ti,2)

Y[1:Ti,1] <- dat$AENA_H-dat$INFE-dat$PROD_T
Y[1:Ti,2] <- dat$TWH_POP

# Set up the time varing Z matrix (fill it with out exogenous variables)
Z <- array(0, c(2, 4, Ti))


# wages measurement equation  a*gap + b*(pr-pr*) + c*(pi-pi*) 
Z[1, 1, 1:Ti] <-  NA
Z[1, 3, 1:Ti] <-  0.0
Z[1, 3, 1:Ti] <-  (dat$PROD-dat$PROD_T)
Z[1, 4, 1:Ti] <- (dat$INFL-dat$INFE)

# total hours measurement equation u = u* + gap
Z[2, 1, 1:Ti] <- 1.0
Z[2, 2, 1:Ti] <- 1.0


# Set up the time invariant T matrix 
T <- diag(4) 
T[1, 1] <- NA 

Q <- diag(c(NA,NA,NA,NA))

P1inf <- diag(1,4) 
P1inf[1,1] <- 0


P1 <- diag(0,4)

a1 <- c(0,0,0,0)

wages_mod <- SSModel(Y ~ -1 + SSMcustom(Z = Z, T = T, Q = Q,a1 =a1,  P1 = P1, P1inf = P1inf,
                                     state_names = c("gap", "nht*","b", "c")), H = diag( c(NA,0), 2))


# *********************************************************************
# parameter optimistion
# *********************************************************************

# Set up objective function

objf <- function(params, model, estimate = TRUE) {
  
  model$Z[1,1,] <- params[1]                       
  
  model$T[,,1][1,1] <- params[2]
  
  diag(model$Q[,,1]) <- c(params[3]^2, 0.01*(params[3]^2),0,0)
  
  model$H[,,1][1,1] <- (params[4])^2
  
  
  if (estimate) {
    -logLik(model)
  } else {
    model
  }
}


# Optimise hyperparameters
opt <- optim(c(0.5,0.5,0.5,0.1), objf, method = "L-BFGS-B",
               
               model = wages_mod,  control =list(trace = 6, REPORT = 5, maxit = 1000), hessian = TRUE)



# Paramaterise model
wages_model_opt <- objf(opt$par, wages_mod, estimate = FALSE)

# Estimate diagnostics
se_15 <- diag(sqrt(solve(opt15$hessian)))



#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# State space model for wages + Labour demand - trend input
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# W = P* + INFE + GAP
# N/pop =  Nshare -P* + Y/pop +sigma*(W/P)

# State space model is of the form:
#    Y(t) = Z(t)*A(t) + H
#    A(t+1) = T*A(t) +  Q

# A = [gap,H*, a1, P, d, gap]  = 5x1
# n = T x 4


# Y = [Ti x n]
# Z = [n x m]
# A = [m x 1]
# T = [m x m]
# H = [n x n]
# Q = [m x m]

# T = [a, 0, 0, 0, 
#      0, 1, 0, 0, 
#      0, 0, 1, 0, 
#      0, 0, 0, 1,]



Ti <- dim(dat)[1]
Y <- matrix(NA,Ti,3)
Int <- matrix(NA,Ti,3)

Y[1:Ti,1] <- dat$AENA_H-dat$INFE
Y[1:Ti,2] <- dat$TWH_POP
Y[1:Ti,3] <- dat$PROD




# Set up the time varing Z matrix (fill it with out exogenous variables)
Z <- array(0, c(3, 6, Ti))


# wages measurement equation  a*gap + b*(pr-pr*) + c*(pi-pi*) 
Z[1, 1, 1:Ti] <-  NA # hours gap
Z[1, 2, 1:Ti] <-  0.0  # 
Z[1, 3, 1:Ti] <- (dat$INFL-dat$INFE)
Z[1, 5, 1:Ti] <-  NA  # prod gap


# total hours measurement equation u = u* + gap
Z[2, 1, 1:Ti] <- 1.0
Z[2, 2, 1:Ti] <- 1.0


# Prod gap measurement equation u = u* + gap
Z[3, 4, 1:Ti] <- 1.0
Z[3, 6, 1:Ti] <- 1.0



# Set up the time invariant T matrix 
T <- diag(4) 
T[1, 1] <- NA 

Q <- diag(c(NA,NA,NA,NA))

P1inf <- diag(1,4) 
P1inf[1,1] <- 0


P1 <- diag(0,4)

a1 <- c(0,0,0,0)

wages_mod <- SSModel(Y ~ -1 + SSMcustom(Z = Z, T = T, Q = Q,a1 =a1,  P1 = P1, P1inf = P1inf,
                                        state_names = c("gap", "nht*","b", "c")), H = diag( c(NA,0), 2))


# *********************************************************************
# parameter optimistion
# *********************************************************************

# Set up objective function

objf <- function(params, model, estimate = TRUE) {
  
  model$Z[1,1,] <- params[1]                       
  
  model$T[,,1][1,1] <- params[2]
  
  diag(model$Q[,,1]) <- c(params[3]^2, 0.01*(params[3]^2),0,0)
  
  model$H[,,1][1,1] <- (params[4])^2
  
  
  if (estimate) {
    -logLik(model)
  } else {
    model
  }
}


# Optimise hyperparameters
opt <- optim(c(0.5,0.5,0.5,0.1), objf, method = "L-BFGS-B",
             
             model = wages_mod,  control =list(trace = 6, REPORT = 5, maxit = 1000), hessian = TRUE)



# Paramaterise model
wages_model_opt <- objf(opt$par, wages_mod, estimate = FALSE)

# Estimate diagnostics
se_15 <- diag(sqrt(solve(opt15$hessian)))

#-----------------------------------------------------------------------------------------
# Run Kalman filter and smoother
#-----------------------------------------------------------------------------------------

dlm_wages <- KFS(wages_model_opt)

#-----------------------------------------------------------------------------------------
# Mark-ups and the labour income share
#-----------------------------------------------------------------------------------------

# For rental rate on capital, used to derive the markup we need 
# p_cap (price deflator on capital stock)
# depreciation rate
# nominal interest rate (10 yr bond?)
# gdp deflator
# R = pk/p*(i - dlog(pk)+dep)

Annual_KT <- KT_DATA %>% 
  filter(table_title == "Table 56. Capital Stock, by Type of asset") %>% 
  filter(series %in% c("End-year net capital stock: Chain volume measures ;",
                       "End-year net capital stock: Current prices ;")) %>% 
  select(date, value, series) %>% 
  spread(series, value)


Qtrly_Inv <-  NA_DATA %>% 
  filter(table_title %in% c("Table 2. Expenditure on Gross Domestic Product (GDP), Chain volume measures",
                           "Table 3. Expenditure on Gross Domestic Product (GDP), Current prices")) %>% 
  filter(series == "All sectors ;  Gross fixed capital formation ;") %>% 
  filter(series_type == "Seasonally Adjusted") %>%
  mutate(Var = if_else(table_title =="Table 2. Expenditure on Gross Domestic Product (GDP), Chain volume measures", "IT","ITZ")) %>% 
  select(date, value, Var) %>% 
  spread(Var, value) %>% 
  mutate(PIT = ITZ/IT)

QTRLY_KT <- list()
for(i in c(1,2)){
  
  Annual_series <- ts(Annual_KT[[i+1]], start = year(Annual_KT$date[1]))
  indic <- ts(Qtrly_Inv[[i+1]], start = c(year(Qtrly_Inv$date[1]),quarter(Qtrly_Inv$date[1])), frequency = 4)
  
  disag_out <- tempdisagg::td(Annual_series~-1+indic,conversion = "sum", to = "quarter")
  
  QTRLY_KT[[i]] <- disag_out %>% predict()
  
}

# Replace with data download when 

m <- tst.macrodata()$m


RR_Data <- tibble(date = Y_L_W_P$date) %>% 
  left_join(Qtrly_Inv) %>% 
  left_join(tibble(KT = QTRLY_KT[[1]],
                   date = seq.Date(ymd("1959-09-01"),length.out = length(QTRLY_KT[[1]]), by = "quarter"))) %>%
  left_join(tibble(KTZ = QTRLY_KT[[2]],
                   date = seq.Date(ymd("1959-09-01"),length.out = length(QTRLY_KT[[1]]), by = "quarter"))) %>%
 mutate(PKT = KTZ/KT) %>%
  left_join(Y_L_W_P %>% 
              mutate(PGDP = PGDP/100) %>% 
              select(date, PGDP)) %>%
  left_join(m %>%
              rename(date = Date) %>% 
              select(date, rate_10))

  

Dep <-  KT_DATA %>% 
  filter(table_title == "Table 56. Capital Stock, by Type of asset") %>% 
  filter(series %in% c("Consumption of fixed capital: Current prices ;",
                       "End-year net capital stock: Current prices ;")) %>% 
  select(date, value, series) %>% 
  spread(series, value) %>% 
  mutate(Dep_rate = .[[2]]/lag(.[[3]])) %>%
  filter(date %in% RR_Data$date) %>% 
  summarise(Dep_rate = mean(Dep_rate, na.rm = T))

# R = pk/p*(i - dlog(pk)+dep)

RR_Data <- RR_Data %>% 
  mutate(R = 100*(PIT/PGDP*((rate_10/100)-(PIT/lag(PIT,4)-1) + Dep[[1]])))

RR_Data %>%
  
  select(date,R, rate_10) %>% 
  gather(Var, Val, -date) %>% 
  tst.plot_line(aes(date, Val, colour = Var))

# MGR



#**********************
#* Aggregate LA
#**********************


Y_L_W_P_M <- NA_DATA %>%
  filter(table_title == "Table 1. Key National Accounts Aggregates") %>% 
  filter(grepl("Gross domestic product: Chain volume measures ;",series)) %>% 
  filter(grepl("Seasonally", series_type)) %>%
  mutate(O_remove = forecast::tsclean(ts(.$value, f = 4)) ) %>% 
  filter(!is.na(O_remove)) %>% 
  mutate(Trend = stl(ts(O_remove, f = 4), s.window = "per", t.window = 60)$time.series[,2]) %>% 
  select(date,value, Trend) %>% 
  rename(Output = value) %>% #A85389483J 
  left_join(LA_DATA %>% 
              filter(table_no != "industry+summary+table") %>% 
              filter(series_id == "A85389483J") %>% 
              select(date,value) %>% 
              rename(Hours = value)) %>%
  left_join(LA_DATA %>% 
              filter(table_no != "industry+summary+table") %>% 
              filter(series_id == "A85389465C") %>% 
              select(date,value) %>% 
              rename(Vac = value)) %>%
  left_join(LA_DATA %>% 
              filter(table_no != "industry+summary+table") %>% 
              filter(series_id == "A85389473C") %>% 
              select(date,value) %>% 
              rename(LF = value)) %>%
  left_join(LA_DATA %>% 
              filter(table_no != "industry+summary+table") %>% 
              filter(series_id == "A85389479T") %>% 
              select(date,value) %>% 
              rename(UNE = value)) %>%
  
  
  left_join(NA_DATA %>% 
              filter(table_title == "Table 7. Income from Gross Domestic Product (GDP), Current prices") %>% 
              filter(series == "Compensation of employees ;") %>% 
              filter(grepl("Seasonally", series_type)) %>%
              select(date,value) %>% 
              rename(COE = value)) %>%
  left_join(NA_DATA %>% 
              filter(table_title == "Table 5. Expenditure on Gross Domestic Product (GDP), Implicit price deflators") %>% 
              filter(series == "Households ;  Final consumption expenditure ;") %>% 
              filter(grepl("Seasonally", series_type)) %>%
              select(date,value) %>% 
              rename(PCONH = value))  %>%
  left_join(NA_DATA %>% 
              filter(table_title == "Table 5. Expenditure on Gross Domestic Product (GDP), Implicit price deflators") %>% 
              filter(series == "GROSS DOMESTIC PRODUCT ;") %>% 
              filter(grepl("Seasonally", series_type)) %>%
              select(date,value) %>% 
              rename(PGDP = value)) %>% 
  left_join(Median_E) %>% 
  left_join(RR_Data %>% 
              select(-PGDP)) %>% 
  mutate(Median_HE = zoo::na.spline(Median_HE)) %>% 
  mutate(Median_HE = ifelse(Median_HE < 0.01,NA,Median_HE)) %>%
  mutate(AENA_H = COE*1000/Hours) %>% 
    mutate(Vac_Rate = Vac/LF, # TODO add
         UNE_Rate = UNE/LF, # TODO Add
         #Hours_Share = Hours/Hours_TOTAL*100, # TODO Add
         YGAP = Output/Trend*100-100,  # TODO
         Prod = Output/Hours, 
         Mark_up_1 = Prod/(AENA_H/(PGDP)),
         Mark_up = 1-(COE/((PGDP/100)*Output))-(R/100)*(KT/Output)) %>%
#  filter(!is.na(Mark_up)) %>% 
  mutate(O_remove = forecast::tsclean(ts(Mark_up, f = 4)) ) %>% # M-PR+A = Pr-A+P
  #mutate(O_remove = tsoutliers::tso(ts(.$value, f = 4)) %>% .[[3]]) %>%
  mutate(Trend_MU = stl(ts(O_remove, f = 4), s.window = "per", t.window = 60)$time.series[,2]) %>% 
  mutate(MGAP = (Mark_up-Trend_MU)*100 ) %>% 
  left_join(tibble(date = dat$date,
                   LGAP = dlm_wages$alphahat[,"gap"]))
  
  
  
 # mutate(Median_HE = Hours*Median_HE) %>% 
  


WP_PROD <- Y_L_W_P_M %>%
  filter(date >= "1997-06-01") %>% 
  gather(Var, Val, -date) %>% 
  group_by(Var) %>% 
  mutate(Val = Val/Val[which(date == "2019-12-01")]*100) %>% 
  spread(Var, Val) %>% 
  mutate(Prod = Output/Hours*100,
         #AENA_H = (COE/Hours)*100,
         Cons_RW = AENA_H/PCONH*100,
         Prod_RW = AENA_H/PGDP*100,
         Cons_RW_M = Median_HE/PCONH*100,
         PROD_RW_M = Median_HE/PGDP*100) %>% 
  select(date, Prod_RW,Cons_RW,Prod) %>% 
  gather(Var, Val, -date) %>% 
  filter(!is.na(Val)) %>% 
  tst.plot_line(aes(date, Val, colour = Var))+
  ggtitle("Real wages and productivity")




BEV_C <- Y_L_W_P_M %>% 
  select(date, Vac_Rate, UNE_Rate) %>% 
  mutate(Year = year(date)) %>%
  mutate(Year = ifelse(Year == 2023,"2023",
                       ifelse(Year == 2022,"2022",
                              ifelse(Year == 2021,"2021",
                                     ifelse(Year == 2020,"2020",
                                            ifelse(Year == 2024,"2024","All other years"
                                            ))
                              )
                       )
  )
  ) %>% 
  ggplot(aes(UNE_Rate, Vac_Rate, colour = Year)) +
  geom_point(size = 4) + 
  tst_theme()+
  scale_colour_tst(discrete = TRUE, palette = "mixed")+
  ggtitle("Beveridge curve")


MARK_UP_YGAP <- Y_L_W_P_M %>%
  filter(!is.na(MGAP)) %>% 
  select(date, MGAP,YGAP,LGAP) %>% 
  gather(Var, Val, -date) %>% 
  tst.plot_line(aes(date, Val, colour = Var))+
  geom_hline(yintercept = 0)+
  ggtitle("Mark up and output cycles")


YGAP_MU_REG <- lm(Y_L_W_P_M$MGAP~-1+Y_L_W_P_M$YGAP+Y_L_W_P_M$LGAP) 

PGDP_DECOMP_dat <-   Y_L_W_P_M %>%  
  select(date, PGDP, Prod, AENA_H) %>%
  mutate(PGDP = PGDP/lag(PGDP,4)*100-100) %>%
  mutate(Prod = -1*(Prod/lag(Prod,4)*100-100)) %>%
  mutate(`Hourly wage` = AENA_H/lag(AENA_H,4)*100-100) %>%
#  mutate(YGAP = YGAP*YGAP_MU_REG$coefficients[2]) %>%
  mutate(Mark_up = PGDP-`Hourly wage`-Prod) %>% 
  select(-AENA_H) %>% 
  filter(!is.na(Mark_up)) %>% 
  gather(Var, Val, -date) %>% 
  filter(!is.na(Val)) 

PGVA_DECOMP_Chart <-  PGDP_DECOMP_dat %>% 
  filter(Var != "PGDP") %>% 
  ggplot(aes(date, Val)) +
  geom_bar(aes(fill = Var),stat = "identity") +
  geom_line(data =PGDP_DECOMP_dat %>% 
              filter(Var == "PGDP"), aes(date, Val), colour = "purple", size =1)+
  tst_theme()+
  scale_fill_tst(discrete = TRUE)+
  ggtitle("Output price decomposition")

(WP_PROD+BEV_C)/(MARK_UP_YGAP+PGVA_DECOMP_Chart)

#**********************
#* Terms of Trade - side plot
#**********************

ToT_dat <- NA_DATA %>% 
  filter(series_id == "A2304200A") %>% 
  filter(grepl("Seasonally", series_type)) %>%
  select(date,value) %>% 
  rename(ToT = value) %>% 
  mutate(Mean_5yr_pre_COVID = ifelse(date >= "2015-03-01" & date <= "2019-12-01", mean(ToT[.$date >= "2015-03-01" & .$date <= "2019-12-01"]),NA)) %>% 
  mutate(Mean_pre_MB = ifelse(date <= "2003-12-01", mean(ToT[.$date <= "2003-12-01"]),NA))

Max_ToT_MB <- ToT_dat %>% 
  filter(date <= "2012-06-01") %>% .$ToT %>% 
  max()

Mean_Pre_boom <- ToT_dat[1,"Mean_pre_MB"][[1]] 


Current_TOT <- ToT_dat %>% 
  filter(date > "2016-06-01") %>% .$ToT %>% 
  max()
  
Mean_5y_Covid <- ToT_dat[!is.na(ToT_dat$Mean_5yr_pre_COVID),"Mean_5yr_pre_COVID"]
Mean_5y_Covid <- Mean_5y_Covid[1,1][[1]] 

TOT_PLOT <- ToT_dat %>% 
  gather(Var,Val, -date) %>% 
  tst.plot_line(aes(date, Val, colour = Var))+
  annotate("segment", x = ymd("2001-12-01"), y = 51, xend = ymd("2010-12-01"), yend = 98,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  annotate("text", x = ymd("1990-12-01"), y = 80, label = paste0("Inrease from pre mining boom \n average to peak = ",round(Max_ToT_MB/Mean_Pre_boom*100-100,2)), size =2)+
  annotate("segment", x = ymd("2016-12-01"), y = 73, xend = ymd("2022-03-01"), yend = 107,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  annotate("text", x = ymd("2012-12-01"), y = 60, label = paste0("Inrease from pre  COVID  \n 5yr average to peak = ",round(Current_TOT/Mean_5y_Covid$Mean_5yr_pre_COVID*100-100,2)), size = 2)

TOT_PLOT+WP_PROD
  
  


#**********************
#* Industry
#**********************

ind <- c("AG","MIN","MAN","EGW","CONS","WT","RT","AFS","TPW","IMT","FIS","RHR","PST","ASS","PAS","ET",'HCS',"ARS","OS")
GVA <- c("A2716160J","A2716163R","A2716166W","A2716175X","A2716179J","A2716181V","A2716182W","A2716183X","A2716188K","A85231762X","A2716189L","A2716190W","A2716191X","A2716585R","A2716192A","A2716193C","A2716194F","A2716195J","A2716196K")

GVAZ <- NA_DATA %>% 
  filter(table_title == "Table 45. Gross Value Added by Industry, Current prices") %>% 
  filter(series_type == "Seasonally Adjusted") %>% 
  filter(grepl("Gross value added at basic prices", .$series)) %>% 
  select(series,series_id) %>% 
  unique() 

Hours_Inc <- LA_DATA %>% 
  filter(grepl("Payments; Average hourly income per Labour",.$series)) %>%
  filter(series_type == "Seasonally Adjusted") %>%
  filter(data_type == "AVERAGE") %>%
  filter(!grepl("Revisions",.$table_title)) %>% 
  filter(!grepl("All Industries",.$table_title)) %>% 
  select(table_title, series_id) %>% 
  unique()


Hours <- LA_DATA %>% 
  filter(grepl("Volume; Labour Account hours actually worked in all jobs",.$series)) %>%
  filter(series_type == "Seasonally Adjusted") %>%
  filter(data_type == "FLOW") %>%
  filter(!grepl("Revisions",.$table_title)) %>% 
  filter(!grepl("All Industries",.$table_title)) %>% 
  select(table_title, series_id) %>% 
  unique()

Vacancies <- LA_DATA %>% 
  filter(grepl("Jobs; Job vacancies",.$series)) %>%
  filter(series_type == "Seasonally Adjusted") %>%
  filter(data_type == "STOCK") %>%
  filter(!grepl("Revisions",.$table_title)) %>% 
  filter(!grepl("All Industries",.$table_title)) %>% 
  select(table_title, series_id) %>% 
  unique()

LF <- LA_DATA %>% 
  filter(grepl("Persons; Labour Account labour force",.$series)) %>%
  filter(series_type == "Seasonally Adjusted") %>%
  filter(data_type == "STOCK") %>%
  filter(!grepl("Revisions",.$table_title)) %>% 
  filter(!grepl("All Industries",.$table_title)) %>% 
  select(table_title, series_id) %>% 
  unique()

Unemployment <- LA_DATA %>% 
  filter(grepl("Persons; Labour Force Survey unemployed persons",.$series)) %>%
  filter(series_type == "Seasonally Adjusted") %>%
  filter(data_type == "STOCK") %>%
  filter(!grepl("Revisions",.$table_title)) %>% 
  filter(!grepl("All Industries",.$table_title)) %>% 
  select(table_title, series_id) %>% 
  unique()


ind_dat <- list()
for(i in seq_along(ind)){
  
  GVA_NAME <- paste0(ind[[i]],"_GVA")
  HRS_INC_NAME <- paste0(ind[[i]],"_WH")
  HOURS_NAME <- paste0(ind[[i]],"_HRS")
  GVAZ_NAME <- paste0(ind[[i]],"_GVAZ")
  PGVA_NAME <- paste0(ind[[i]],"_PGVA")
  VAC_NAME <- paste0(ind[[i]],"_VAC")
  LF_NAME <- paste0(ind[[i]],"_LF")
  UNE_NAME <- paste0(ind[[i]],"_UNE")
  
  
  
ind_dat[[ind[[i]]]]$ind_data <-   NA_DATA %>%
    filter(series_id == GVA[[i]]) %>%
    mutate(O_remove = forecast::tsclean(ts(.$value, f = 4)) ) %>% 
    #mutate(O_remove = tsoutliers::tso(ts(.$value, f = 4)) %>% .[[3]]) %>%
  filter(!is.na(O_remove)) %>% 
  mutate(Trend = stl(ts(O_remove, f = 4), s.window = "per", t.window = 12)$time.series[,2]) %>% 
  select(date,value, Trend) %>% 
    rename(Output = value) %>%
    left_join(NA_DATA %>% 
                filter(series_id == GVAZ$series_id[[i]]) %>% 
                select(date,value) %>% 
                rename(Outputz = value)) %>% 
    left_join(LA_DATA %>% 
                filter(table_no != "industry+summary+table") %>% 
                filter(series_id == Hours$series_id[[i]]) %>% 
                select(date,value) %>% 
                rename(Hours = value)) %>%
    left_join(LA_DATA %>% 
                filter(table_no != "industry+summary+table") %>% 
                filter(series_id == Hours_Inc$series_id[[i]]) %>% 
                select(date,value) %>% 
                rename(AENA_H = value)) %>%
  left_join(LA_DATA %>% 
              filter(table_no != "industry+summary+table") %>% 
              filter(series_id == LF$series_id[[i]]) %>% 
              select(date,value) %>% 
              rename(LF = value)) %>%
  left_join(LA_DATA %>% 
              filter(table_no != "industry+summary+table") %>% 
              filter(series_id == Unemployment$series_id[[i]]) %>% 
              select(date,value) %>% 
              rename(Unemployment = value)) %>%
  left_join(LA_DATA %>% 
              filter(table_no != "industry+summary+table") %>% 
              filter(series_id == Vacancies$series_id[[i]]) %>% 
              select(date,value) %>% 
              rename(Vac = value)) %>%
  left_join(LA_DATA %>% 
              filter(table_no != "industry+summary+table") %>% 
              filter(series_id == "A85389483J") %>% 
              select(date,value) %>% 
              rename(Hours_TOTAL = value)) %>%
  
  
  left_join(NA_DATA %>% 
              filter(table_title == "Table 5. Expenditure on Gross Domestic Product (GDP), Implicit price deflators") %>% 
              filter(series == "Households ;  Final consumption expenditure ;") %>% 
              filter(grepl("Seasonally", series_type)) %>%
              select(date,value) %>% 
              rename(PCONH = value)) %>%
    mutate(PGVA = Outputz/Output) %>%
    mutate(Vac_Rate = Vac/LF,
           UNE_Rate = Unemployment/LF,
           Hours_Share = Hours/Hours_TOTAL*100,
           YGAP = Output/Trend*100-100,
           Prod = Output/Hours,
           Mark_up = Prod/(AENA_H/PGVA)) %>%
  filter(!is.na(Mark_up)) %>% 
    mutate(O_remove = forecast::tsclean(ts(Mark_up, f = 4)) ) %>% 
  #mutate(O_remove = tsoutliers::tso(ts(.$value, f = 4)) %>% .[[3]]) %>%
  mutate(Trend_MU = stl(ts(O_remove, f = 4), s.window = "per", t.window = 12)$time.series[,2]) %>% 
  mutate(MGAP = Mark_up/Trend_MU*100-100)


# M = Pr-(A-PGVA) -> Pr-A+PGVA
# M+A-Pr
  

# Real wages and productivity  
ind_dat[[ind[[i]]]]$RW_PLOT <- ind_dat[[ind[[i]]]]$ind_data %>%  
  gather(Var, Val, -date) %>% 
    group_by(Var) %>% 
    mutate(Val = Val/Val[which(date == "2019-12-01")]*100) %>% 
    spread(Var, Val) %>% 
    mutate(Prod = Output/Hours*100,
           Prod_RW = AENA_H/PGVA*100,
           Cons_RW = AENA_H/PCONH*100) %>% 
    select(date, Prod_RW,Prod, Cons_RW) %>% 
    gather(Var, Val, -date) %>% 
    filter(!is.na(Val))  %>% 
  tst.plot_line(aes(date, Val, colour = Var))+
  ggtitle("Real wages and productivity")
  
# Beveridge curve
ind_dat[[ind[[i]]]]$BV_CURVE <- ind_dat[[ind[[i]]]]$ind_data %>%  
  select(date, Vac_Rate, UNE_Rate) %>% 
  mutate(Year = year(date)) %>%
  mutate(Year = ifelse(Year == 2023,"2023",
                       ifelse(Year == 2022,"2022",
                              ifelse(Year == 2021,"2021",
                                     ifelse(Year == 2020,"2020",
                                            ifelse(Year == 2024,"2024","All other years"
                                                   ))
                                     )
                              )
                       )
         ) %>% 
  ggplot(aes(UNE_Rate, Vac_Rate, colour = Year)) +
  geom_point(size = 4) + 
  tst_theme()+
  scale_colour_tst(discrete = T, palette = "mixed")+
  ggtitle("Beveridge curve")

# Employment share
ind_dat[[ind[[i]]]]$HRS_SHRE <- ind_dat[[ind[[i]]]]$ind_data %>%  
  select(date, Hours_Share) %>% 
  gather(Var, Val, -date) %>% 
  filter(!is.na(Val))  %>% 
  tst.plot_line(aes(date, Val, colour = Var))

  # Output gap
ind_dat[[ind[[i]]]]$YGAP <- ind_dat[[ind[[i]]]]$ind_data %>%  
  select(date, YGAP, MGAP,) %>%
  #mutate(Mark_up = Mark_up/lag(Mark_up,4)*100-100) %>%
  filter(!is.na(MGAP)) %>% 
  gather(Var, Val, -date) %>% 
  filter(!is.na(Val))  %>% 
  tst.plot_line(aes(date, Val, colour = Var))+geom_hline(yintercept =  0)+
  ggtitle("Mark up and output cycles")


ind_dat[[ind[[i]]]]$MU_REG <- ind_dat[[ind[[i]]]]$ind_data %>% 
  select(date, YGAP, MGAP) %>%
  #mutate(Mark_up = Mark_up/lag(Mark_up,4)*100-100) %>%
  filter(!is.na(MGAP))

ind_dat[[ind[[i]]]]$MU_REG <-   lm(ind_dat[[ind[[i]]]]$MU_REG$MGAP~ind_dat[[ind[[i]]]]$MU_REG$YGAP)

ind_dat[[ind[[i]]]]$PGVA_DECOMP_dat <-   ind_dat[[ind[[i]]]]$ind_data %>%  
  select(date, PGVA, Prod, AENA_H) %>%
  mutate(PGVA = PGVA/lag(PGVA,4)*100-100) %>%
  mutate(Prod = -1*(Prod/lag(Prod,4)*100-100)) %>%
  mutate(AENA_H = AENA_H/lag(AENA_H,4)*100-100) %>%
#  mutate(YGAP = YGAP*ind_dat[[ind[[i]]]]$MU_REG$coefficients[2]) %>%
  mutate(Mark_up_res = PGVA-AENA_H-Prod) %>% 
  filter(!is.na(Mark_up_res)) %>% 
  gather(Var, Val, -date) %>% 
  filter(!is.na(Val)) 

ind_dat[[ind[[i]]]]$PGVA_DECOMP_Chart <-  ind_dat[[ind[[i]]]]$PGVA_DECOMP_dat %>% 
    filter(Var != "PGVA") %>% 
    ggplot(aes(date, Val)) +
    geom_bar(aes(fill = Var),stat = "identity") +
    geom_line(data =ind_dat[[ind[[i]]]]$PGVA_DECOMP_dat %>% 
                filter(Var == "PGVA"), aes(date, Val), colour = "purple")+
    tst_theme()+
    scale_fill_tst(discrete = T)+
    ggtitle("Output price decomposition")


  
}

ind_for_chart <- "AFS"

(ind_dat[[ind_for_chart]]$RW_PLOT+ind_dat[[ind_for_chart]]$BV_CURVE)/(ind_dat[[ind_for_chart]]$YGAP+ind_dat[[ind_for_chart]]$PGVA_DECOMP_Chart)+
  plot_annotation(title = paste0("Drivers of inflation - ",ind_for_chart))




