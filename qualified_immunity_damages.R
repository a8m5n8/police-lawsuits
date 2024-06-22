library(dplyr)
library(readxl)
library(lubridate)

### Read in Mapping Police Violence data on police violence incidents
victims <- read_xlsx("~/police-lawsuits/MPVDatasetDownload.xlsx")

### Read in CAPstat data on New York police violence cases
nycases <- read_xlsx("~/police-lawsuits/capstat.xlsx") %>% 
  mutate(charges=wrongful_death+negligent_training+assault_battery
         +excessive_force+emotional_distress+deprive_due_process
         +deliberate_indifference+failure_to_intervene
         +ny_constitution_violation+municipal_liability+negligence
         +respondeat_superior+indiff_denial_medical_care
         +conspiracy+fabrication
         +false_arrest+malicious_prosecution+free_speech+supervisor_liability
         +denial_fair_trial+unlawful_entry+constitutional_tort+trespass
         +conversion+equal_protection_clause,
         black=ifelse(race %in% c("black"),1,0))

### Read in data on police budgets from Schwartz (2020)
budgets <- read_xlsx("~/police-lawsuits/police_litigation_budgets.xlsx") 
### Read in data on police lawsuit costs from Schwartz (2020)
costs <- read_xlsx("~/police-lawsuits/lawsuit_costs.xlsx") 

### Read in data on police lawsuit review policies from Schwartz (2020)
policies <- read_xlsx("~/police-lawsuits/lawsuit_review_policies.xlsx") 

### Calculate descriptive statistics for lawsuit payments
mean(budgets$avg_annual_lawsuit_pmt)
# $6,548,157
median(budgets$avg_annual_lawsuit_pmt)
# $1,226,214
smaller <- budgets %>% filter(avg_annual_lawsuit_pmt<1226214)
mean(smaller$avg_annual_lawsuit_pmt)
# $402,904.1

### Predictictors of probability of settlement
prsettle <- lm(settled~black+charges+no_officers+county,nycases)
summary(prsettle)

### Join New York case data onto police violence data to see share of incidents that result in litigation
main <- victims %>% filter(agency %in% "New York Police Department", date <= as.Date("2018-06-30"),
                           date >= as.Date("2015-01-01")) %>%
  full_join(nycases,by="name") %>% 
  mutate(lawsuit=if_else(is.na(case),0,1),
         anypaid=case_when((settled==1 & settlement>0) ~1, 
                           (settled==1 & is.na(settlement)) ~1,
                           settlement==0 ~0,
                           settled==0 ~0,
                           lawsuit==0 ~0))

### Robustness check for incident dates
main_sens <- victims %>% filter(agency %in% "New York Police Department", date <= as.Date("2018-07-31"),
                                date >= as.Date("2012-09-20")) %>%
  full_join(nycases,by="name") %>% 
  mutate(lawsuit=if_else(is.na(case),0,1),
         anypaid=case_when((settled==1 & settlement>0) ~1, 
                           (settled==1 & is.na(settlement)) ~1,
                           settlement==0 ~0,
                           settled==0 ~0,
                           lawsuit==0 ~0))

### Mean share of police violence incidents resulting in lawsuit and resulting in payment
mean(main$lawsuit)
t.test(main$lawsuit)
# 0.4
# 95 percent confidence interval:
# 0.2511552 0.5488448
mean(main$anypaid)
t.test(main$anypaid)
# 0.1555556
# 95 percent confidence interval:
# 0.04543808 0.26567303

### Robustness check for incident dates
mean(main_sens$lawsuit)
mean(main_sens$anypaid)


mainlawsuit <- main %>% filter(settled==1)
prwin <- mean(mainlawsuit$anypaid)
t.test(mainlawsuit$anypaid)
prwin_low <- 0.2561633
prwin_hi <- 0.9105034 

mainpaid <- mainlawsuit %>% filter(settlement>0)
avgpay <- mean(mainpaid$settlement)

### NYPD victims who didn't file lawsuits 1/2015 - 6/2018
nrow(main %>% filter(lawsuit==0))*prwin*avgpay
nrow(main %>% filter(lawsuit==0))*prwin_low*avgpay
nrow(main %>% filter(lawsuit==0))*prwin_hi*avgpay

### NYPD lawsuits that haven't settled yet
nrow(main %>% filter(settled==0))*prwin*avgpay
nrow(main %>% filter(settled==0))*prwin_low*avgpay
nrow(main %>% filter(settled==0))*prwin_hi*avgpay

### Projecting all NYPD lawsuits (all years)
main_noqi <- victims %>% filter(agency %in% "New York Police Department") %>%
  full_join(nycases,by="name") %>% 
  mutate(lawsuit=if_else(is.na(case),0,1),
         anypaid=case_when((settled==1 & settlement>0) ~1, 
                           (settled==1 & is.na(settlement)) ~1,
                           settlement==0 ~0,
                           settled==0 ~0,
                           lawsuit==0 ~0),
         inmain=if_else( date <= as.Date("2018-06-30")&
                         date >= as.Date("2015-01-01"),1,0)) %>% 
  filter(lawsuit==0)

### Total NYPD = this + actual payments + proj. pmt for pending cases
.6*nrow(main_noqi %>% filter(inmain==0))*prwin*avgpay + nrow(main_noqi %>% filter(inmain==1))*prwin*avgpay
.6*nrow(main_noqi %>% filter(inmain==0))*prwin_low*avgpay + nrow(main_noqi %>% filter(inmain==1))*prwin_low*avgpay
.6*nrow(main_noqi %>% filter(inmain==0))*prwin_hi*avgpay + nrow(main_noqi %>% filter(inmain==1))*prwin_hi*avgpay

### Sensitivity: 80% not 100% lawsuit rate
.8*.6*nrow(main_noqi %>% filter(inmain==0))*prwin*avgpay + .8*nrow(main_noqi %>% filter(inmain==1))*prwin*avgpay
.8*.6*nrow(main_noqi %>% filter(inmain==0))*prwin_low*avgpay + .8*nrow(main_noqi %>% filter(inmain==1))*prwin_low*avgpay
.8*.6*nrow(main_noqi %>% filter(inmain==0))*prwin_hi*avgpay + .8*nrow(main_noqi %>% filter(inmain==1))*prwin_hi*avgpay

### Non-NYC top 10 jurisdictions
### Counties: Los Angeles, Maricopa, Harris, Cook, Riverside, Dallas, San Bernardino, New York City, Clark
top10_noqi <- victims %>% filter(county %in% c("Los Angeles","Maricopa","Harris","Cook","Riverside","Dallas","San Bernardino","Clark")) %>% 
  group_by(agency,county,state) %>% 
  summarise(cases=n_distinct(id),
            tot_settle=.6*cases*prwin*avgpay,
            tot_settle_low=.6*cases*prwin_low*avgpay,
            tot_settle_hi=.6*cases*prwin_hi*avgpay,
            tot_settle_sens=.8*.6*cases*prwin*avgpay,
            tot_settle_low_sens=.8*.6*cases*prwin_low*avgpay,
            tot_settle_hi_sens=.8*.6*cases*prwin_hi*avgpay)

### County projections
rest_noqi <- victims %>% filter(!(agency %in% "New York Police Department")) %>% 
  group_by(county,state) %>% 
  summarise(cases=n_distinct(id),
            tot_settle=.6*cases*prwin*avgpay,
            tot_settle_low=.6*cases*prwin_low*avgpay,
            tot_settle_hi=.6*cases*prwin_hi*avgpay)

### Non-NYC projections
rest_noqi_wsens <- victims %>% filter(!(agency %in% "New York Police Department")) %>% 
  group_by(county,state) %>% 
  summarise(cases=n_distinct(id),
            tot_settle=.6*cases*prwin*avgpay,
            tot_settle_low=.6*cases*prwin_low*avgpay,
            tot_settle_hi=.6*cases*prwin_hi*avgpay,
            tot_settle_sens=.8*.6*cases*prwin*avgpay,
            tot_settle_low_sens=.8*.6*cases*prwin_low*avgpay,
            tot_settle_hi_sens=.8*.6*cases*prwin_hi*avgpay)

### Budget share
noqi <- victims %>% 
  group_by(agency,county,state) %>% 
  summarise(cases=n_distinct(id),
            tot_settle=.6*cases*prwin*avgpay,
            tot_settle_low=.6*cases*prwin_low*avgpay,
            tot_settle_hi=.6*cases*prwin_hi*avgpay,
            tot_settle_sens=.8*.6*cases*prwin*avgpay,
            tot_settle_low_sens=.8*.6*cases*prwin_low*avgpay,
            tot_settle_hi_sens=.8*.6*cases*prwin_hi*avgpay)
budget_share <- budgets %>% 
  rename(agency=law_enforcement_agency) %>% 
  left_join(noqi,by="agency")

budget_share_clean <- read.csv("~/police-lawsuits/budget_share.csv") %>%
  mutate(rel_budget_share = ifelse(!is.na(pct_agcy_lawsuit_total_budget),pct_agcy_lawsuit_total_budget,pct_total_budget_police_lawsuit),
         rel_budget=ifelse(!is.na(pct_agcy_lawsuit_total_budget),avg_agcy_total_budget,avg_juris_annual_budget),
         change_budget_share = (tot_settle/(rel_budget*7))/rel_budget_share,
         change_budget_share_low = (tot_settle_low/(rel_budget*7))/rel_budget_share,
         change_budget_share_hi = (tot_settle_hi/(rel_budget*7))/rel_budget_share,
         change_budget_share_sens = (tot_settle_sens/(rel_budget*7))/rel_budget_share,
         change_budget_share_sens_low = (tot_settle_low_sens/(rel_budget*7))/rel_budget_share,
         change_budget_share_sens_hi = (tot_settle_hi_sens/(rel_budget*7))/rel_budget_share)

### Litigation expenses
totalcosts <- budgets %>% 
  left_join(costs,by="jurisdiction") %>% 
  mutate(litcosts = ifelse(avglitigation %in% "Unavailable",3523329,
                           ifelse(is.na(avglitigation),3523329,
                                  as.numeric(avglitigation))
                              ),
         totalcosts= avg_annual_lawsuit_pmt + litcosts)

### average: $3,523,329
.6*3523329/.4


### Predictors of probability of damages awarded
damages <- lm(settlement~black+charges+no_officers+county.y+age,main)
summary(damages)



