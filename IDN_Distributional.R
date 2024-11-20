# Load Libraries ----
packages = c("tidyverse",
             "openxlsx",
             "ggthemes",
             "magrittr",
             "Matrix",
             "rsdmx",
             "hrbrthemes",
             "kableExtra",
             "viridis")


# Now load or install&load all
package.check = lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


# 1. Metadata --------- 
## 1.1. Sector List ------
products <- read.delim(paste0("Data/IOT_", 2019, "_pxp/products.txt", sep = ""), stringsAsFactors = FALSE) %>% 
  rename(sector = 2)

## 1.2. Country List -----
countries <- read.delim(paste0("Data/IOT_", 2019, "_pxp/unit.txt", sep = ""), stringsAsFactors = FALSE) %>% 
  select(region) %>% 
  distinct()

## 1.3. Country-Sector Combination------
# expected dimension: 163 * 49 = 7987 
order <- expand_grid(countries, products %>% select(-Number, -CodeTxt))

## Joining Region & Sector into a column to create unique values
region_codenr <- order %>% 
  select(region, CodeNr) %>% 
  unite("reg_CodeNr", region:CodeNr, sep = "_")

## Country with Full Name
region_full <- read.delim(paste0("Data/IOT_", 2019, "_pxp/region.txt"), stringsAsFactors = FALSE) %>% 
  set_colnames(c("region", "region_full")) %>% 
  
  left_join(countries, ., by = c("region"))

## List of electricity 12 sub-sectors
electricity_sectors <- products[128:139,] %>% 
  select(CodeNr) %>% 
  as_vector()

# 2. Data Preparation -------
## 2.1 final demand matrix; EXIOBASE 3.8.2 (million Euro) ---------
y <- read.delim(paste0("Y.txt", sep = ""), stringsAsFactors = FALSE) %>% 
  filter(!(region %in% c("category", "region"))) %>% # removing unused rows
  rename("sector" = "X") %>% 
  left_join(order, by = c("region", "sector")) %>% # joining sector Order data with sector code
  select(all_of((countries$region))) %>% # selecting only household final demand category
  sapply(., as.numeric) %>% # from character to numeric
  data.frame() %>% 
  select(ID) %>% 
  bind_cols(order, .)

y_gas_idn <- y %>% 
  filter(sector == "Motor Gasoline") %>% 
  rename(gas = 4)

y_fuel_idn <- y %>%
  filter(sector == "Liquefied Petroleum Gases (LPG)") %>% 
  rename(fuel = 4)

y_gasfuel_idn <- y_gas_idn %>% 
  bind_cols(y_fuel_idn %>% 
              select(fuel)) %>% 
  mutate(gasfuel = gas + fuel) %>% 
  select(-gas, -fuel)

y_gasfuel_join <- y %>% 
  mutate(ID = case_when(sector == "Motor Gasoline" ~ 0, sector != "Motor Gasoline" ~ ID),
         ID = case_when(sector == "Liquefied Petroleum Gases (LPG)" ~ 0, sector != "Liquefied Petroleum Gases (LPG)" ~ ID)) %>%
  left_join(., y_gasfuel_idn, by = c("region", "sector", "CodeNr")) %>% 
  mutate(gasfuel = replace_na(gasfuel, 0)) %>% 
  mutate(ID = ID + gasfuel) %>% 
  select(-gasfuel)


# total household final demand of Indonesia (200 x 1)
y_id <- y %>% 
  select(ID) %>% 
  as.matrix() %>% 
  set_rownames(order$CodeNr) %>% 
  rowsum(., row.names(.), reorder = FALSE) %>% 
  data.frame() %>% 
  select(ID) %>% 
  rename(FD = 1)

# Joining the final demand of liquefied petroleum gas to liquid fuel to form "fuel" product (private vehicle and cooking fuel)
y_id[67,] <- y_id[67,] + y_id[75,] # Joining the final demand of liquefied petroleum gas (row 75) to motor gasoline (row 67)
y_id[c(75),] <- 0 # setting the final demand of liquefied petroleum gas to 0

y_idn_share <- ((y_gasfuel_join %>% 
                   select(ID))/(do.call(rbind, replicate(49, y_id, simplify = FALSE)))) %>% 
  mutate(ID = replace_na(ID, 0)) %>% 
  as_vector()

## 2.2 gross output vector; EXIOBASE 3.8.2 (Million Euro) -----
x <- read.delim(paste0("Data/IOT_2019_pxp/x.txt", sep = ""), stringsAsFactors = FALSE) %>% 
  mutate(indout = as.numeric(indout)) %>% 
  right_join(order %>% 
               select(-CodeNr), ., by = c("region", "sector"))# Adding Country-Sector columns

x_gas_idn <- x %>% 
  filter(sector == "Motor Gasoline") %>% 
  rename(gas = 3)

x_fuel_idn <- x %>%
  filter(sector == "Liquefied Petroleum Gases (LPG)") %>% 
  rename(fuel = 3)

x_gasfuel_idn <- x_gas_idn %>% 
  bind_cols(x_fuel_idn %>% 
              select(fuel)) %>% 
  mutate(gasfuel = gas + fuel) %>% 
  select(-gas, -fuel)

x_gasfuel_join <- x %>% 
  mutate(indout = case_when(sector == "Motor Gasoline" ~ 0, sector != "Motor Gasoline" ~ indout),
         indout = case_when(sector == "Liquefied Petroleum Gases (LPG)" ~ 0, sector != "Liquefied Petroleum Gases (LPG)" ~ indout)) %>%
  left_join(., x_gasfuel_idn, by = c("region", "sector")) %>% 
  mutate(gasfuel = replace_na(gasfuel, 0)) %>% 
  mutate(indout = indout + gasfuel)

## 2.3 direct CO2 emissions vector; EXIOBASE 3.8.2 (kg CO2) ----------
f <- read.delim(paste0("Data/IOT_2019_pxp/satellite/F.txt", sep = ""), stringsAsFactors = FALSE, skip = 2) %>% 
  filter(grepl("CO2", stressor)) %>%  # retrieving rows containing relevant CO2 emissions categories
  select(-stressor) %>% 
  colSums() %>% 
  data.frame() %>% 
  rename(f = 1) %>% 
  bind_cols(order, .) %>% 
  left_join(x_gasfuel_join, by = c("region", "sector")) %>% # joining direct emissions with gross output
  select(region, sector, f, indout) %>% 
  mutate(f = case_when(indout == 0 ~ 0, indout != 0 ~ f))  # setting emissions value with 0 gross output to 0

f_gas_idn <- f %>% 
  filter(sector == "Motor Gasoline") %>% 
  rename(gas = 3)

f_fuel_idn <- f %>%
  filter(sector == "Liquefied Petroleum Gases (LPG)") %>% 
  rename(fuel = 3)

f_gasfuel_idn <- f_gas_idn %>% 
  bind_cols(f_fuel_idn %>% 
              select(fuel)) %>% 
  mutate(gasfuel = gas + fuel) %>% 
  select(-gas, -fuel)

f_gasfuel_join <- f %>% 
  mutate(f = case_when(sector == "Motor Gasoline" ~ 0, sector != "Motor Gasoline" ~ f),
         f = case_when(sector == "Liquefied Petroleum Gases (LPG)" ~ 0, sector != "Liquefied Petroleum Gases (LPG)" ~ f)) %>%
  left_join(., x_gasfuel_idn, by = c("region", "sector")) %>% 
  mutate(gasfuel = replace_na(gasfuel, 0)) %>% 
  mutate(f = f + gasfuel)

## 2.4 emissions intensity of economic output vector; q = f/x; EXIOBASE 3.8.2 (kg/million Euro) -------
q <- f_gasfuel_join %>% 
  mutate(q = (f)/indout, # calculating emissions intensity
         q = replace(q, is.na(q), 0)) %>%  # removing possible NA values
  select(q)

## 2.5 Leontief inverse matrix; EXIOBASE 3.8.2 ---------
L <- readRDS("Data/L.rds")

## 2.6 Household expenditure Survey Database ------
### mapping spreadsheet; EXIOBASE - SUSENAS (Indonesia household expenditure survey data) --------
mapping_EXIOBASE <- read.xlsx("Data/Mapping_IDN_EXIOBASE.xlsx") %>% 
  select(-N, -Product) %>% 
  t() %>% 
  data.frame()

CodeNr <- read.xlsx("Data/Mapping_IDN_EXIOBASE.xlsx") %>%  
  select(CodeNr) %>% 
  filter(!(CodeNr %in% c("Other", "Delete"))) %>% 
  as_vector()

names(mapping_EXIOBASE) <- mapping_EXIOBASE[1,]

mapping_EXIOBASE <- mapping_EXIOBASE[-1,]

mapping_EXIOBASE <- mapping_EXIOBASE %>% 
  gather(., "product", "cons_item", 1:202)

mapping_EXIOBASE$cons_item <- gsub(" ", "", mapping_EXIOBASE$cons_item)

mapping_EXIOBASE$cons_item <- paste("cons_annual", mapping_EXIOBASE$cons_item, sep = "")

mapping_EXIOBASE$cons_item <- gsub("cons_annualNA", NA, mapping_EXIOBASE$cons_item)

### loading Hh survey Data -------
Hh_IDN <- read.csv("Data/2019_data1.csv")

### dividing household annual expenditure of electricity based on the final demand share of electricity sub-sectors -------
y_elec_share <- y_id %>% 
  rownames_to_column() %>% 
  filter(rowname %in% electricity_sectors) %>% 
  mutate(sum = sum(FD, na.rm = TRUE),
         share = FD / sum) %>% 
  select(share) %>% 
  as_vector()

y_elec <- Hh_IDN %>% 
  select(cons_annual197) %>% 
  data.frame() %>% 
  rep(.$cons_annual197, 12) %>% 
  bind_cols() %>% 
  set_colnames(c(paste0("cons_annual197", letters[1:12]))) %>% 
  as.matrix()

y_elec_sub <- (t(t(y_elec) * (y_elec_share))) %>% 
  data.frame()

### joining the disaggregated household expenditure of electricity to the household expenditure data
Hh_IDN <- cbind(Hh_IDN %>% 
                  select(resp_id:cons_annual196), 
                y_elec_sub, 
                Hh_IDN %>% 
                  select(cons_annual198:X_merge))

### mapping and transforming Hh expenditure data to into EXIOBASE sector classification
list_product <- list()

for (i in CodeNr) {
  
  product <- mapping_EXIOBASE %>% 
    filter(product == i) %>%
    na.omit() %>% 
    select(cons_item) %>% 
    as_vector()
  
  list_product[[as.character(i)]] <- Hh_IDN %>% 
    select(all_of(product)) %>% 
    mutate(value = rowSums(., na.rm = TRUE)) %>% 
    select(value) %>% 
    set_colnames(c(i))
  
}

Hh_IDN_EXIOBASE <- bind_cols(list_product)

lpg_join <- Hh_IDN_EXIOBASE[,c(67, 75)] %>% 
  mutate(p23.20.a = p23.20.a + p23.20.i) %>% 
  select(p23.20.a)

Hh_IDN_EXIOBASE[,67] <- lpg_join

Hh_IDN_EXIOBASE[,c(75)] <- 0

rm(lpg_join)

#Mapping household survey expenditure data into exiobase classification
y_hh_map_exiobase <- data.frame(
  result = (mapply('*', Hh_IDN_EXIOBASE, y_idn_share))
  )
gc()

saveRDS(y_hh_map_exiobase, file = "Data/y_hh_map_exiobase.rds")
gc()

# 2.7 Emissions Embodied in Household's Expenditure --------
# I run the following process in partition to make this code executable without using high performance computing apparatus

# Define chunk size
chunk_size <- 1000
num_chunks <- ceiling(315672 / chunk_size)

# Initialize result matrix
e_hh_mapped <- matrix(0, nrow = 315672, ncol = 9800)

# Process in chunks
for (i in seq(1, 315672, by = chunk_size)) {
  end_idx <- min(i + chunk_size - 1, 315672)
  chunk_indices <- i:end_idx
  e_hh_mapped[chunk_indices, ] <- (as.matrix(y_hh_map_exiobase[chunk_indices,])%*%t(as_vector(q)*L))/(1000000*15589)
  gc()
}

rm(y_hh_map_exiobase)

e_hh_mapped12 <- bind_rows(readRDS("Data/e_hh_mapped1.rds"),
                          readRDS("Data/e_hh_mapped2.rds"))
 
gc()
 
e_hh_mapped34 <- bind_rows(readRDS("Data/e_hh_mapped3.rds"),
                          readRDS("Data/e_hh_mapped4.rds"))
 
gc()
 
e_hh_mapped <- bind_rows(e_hh_mapped12,e_hh_mapped34)
 
rm(e_hh_mapped12, e_hh_mapped34)
 
gc()

## 2.6.1 National + Imported scenario --------
e_hh_int <- Reduce(`+`, split.default(e_hh_mapped, (seq_along(e_hh_mapped) - 1) %/% 200)) %>% 
  set_colnames(c(products$CodeNr))

gc()

#e_hh_int <- readRDS("Data/e_hh_int.rds")

## 2.6.2 National carbon tax scenario --------
e_hh_nat <- e_hh_mapped[,8401:8600]

#e_hh_nat <- readRDS("Data/e_hh_nat.rds")


## 2.6.3 Electricity carbon tax scenario ---------
e_hh_elec <- e_hh_nat[,128:139]

#e_hh_elec <- readRDS("Data/e_hh_elec.rds")


## 2.6.4 Fuel carbon tax scenario --------
e_hh_fuel <- e_hh_nat %>% 
  data.frame() %>% 
  set_colnames(c(products$CodeNr)) %>% 
  select(p23.20.a, p23.20.e, p23.20.f, p23.20.i, p24.e,
         p60.1, p60.2, p60.3, p61.1, p61.2, p62, p63)

#e_hh_fuel <- readRDS("Data/e_hh_fuel.rds")


# Gathering analysis data
# total expenditure and survey weight data
expenditure <- Hh_IDN_EXIOBASE %>% 
  rowSums() %>% 
  data.frame() %>% 
  rename(consumption = 1) %>% 
  mutate(consumption = consumption) %>% # million local currency
  mutate(exp_quintile = ntile(consumption, 5),
         exp_decile = ntile(consumption, 10)) %>% 
  bind_cols(., Hh_IDN %>% 
              select(wert))


## 2.6 household direct emissions -----
# loading household direct emissions raw data (kg CO2)
HHE <- read.delim(paste0("Data/IOT_2019_pxp/satellite/F_Y.txt", sep = ""), stringsAsFactors = FALSE) %>% 
  select(region, all_of(countries$region)) %>% 
  filter(!(region %in% c("stressor", "category"))) %>% # removing unused rows
  rename("stressor" = "region") %>%
  filter(grepl("CO2", stressor)) %>% # getting rows containing relevant CO2 emissions categories
  select(-stressor) %>% 
  mutate(across(everything(), as.numeric)) %>% # converting character into numeric
  colSums() %>% 
  data.frame() %>% 
  rownames_to_column() %>% 
  rename(region = 1, HHE = 2) %>% 
  filter(region == "ID") %>% 
  select(HHE)
  
# preparing household direct emissions data of Indonesia
HHE_IDN <- Hh_IDN_EXIOBASE %>% 
  select(p23.20.a) %>% 
  mutate(share = p23.20.a/sum(p23.20.a)) %>% 
  select(share) %>% 
  bind_cols(., expenditure %>% 
              select(wert), HHE) %>% 
  mutate(s_wert = wert/sum(wert, na.rm = TRUE),
         e_hh_dir =  HHE / wert * share) %>% 
  select(e_hh_dir)

# Emissions embodied in households' main consumption items (tonne CO2)
e_hh_agg <- bind_cols(e_hh_nat %>% 
                        rowSums() %>% 
                        data.frame(), e_hh_int %>% 
                        rowSums() %>% 
                        data.frame(), e_hh_elec %>% 
                        rowSums() %>% 
                        data.frame(), e_hh_fuel %>% 
                        rowSums() %>% 
                        data.frame(),
                      HHE_IDN) %>% 
  set_colnames(c("national",
                 "national_import",
                 "electricity",
                 "lpg",
                 "e_hh_dir")) %>% 
  mutate(national = national + e_hh_dir,
         national_import = national_import + e_hh_dir,
         lpg = lpg + e_hh_dir) %>% 
  select(-e_hh_dir) %>% 
  divide_by(1000) 


# function to generate results
swf_decile <- function(tax, tax_scenario, transfer_scenario, percent_transfer){
  
  # tax rate and revenue
  welfare <- expenditure %>%
    group_by(exp_decile) %>% 
    mutate(v_wert = wert/sum(wert, na.rm = TRUE)) %>% #calculating share of households survey weight by quintile
    ungroup() %>% 
    bind_cols(., e_hh_agg %>%
                select(tax_scenario)) %>% # selecting tax scenario
    mutate(tax_rate = tax * 14860 , # tax rate in million IDR
           tax_emb_cons = tax_rate * eval(parse(text = paste0(tax_scenario))), # calculating tax embodied in expenditure of every household
           revenue = sum(tax_emb_cons, na.rm = TRUE), # measuring tax revenue
           uniform = revenue/nrow(.) * percent_transfer/100, # calculating the amount of uniform cash transfer received by every household
           poorest20 = case_when(exp_decile %in% c(1,2) ~ revenue / (nrow(.) * 0.2) * percent_transfer/100, # calculating the amount of cash transfer to poorest 20% of households
                                 TRUE ~ 0),
           poorest40 = case_when(exp_decile %in% c(1,2,3,4) ~ revenue / (nrow(.) * 0.4) * percent_transfer/100, # calculating the amount of cash transfer to poorest 20% of households
                                 TRUE ~ 0), 
           blt = 1500000, # $100 cash transfer for all households: government's BLT program
           no_transfer = 0, # no transfer
           net_impact = (- tax_emb_cons + eval(parse(text = paste0(transfer_scenario))))/consumption*v_wert) %>% # total expenditure after tax and cash transfer
    select(exp_decile, net_impact) %>%
    group_by(exp_decile) %>%
    mutate(net_impact = sum(net_impact, na.rm = TRUE)) %>%  # sum of relative net impact of carbon tax and cash transfer on household's expenditure by quintile, monetary
    ungroup() %>%
    distinct_all() %>%
    arrange(exp_decile)
  
  return(welfare)
  
}


# 4. Visualization -----------

# 4.1 Relative net impact of carbon tax (monetary) --------

df_plot <- swf_decile(tax = 40, # tax rate; input: numeric
                      tax_scenario = "national", # input: national, national_import, electricity, lpg
                      transfer_scenario = "no_transfer", #input: no_transfer, uniform, poorest20, poorest40, blt
                      percent_transfer = 100) %>% 
  select(exp_decile, net_impact) %>% 
  mutate(Transfer = "No transfer") %>% 
  bind_rows(.,
            swf_decile(tax = 40, # tax rate; input: numeric
                       tax_scenario = "national", # input: national, national_import, electricity, lpg
                       transfer_scenario = "uniform", #input: no_transfer, uniform, poorest20, poorest40, blt
                       percent_transfer = 100) %>% 
              select(exp_decile, net_impact) %>% 
              mutate(Transfer = "Uniform")) %>% 
  bind_rows(.,
            swf_decile(tax = 40, # tax rate; input: numeric
                       tax_scenario = "national", # input: national, national_import, electricity, lpg
                       transfer_scenario = "blt", #input: no_transfer, uniform, poorest20, poorest40, blt
                       percent_transfer = 100) %>% 
              select(exp_decile, net_impact) %>% 
              mutate(Transfer = "BLT")) %>% 
  mutate(Transfer = factor(Transfer, c("No transfer", "Uniform", "BLT")))




pdf("Article/Figures/New/6.0/net_impact.pdf", width = 4.5, height = 4)

# Plot
ggplot(df_plot, aes(exp_decile, net_impact, color = Transfer, shape = Transfer)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  geom_hline(yintercept=0, linewidth = 0.4, lty = 2) +
  scale_color_wsj() +
  scale_fill_solarized() +
  scale_shape_few() +
  scale_x_continuous(breaks = c(seq(from = 1, to = 10)), limits = c(1, 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_pander(base_family = "Times") +
  theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
        plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
        plot.caption = element_text(vjust = 4.5),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        panel.border = element_rect(color = "white"),
        panel.spacing.y = unit(1, "lines"),
        plot.margin = margin(1, 0, 1, 1, "cm"),
        axis.text.x = element_text(size = 10, vjust = -1.5),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -1.5),  # Adjusting x-axis text size
        axis.title.y = element_text(size = 10, vjust = 3)) +
  labs(y = "Relative net impact",
       x = "Income decile") 

dev.off()


# 4.2 Distribution of carbon tax in consumption items ----------
tax_emb_cons <- e_hh_nat %>%  # emissions embodied in consumption items (tonne)
  set_colnames(c(products$CodeNr)) %>% 
  bind_cols(HHE_IDN,.) %>% 
  mutate(p23.20.a = p23.20.a + e_hh_dir) %>% 
  select(-e_hh_dir) %>%  
  mutate(across(1:200, ~ . *40)) %>%  # 40 USD per tonne tax rate
  bind_cols(expenditure %>% 
              select(exp_decile, wert), .) %>% 
  group_by(exp_decile) %>% 
  mutate(v_wert = wert/sum(wert, na.rm = TRUE)) %>% #calculating share of households survey weight by quintile
  ungroup() %>% 
  select(-wert) %>% 
  transmute(foods = rowSums(select(., c(p01.a:p01.n), c(p15.a:p15.k))), across(-c(c(p01.a:p01.n), c(p15.a:p15.k)))) %>% 
  transmute(electricity = rowSums(select(., p40.11.a:p40.11.l)), across(-c(p40.11.a:p40.11.l))) %>% 
  #transmute(fuel = rowSums(select(., c(p23.20.a, p23.20.e, p23.20.f, p23.20.i, p24.e))), across(-c(p23.20.a, p23.20.e, p23.20.f, p23.20.i, p24.e))) %>% 
  transmute(fuel = rowSums(select(., c(p23.20.a, p23.20.e, p23.20.f, p23.20.i, p24.e,
                                       p60.1, p60.2, p60.3, p61.1, p61.2, p62, p63))), across(-c(p23.20.a, p23.20.e, p23.20.f, p23.20.i, p24.e,
                                                                                                 p60.1, p60.2, p60.3, p61.1, p61.2, p62, p63))) %>% 
  #transmute(other = rowSums(select(., c(5:162))), across(-c(5:162))) %>% 
  transmute(other = rowSums(select(., c(5:155))), across(-c(5:155))) %>% 
  group_by(exp_decile) %>% 
  summarise(across(everything(), ~ sum(. * v_wert, na.rm = TRUE)), .groups = 'drop') %>% 
  ungroup() %>% 
  select(-v_wert) %>% 
  set_colnames(c("exp_decile", "other", "fuel", "electricity", "foods")) %>% 
  pivot_longer(2:5, names_to = "cons_item", values_to = "tax_emb_cons") %>% 
  mutate(cons_item = factor(cons_item, c("electricity",
                                         "fuel",
                                         "foods",
                                         "other"))) %>% 
  group_by(exp_decile) %>% 
  mutate(tax_decile = tax_emb_cons/sum(tax_emb_cons)) %>% 
  ungroup()

pdf("Article/Figures/New/6.0/tax_emb_cons2.pdf", width = 5.5, height = 3.5)

print(
  ggplot(tax_emb_cons, aes(x = factor(exp_decile), y = tax_decile, fill = cons_item)) +
    geom_bar(stat = "identity", position = "stack", width = 0.5) +
    scale_fill_wsj() +
    labs(x = "Income decile", y = "Contribution to carbon tax", fill = "Consumption item") +
    #scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000), limits = c(0, 1000)) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_pander(base_family = "Times") + 
    theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
          plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
          plot.caption = element_text(vjust = 4.5),
          plot.background = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent"),
          legend.position = "none",
          legend.title = element_blank(),
          panel.border = element_rect(color = "white"),
          axis.title.x = element_text(size = 12, vjust = -2.5, color = "transparent"),
          axis.text.x = element_text(size = 12, vjust = -1.5),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 12))
)

dev.off()

# 4.3 Average Annual Expenditure --------------
cons <-  Hh_IDN_EXIOBASE %>% 
  data.frame() %>% 
  bind_cols(expenditure %>% 
              select(exp_decile, wert), .) %>% 
  group_by(exp_decile) %>% 
  mutate(v_wert = wert/sum(wert, na.rm = TRUE)) %>% #calculating share of households survey weight by quintile
  ungroup() %>% 
  select(-wert) %>% 
  transmute(foods = rowSums(select(., c(p01.a:p01.n), c(p15.a:p15.k))), across(-c(c(p01.a:p01.n), c(p15.a:p15.k)))) %>% 
  transmute(electricity = rowSums(select(., p40.11.a:p40.11.l)), across(-c(p40.11.a:p40.11.l))) %>% 
  #transmute(fuel = rowSums(select(., c(p23.20.a, p23.20.e, p23.20.f, p23.20.i, p24.e))), across(-c(p23.20.a, p23.20.e, p23.20.f, p23.20.i, p24.e))) %>% 
  transmute(fuel = rowSums(select(., c(p23.20.a, p23.20.e, p23.20.f, p23.20.i, p24.e,
                                       p60.1, p60.2, p60.3, p61.1, p61.2, p62, p63))), across(-c(p23.20.a, p23.20.e, p23.20.f, p23.20.i, p24.e,
                                                                                                 p60.1, p60.2, p60.3, p61.1, p61.2, p62, p63))) %>% 
  #transmute(other = rowSums(select(., c(5:162))), across(-c(5:162))) %>% 
  transmute(other = rowSums(select(., c(5:155))), across(-c(5:155))) %>% 
  group_by(exp_decile) %>% 
  summarise(across(everything(), ~ sum(. * v_wert, na.rm = TRUE)), .groups = 'drop') %>% 
  ungroup() %>% 
  select(-v_wert) %>% 
  pivot_longer(2:5, names_to = "cons_item", values_to = "cons") %>% 
  mutate(cons = cons/14680) %>% 
  mutate(cons_item = factor(cons_item, c("electricity",
                                         "fuel",
                                         "foods",
                                         "other")))

cons_share <- cons %>% 
  group_by(exp_decile) %>% 
  mutate(cons_share = cons/sum(cons, na.rm = TRUE)) %>% 
  ungroup()

pdf("Article/Figures/New/6.0/exp_mean.pdf", width = 5.5, height = 4)

print(
  ggplot(cons_share, aes(x = factor(exp_decile), y = cons_share, fill = cons_item)) +
    geom_bar(stat = "identity", position = "fill", width = 0.5) +
    scale_fill_wsj() +
    labs(x = "Income decile", y = "Contribution to expenditure", fill = "Consumption item") +
    #scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000), limits = c(0, 9000)) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_pander(base_family = "Times") + 
    theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
          plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
          plot.caption = element_text(vjust = 4.5),
          plot.background = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent"),
          legend.position = "bottom",
          legend.title = element_blank(),
          panel.border = element_rect(color = "white"),
          panel.spacing.y = unit(1, "lines"),
          axis.title.x = element_text(size = 12, vjust = -2.5),
          axis.text.x = element_text(size = 12, vjust = -1.5),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 12))
)

dev.off()

# 4.4 Relative Net Impact; Other Carbon Taxation Scenarios ----
list_df_scenario <- list()
list_df_rate <- list()

tax_scenario <- c("national", "national_import", "electricity", "lpg")
tax_rate <- c(2, 40, 100, 120)

for (i in tax_rate){
  for (j in tax_scenario){
    
    list_df_scenario[[as.character(j)]] <- swf_decile(tax = i, # tax rate; input: numeric
                                                      tax_scenario = j, # input: national, national_import, electricity, lpg
                                                      transfer_scenario = "no_transfer", #input: no_transfer, uniform, poorest20, poorest40, blt
                                                      percent_transfer = 100) %>% 
      select(exp_decile, net_impact) %>% 
      mutate(Transfer = "No transfer") %>% 
      bind_rows(.,
                swf_decile(tax = i, # tax rate; input: numeric
                           tax_scenario = j, # input: national, national_import, electricity, lpg
                           transfer_scenario = "uniform", #input: no_transfer, uniform, poorest20, poorest40, blt
                           percent_transfer = 100) %>% 
                  select(exp_decile, net_impact) %>% 
                  mutate(Transfer = "Uniform")) %>% 
      bind_rows(.,
                swf_decile(tax = i, # tax rate; input: numeric
                           tax_scenario = j, # input: national, national_import, electricity, lpg
                           transfer_scenario = "blt", #input: no_transfer, uniform, poorest20, poorest40, blt
                           percent_transfer = 100) %>% 
                  select(exp_decile, net_impact) %>% 
                  mutate(Transfer = "BLT")) %>% 
      mutate(Transfer = factor(Transfer, c("No transfer", "Uniform", "BLT")),
             Scenario = j)
  }
  
  list_df_rate[[as.character(i)]] <- bind_rows(list_df_scenario) %>% 
    mutate(tax_rate = as.character(i))
  
}


netimp_all <- bind_rows(list_df_rate) %>% 
  mutate(Scenario = case_when(Scenario == "national" ~ "National Economy-wide",
                              Scenario == "national_import" ~ "National + Import Economy-wide",
                              Scenario == "electricity" ~ "National Electricity-only",
                              Scenario == "lpg" ~ "National Fuel-only")) %>% 
  mutate(Scenario = factor(Scenario, c("National Economy-wide", "National + Import Economy-wide", "National Electricity-only", "National Fuel-only")))


# $2 tax rate
pdf("Article/Figures/New/6.0/net_impact_all_2.pdf", width = 6, height = 8)

# Plot
ggplot(netimp_all %>% 
         filter(tax_rate == 2) , aes(exp_decile, net_impact, color = Transfer, shape = Transfer)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  geom_hline(yintercept=0, linewidth = 0.4, lty = 2) +
  scale_color_wsj() +
  scale_fill_solarized() +
  scale_shape_few() +
  scale_x_continuous(breaks = c(seq(from = 1, to = 10)), limits = c(1, 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_pander(base_family = "Times") +
  facet_wrap(~Scenario, scales = "free", ncol = 2, labeller = labeller(Scenario = label_wrap_gen(10))) +
  theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
        plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
        plot.caption = element_text(vjust = 4.5),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.border = element_rect(color = "white"),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.5, "lines"),
        plot.margin = margin(1, 0, 1, 1, "cm"),
        axis.text.x = element_text(size = 10, vjust = -1.5),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 11, vjust = -3),  # Adjusting x-axis text size
        axis.title.y = element_text(size = 11, vjust = 3)) +
  labs(y = "Relative net impact",
       x = "Income decile") 

dev.off()




# $40 tax rate
pdf("Article/Figures/New/6.0/net_impact_all_40.pdf", width = 6, height = 8)

# Plot
ggplot(netimp_all %>% 
         filter(tax_rate == 40) , aes(exp_decile, net_impact, color = Transfer, shape = Transfer)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  geom_hline(yintercept=0, linewidth = 0.4, lty = 2) +
  scale_color_wsj() +
  scale_fill_solarized() +
  scale_shape_few() +
  scale_x_continuous(breaks = c(seq(from = 1, to = 10)), limits = c(1, 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_pander(base_family = "Times") +
  facet_wrap(~Scenario, scales = "free", ncol = 2, labeller = labeller(Scenario = label_wrap_gen(10))) +
  theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
        plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
        plot.caption = element_text(vjust = 4.5),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.border = element_rect(color = "white"),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.5, "lines"),
        plot.margin = margin(1, 0, 1, 1, "cm"),
        axis.text.x = element_text(size = 10, vjust = -1.5),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 11, vjust = -3),  # Adjusting x-axis text size
        axis.title.y = element_text(size = 11, vjust = 3)) +
  labs(y = "Relative net impact",
       x = "Income decile") 

dev.off()




# $100 tax rate
pdf("Article/Figures/New/6.0/net_impact_all_100.pdf", width = 6, height = 8)

# Plot
ggplot(netimp_all %>% 
         filter(tax_rate == 100) , aes(exp_decile, net_impact, color = Transfer, shape = Transfer)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  geom_hline(yintercept=0, linewidth = 0.4, lty = 2) +
  scale_color_wsj() +
  scale_fill_solarized() +
  scale_shape_few() +
  scale_x_continuous(breaks = c(seq(from = 1, to = 10)), limits = c(1, 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_pander(base_family = "Times") +
  facet_wrap(~Scenario, scales = "free", ncol = 2, labeller = labeller(Scenario = label_wrap_gen(10))) +
  theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
        plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
        plot.caption = element_text(vjust = 4.5),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.border = element_rect(color = "white"),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.5, "lines"),
        plot.margin = margin(1, 0, 1, 1, "cm"),
        axis.text.x = element_text(size = 10, vjust = -1.5),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 11, vjust = -3),  # Adjusting x-axis text size
        axis.title.y = element_text(size = 11, vjust = 3)) +
  labs(y = "Relative net impact",
       x = "Income decile") 

dev.off()



# $120 tax rate
pdf("Article/Figures/New/6.0/net_impact_all_120.pdf", width = 6, height = 8)

# Plot
ggplot(netimp_all %>% 
         filter(tax_rate == 120) , aes(exp_decile, net_impact, color = Transfer, shape = Transfer)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  geom_hline(yintercept=0, linewidth = 0.4, lty = 2) +
  scale_color_wsj() +
  scale_fill_solarized() +
  scale_shape_few() +
  scale_x_continuous(breaks = c(seq(from = 1, to = 10)), limits = c(1, 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_pander(base_family = "Times") +
  facet_wrap(~Scenario, scales = "free", ncol = 2, labeller = labeller(Scenario = label_wrap_gen(10))) +
  theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
        plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
        plot.caption = element_text(vjust = 4.5),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        panel.border = element_rect(color = "white"),
        panel.spacing.x = unit(1.5, "lines"),
        panel.spacing.y = unit(1.5, "lines"),
        plot.margin = margin(1, 0, 1, 1, "cm"),
        axis.text.x = element_text(size = 10, vjust = -1.5),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 11, vjust = -3),  # Adjusting x-axis text size
        axis.title.y = element_text(size = 11, vjust = 3)) +
  labs(y = "Relative net impact",
       x = "Income decile") 

dev.off()



# 4.5 Stress Test ------------
fun_stest <- function(scene){
  
  tax_rate <- c(2, 40, 100, 120)
  
  percent_trf <- c(0, 25, 75, 100)
  
  list_df_trf <- list()
  list_df_tax <- list()
  
  for (i in tax_rate){
    for (j in percent_trf){
      
      list_df_trf[[as.character(j)]] <- swf_decile(tax = i, # tax rate; input: numeric
                                                   tax_scenario = scene, # input: national, national_import, electricity, lpg
                                                   transfer_scenario = "uniform", #input: no_transfer, uniform, poorest20, poorest40, blt
                                                   percent_transfer = j) %>% 
        select(exp_decile, net_impact) %>% 
        mutate(percent_trf = as.character(j))
      
      
    }
    
    list_df_tax[[as.character(i)]]  <- bind_rows(list_df_trf) %>% 
      mutate(tax_rate = as.character(i))
    
  }
  
  df_plot <- bind_rows(list_df_tax) %>% 
    mutate(tax_rate = case_when(tax_rate == "2" ~ "$2/tonne",
                                tax_rate == "40" ~ "$40/tonne",
                                tax_rate == "100" ~ "$100/tonne",
                                tax_rate == "120" ~ "$120/tonne")) %>% 
    mutate(percent_trf = factor(percent_trf, c("0", "25", "75", "100")),
           tax_rate = factor(tax_rate, c("$2/tonne", "$40/tonne", "$100/tonne", "$120/tonne"))) %>% 
    rename("Recycling" = "percent_trf")
  
  return(df_plot)
  
}


ggplot_stest <- function(x) {
  
  ggplot(x, aes(exp_decile, net_impact, color = Recycling, shape = Recycling)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.5) +
    geom_hline(yintercept=0, linewidth = 0.4, lty = 2) +
    scale_color_wsj() +
    scale_fill_solarized() +
    scale_shape_few() +
    scale_x_continuous(breaks = c(seq(from = 1, to = 10)), limits = c(1, 10)) +
    scale_y_continuous(labels = scales::percent_format()) +
    theme_pander(base_family = "Times") +
    facet_wrap(~tax_rate, scales = "free", ncol = 2) +
    theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
          plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
          plot.caption = element_text(vjust = 4.5),
          plot.background = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent"),
          legend.position = "bottom",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9),
          panel.border = element_rect(color = "white"),
          panel.spacing.x = unit(1.5, "lines"),
          panel.spacing.y = unit(1.5, "lines"),
          plot.margin = margin(1, 0, 1, 1, "cm"),
          axis.text.x = element_text(size = 10, vjust = -1.5),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 11, vjust = -3),  # Adjusting x-axis text size
          axis.title.y = element_text(size = 11, vjust = 3)) +
    labs(y = "Relative net impact",
         x = "Income decile",
         color = "Revenue Recycled (%)",
         shape = "Revenue Recycled (%)") 
  
  
}


## national: % recycling and tax rate
df_plot <- fun_stest(scene = "national") 

# Plot
pdf("Article/Figures/New/6.0/stest_new_nat.pdf", width = 6, height = 8)
ggplot_stest(df_plot)
dev.off()

## national + import: % recycling and tax rate
df_plot <- fun_stest(scene = "national_import")

# Plot
pdf("Article/Figures/New/6.0/stest_new_natimp.pdf", width = 6, height = 8)
ggplot_stest(df_plot)
dev.off()

## national electricity: % recycling and tax rate
df_plot <- fun_stest(scene = "electricity")

# Plot
pdf("Article/Figures/New/6.0/stest_new_elec.pdf", width = 6, height = 8)
ggplot_stest(df_plot)
dev.off()

## national fuel: % recycling and tax rate
df_plot <- fun_stest(scene = "lpg")

# Plot
pdf("Article/Figures/New/6.0/stest_new_fuel.pdf", width = 6, height = 8)
ggplot_stest(df_plot)
dev.off()


# Uniform cash transfer
fun_stest <- function(scene){
  
  tax_rate <- c(2, 40, 100, 120)
  
  percent_trf <- c(0, 25, 75, 100)
  
  list_df_trf <- list()
  list_df_tax <- list()
  
  for (i in tax_rate){
    for (j in percent_trf){
      
      list_df_trf[[as.character(j)]] <- swf_decile(tax = i, # tax rate; input: numeric
                                                   tax_scenario = scene, # input: national, national_import, electricity, lpg
                                                   transfer_scenario = "poorest40", #input: no_transfer, uniform, poorest20, poorest40, blt
                                                   percent_transfer = j) %>% 
        select(exp_decile, net_impact) %>% 
        mutate(percent_trf = as.character(j))
      
      
    }
    
    list_df_tax[[as.character(i)]]  <- bind_rows(list_df_trf) %>% 
      mutate(tax_rate = as.character(i))
    
  }
  
  df_plot <- bind_rows(list_df_tax) %>% 
    mutate(tax_rate = case_when(tax_rate == "2" ~ "$2/tonne",
                                tax_rate == "40" ~ "$40/tonne",
                                tax_rate == "100" ~ "$100/tonne",
                                tax_rate == "120" ~ "$120/tonne")) %>% 
    mutate(percent_trf = factor(percent_trf, c("0", "25", "75", "100")),
           tax_rate = factor(tax_rate, c("$2/tonne", "$40/tonne", "$100/tonne", "$120/tonne"))) %>% 
    rename("Recycling" = "percent_trf")
  
  return(df_plot)
  
}

df_join <- fun_stest(scene = "national") %>% 
  mutate(scenario = "National") %>% 
  bind_rows(., fun_stest(scene = "electricity") %>% 
              mutate(scenario = "National Electricity")) %>% 
  filter(exp_decile %in% c(1,2,3,4)) %>%
  filter(tax_rate == "$40/tonne") %>% 
  mutate(scenario = factor(scenario, c("National", "National Electricity"))) %>% 
  mutate(net_impact = net_impact * 100)

pdf("Article/Figures/New/6.0/stest_nat.pdf", width = 8.5, height = 4)

ggplot(df_join, aes(exp_decile, Recycling, fill= net_impact)) + 
  geom_tile() +
  geom_text(data = df_join, aes(label = round(net_impact, 2), color = ifelse(net_impact <= 80, "white", "black")),
            family = "Times")+
  scale_fill_viridis(discrete=FALSE, option = "magma") +
  scale_color_manual(values = c("black", "white")) +
  theme_pander() + 
  facet_wrap(~scenario, scales = "fixed", ncol = 2) +
  theme(text = element_text(family = "Times"),
        axis.title.x = element_text(vjust = -2),
        plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
        plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
        plot.caption = element_text(vjust = 4.5),
        plot.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "none",
        legend.title = element_blank(),
        panel.border = element_rect(color = "white"),
        panel.spacing.y = unit(1, "lines"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        strip.text.x = element_text(size = 14)) +
  labs(y = "% Recycling",
       x = "Income decile") 

dev.off()


# 5. Tables ------------------------
# 5.1 Relative net impact of a $40 carbon tax under all tax scenarios and three transfer options -------

list_df_scenario <- list()
list_df_rate <- list()

tax_scenario <- c("national", "national_import", "electricity", "lpg")
tax_rate <- c(2, 40, 100, 120)

for (i in tax_rate){
  for (j in tax_scenario){
    
    list_df_scenario[[as.character(j)]] <- swf_decile(tax = i, # tax rate; input: numeric
                                                      tax_scenario = j, # input: national, national_import, electricity, lpg
                                                      transfer_scenario = "no_transfer", #input: no_transfer, uniform, poorest20, poorest40, blt
                                                      percent_transfer = 100) %>% 
      select(exp_decile, net_impact) %>% 
      mutate(net_impact = net_impact *100) %>% 
      mutate(Transfer = "No transfer") %>% 
      bind_rows(.,
                swf_decile(tax = i, # tax rate; input: numeric
                           tax_scenario = j, # input: national, national_import, electricity, lpg
                           transfer_scenario = "uniform", #input: no_transfer, uniform, poorest20, poorest40, blt
                           percent_transfer = 100) %>% 
                  select(exp_decile, net_impact) %>% 
                  mutate(net_impact = net_impact *100) %>% 
                  mutate(Transfer = "Uniform")) %>% 
      bind_rows(.,
                swf_decile(tax = i, # tax rate; input: numeric
                           tax_scenario = j, # input: national, national_import, electricity, lpg
                           transfer_scenario = "blt", #input: no_transfer, uniform, poorest20, poorest40, blt
                           percent_transfer = 100) %>% 
                  select(exp_decile, net_impact) %>%
                  mutate(net_impact = net_impact *100) %>%
                  mutate(Transfer = "BLT")) %>% 
      mutate(Transfer = factor(Transfer, c("No transfer", "Uniform", "BLT")),
             Scenario = j)
  }
  
  list_df_rate[[as.character(i)]] <- bind_rows(list_df_scenario) %>% 
    mutate(tax_rate = as.character(i))
  
}


tab_netimp_all <- bind_rows(list_df_rate) %>% 
  mutate(Scenario = case_when(Scenario == "national" ~ "National",
                              Scenario == "national_import" ~ "National + Import",
                              Scenario == "electricity" ~ "National Electricity",
                              Scenario == "lpg" ~ "National Fuel")) %>% 
  mutate(Scenario = factor(Scenario, c("National", "National + Import", "National Electricity", "National Fuel"))) %>% 
  unite("transfer_taxscene", Transfer:Scenario, sep = "_") %>% 
  pivot_wider(., names_from = transfer_taxscene, values_from = net_impact) 


# $2 tax rate
print(
  kable(tab_netimp_all %>% 
          filter(tax_rate == 2) %>% 
          select(-tax_rate) %>% 
          set_colnames(c("Income decile", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT")), 
        digits = 3,
        caption = "",
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/6.0/netimp_all_2.tex")

# $40 tax rate
print(
  kable(tab_netimp_all %>% 
          filter(tax_rate == 40) %>% 
          select(-tax_rate) %>% 
          set_colnames(c("Income decile", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT")), 
        digits = 2,
        caption = "",
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/netimp_all_40.tex")

# $100 tax rate
print(
  kable(tab_netimp_all %>% 
          filter(tax_rate == 100) %>% 
          select(-tax_rate) %>% 
          set_colnames(c("Income decile", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT")), 
        digits = 2,
        caption = "",
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/netimp_all_100.tex")

# $120 tax rate
print(
  kable(tab_netimp_all %>% 
          filter(tax_rate == 120) %>% 
          select(-tax_rate) %>% 
          set_colnames(c("Income decile", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT", "No Transfer", "Uniform", "BLT")), 
        digits = 2,
        caption = "",
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/netimp_all_120.tex")

# 5.2 Products contribution to relative carbon tax ------------
tab_inc_sect <- e_hh_nat %>%  # emissions embodied in consumption items (tonne)
  set_colnames(c(products$CodeNr)) %>% 
  bind_cols(HHE_IDN,.) %>% 
  mutate(p23.20.a = p23.20.a + e_hh_dir) %>% 
  select(-e_hh_dir) %>%  
  # t(as_vector(agg_f_fd$f_nat) * t(as.matrix(Hh_IDN_EXIOBASE))) %>% # emissions embodied in consumption items (tonne)
  # data.frame() %>% 
  bind_cols(expenditure %>% 
              select(exp_decile, wert, consumption), .) %>% 
  mutate(across(4:203, ~ . *(40*14860)/(consumption))) %>% 
  group_by(exp_decile) %>% 
  mutate(v_wert = wert/sum(wert, na.rm = TRUE)) %>% #calculating share of households survey weight by quintile
  ungroup() %>% 
  select(-wert) %>% 
  group_by(exp_decile) %>% 
  summarise(across(everything(), ~ sum(. * v_wert, na.rm = TRUE)), .groups = 'drop') %>% 
  ungroup() %>% 
  select(-v_wert, -exp_decile, -consumption) %>% 
  set_colnames(c(CodeNr)) %>% 
  t() %>% 
  set_colnames(c(paste0("Q", 1:10))) %>% 
  bind_cols(products, .) %>% 
  arrange(-Q1) %>% 
  select(-Number, -CodeTxt, -CodeNr) %>% 
  rename(Product = 1)

elec_sum <- tab_inc_sect %>% 
  filter(grepl("Electricity", Product)) %>% 
  select(Q1:Q10) %>% 
  colSums() %>% 
  t() %>% 
  data.frame() %>% 
  bind_cols(data.frame(Product = "Electricity"), .)

inc_tot <- data.frame(Product = "Relative carbon tax (%)", tab_inc_sect %>% 
                        select(-Product) %>% 
                        colSums() %>% 
                        t()) %>% 
  set_colnames(c("Product", (paste0("Q", 1:10)))) %>% 
  as_tibble()

tab_inc_sect <- tab_inc_sect %>% 
  filter(!grepl("Electricity", Product)) %>% 
  bind_rows(. , elec_sum) %>% 
  arrange(-Q1) %>% 
  top_n(., 10) %>% 
  bind_rows(inc_tot, .) %>% 
  as_tibble() %>% 
  mutate(across(2:11, ~ . * 100 /1000)) 

tab_inc_sect[,2:11] <- round(tab_inc_sect[,2:11], 2) %>% 
  as_tibble()

print(
  kable(tab_inc_sect, 
        digits = 2,
        caption = "",
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/inc_sector_cont.tex")



# 5.3 Tax embodied in products ------
tax_emb_prod <- e_hh_nat %>%  # emissions embodied in consumption items (tonne)
  set_colnames(c(products$CodeNr)) %>% 
  bind_cols(HHE_IDN,.) %>% 
  mutate(p23.20.a = p23.20.a + e_hh_dir) %>% 
  select(-e_hh_dir) %>%  
  # t(as_vector(agg_f_fd$f_nat) * t(as.matrix(Hh_IDN_EXIOBASE))) %>% # emissions embodied in consumption items (tonne)
  # data.frame() %>% 
  bind_cols(expenditure %>% 
              select(exp_decile, wert, consumption), .) %>% 
  mutate(across(4:203, ~ . *(40)/1000)) %>% 
  group_by(exp_decile) %>% 
  mutate(v_wert = wert/sum(wert, na.rm = TRUE)) %>% #calculating share of households survey weight by quintile
  ungroup() %>% 
  select(-wert) %>% 
  group_by(exp_decile) %>% 
  summarise(across(everything(), ~ sum(. * v_wert, na.rm = TRUE)), .groups = 'drop') %>% 
  ungroup() %>% 
  select(-v_wert, -exp_decile, -consumption) %>% 
  set_colnames(c(CodeNr)) %>% 
  t() %>% 
  set_colnames(c(paste0("Q", 1:10))) %>% 
  bind_cols(products, .) %>% 
  arrange(-Q1) %>% 
  select(-Number, -CodeTxt, -CodeNr) %>% 
  rename(Product = 1)

elec_sum <- tax_emb_prod %>% 
  filter(grepl("Electricity", Product)) %>% 
  select(Q1:Q10) %>% 
  colSums() %>% 
  t() %>% 
  data.frame() %>% 
  bind_cols(data.frame(Product = "Electricity"), .)

tax_emb_prod_abs <- tax_emb_prod %>% 
  filter(!grepl("Electricity", Product)) %>% 
  bind_rows(., elec_sum) %>% 
  arrange(-Q1) %>% 
  top_n(., 10)

tax_emb_prod_abs[,2:11] <- round(tax_emb_prod_abs[,2:11], 1) %>% 
  as_tibble()

print(
  kable(tax_emb_prod_abs, 
        digits = 1,
        caption = "",
        format="latex", 
        row.names = FALSE,
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/tax_emb_prod.tex"
)

# Share of tax embodied in products
tax_emb_prod_share <- tax_emb_prod %>% 
  filter(!grepl("Electricity", Product)) %>% 
  bind_rows(., elec_sum) %>% 
  pivot_longer(., 2:11, names_to = "decile", values_to = "share_tax") %>% 
  group_by(decile) %>% 
  mutate(share_tax = share_tax/sum(share_tax, na.rm = TRUE)*100) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "decile", values_from = "share_tax") %>% 
  arrange(-Q1) %>% 
  top_n(., 10)


tax_emb_prod_share[,2:11] <- round(tax_emb_prod_share[,2:11], 1) %>% 
  as_tibble()


print(
  kable(tax_emb_prod_share, 
        digits = 1,
        caption = "",
        format="latex", 
        row.names = FALSE,
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/tax_emb_prod_share.tex"
)

# Electricity sub-sectors' contribution to carbon tax
elec_cont_tax <- tax_emb_prod %>% 
  pivot_longer(., 2:11, names_to = "decile", values_to = "share_tax") %>% 
  group_by(decile) %>% 
  mutate(share_tax = share_tax/sum(share_tax, na.rm = TRUE)*100) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "decile", values_from = "share_tax") %>% 
  filter(grepl("Electricity", Product)) %>% 
  arrange(-Q1) %>% 
  add_row(.before = 1, Product = "Electricity total", !!!colSums(select(., -1)))


elec_cont_tax[,2:11] <- round(elec_cont_tax[,2:11], 1) %>% 
  as_tibble()


print(
  kable(elec_cont_tax, 
        digits = 1,
        caption = "",
        format="latex", 
        row.names = FALSE,
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/tax_emb_prod_share.tex"
)

# 5.5 Expenditures ----------- 
exp_prod <-  Hh_IDN_EXIOBASE %>% 
  data.frame() %>% 
  bind_cols(expenditure %>% 
              select(exp_decile, wert, consumption), .) %>% 
  mutate(across(4:203, ~ . /14860)) %>% 
  group_by(exp_decile) %>% 
  mutate(v_wert = wert/sum(wert, na.rm = TRUE)) %>% #calculating share of households survey weight by quintile
  ungroup() %>% 
  select(-wert) %>% 
  group_by(exp_decile) %>% 
  summarise(across(everything(), ~ sum(. * v_wert, na.rm = TRUE)), .groups = 'drop') %>% 
  ungroup() %>% 
  select(-v_wert, -exp_decile, -consumption) %>% 
  set_colnames(c(CodeNr)) %>% 
  t() %>% 
  set_colnames(c(paste0("Q", 1:10))) %>% 
  bind_cols(products, .) %>% 
  arrange(-Q1) %>% 
  select(-Number, -CodeTxt, -CodeNr) %>% 
  rename(Product = 1)

elec_sum <- exp_prod %>% 
  filter(grepl("Electricity", Product)) %>% 
  select(Q1:Q10) %>% 
  colSums() %>% 
  t() %>% 
  data.frame() %>% 
  bind_cols(data.frame(Product = "Electricity"), .)

exp_prod <- exp_prod %>% 
  filter(!grepl("Electricity", Product)) %>% 
  bind_rows(., elec_sum) %>% 
  arrange(-Q1) %>% 
  top_n(., 20)


exp_prod[,2:6] <- round(exp_prod[,2:6], 1) %>% 
  as_tibble()

print(
  kable(exp_prod, 
        digits = 1,
        caption = "",
        format="latex", 
        row.names = FALSE,
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/exp_prod.tex"
)

# 4 sectors monetary
cons_monetary <- cons %>% 
  pivot_wider(names_from = "exp_decile", values_from = "cons") %>% 
  set_colnames(c("Product", paste0("Q", 1:10))) %>% 
  arrange(Product) %>% 
  mutate(Product = factor(Product, c("electricity",
                                     "fuel",
                                     "foods",
                                     "other")))


cons_monetary[,2:11] <- round(cons_monetary[,2:11], 1) %>% 
  as_tibble()


print(
  kable(cons_monetary, 
        digits = 1,
        caption = "",
        format="latex", 
        row.names = FALSE,
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/tax_emb_prod_monetary.tex"
)


# 4 sectors composition
cons_share <- cons %>% 
  group_by(exp_decile) %>% 
  mutate(cons_share = cons/sum(cons, na.rm = TRUE)*100) %>% 
  ungroup() %>% 
  select(-cons) %>% 
  pivot_wider(names_from = "exp_decile", values_from = "cons_share") %>% 
  set_colnames(c("Product", paste0("Q", 1:10))) %>% 
  arrange(Product) %>% 
  mutate(Product = factor(Product, c("electricity",
                                     "fuel",
                                     "foods",
                                     "other")))


cons_share[,2:11] <- round(cons_share[,2:11], 1) %>% 
  as_tibble()


print(
  kable(cons_share, 
        digits = 1,
        caption = "",
        format="latex", 
        row.names = FALSE,
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/tax_emb_prod_share.tex"
)



# 5.6 Tab for Fig. 7-10 -----
fun_stest <- function(scene){
  
  tax_rate <- c(2, 40, 100, 120)
  
  percent_trf <- c(0, 25, 75, 100)
  
  list_df_trf <- list()
  list_df_tax <- list()
  
  for (i in tax_rate){
    for (j in percent_trf){
      
      list_df_trf[[as.character(j)]] <- swf_decile(tax = i, # tax rate; input: numeric
                                                   tax_scenario = scene, # input: national, national_import, electricity, lpg
                                                   transfer_scenario = "uniform", #input: no_transfer, uniform, poorest20, poorest40, blt
                                                   percent_transfer = j) %>% 
        select(exp_decile, net_impact) %>% 
        mutate(percent_trf = as.character(j))
      
      
    }
    
    list_df_tax[[as.character(i)]]  <- bind_rows(list_df_trf) %>% 
      mutate(tax_rate = as.character(i))
    
  }
  
  df_plot <- bind_rows(list_df_tax) %>% 
    mutate(tax_rate = case_when(tax_rate == "2" ~ "$2/tonne",
                                tax_rate == "40" ~ "$40/tonne",
                                tax_rate == "100" ~ "$100/tonne",
                                tax_rate == "120" ~ "$120/tonne")) %>% 
    mutate(percent_trf = factor(percent_trf, c("0", "25", "75", "100")),
           tax_rate = factor(tax_rate, c("$2/tonne", "$40/tonne", "$100/tonne", "$120/tonne"))) %>% 
    rename("Recycling" = "percent_trf")
  
  return(df_plot)
  
}

df_stest_nat <- fun_stest(scene = "national") %>% 
  unite("Recycling", Recycling:tax_rate, sep = "_") %>% 
  mutate(net_impact = net_impact * 100) %>% 
  pivot_wider(., names_from = Recycling, values_from = net_impact) %>% 
  set_colnames(c("Income decile", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%"))

print(
  kable(df_stest_nat, 
        digits = 2,
        caption = "",
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/stest-new-nat.tex")


df_stest_natimp <- fun_stest(scene = "national_import") %>% 
  unite("Recycling", Recycling:tax_rate, sep = "_") %>% 
  mutate(net_impact = net_impact * 100) %>% 
  pivot_wider(., names_from = Recycling, values_from = net_impact) %>% 
  set_colnames(c("Income decile", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%"))

print(
  kable(df_stest_natimp, 
        digits = 2,
        caption = "",
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/stest-new-natimp.tex")



df_stest_elec <- fun_stest(scene = "electricity") %>% 
  unite("Recycling", Recycling:tax_rate, sep = "_") %>% 
  mutate(net_impact = net_impact * 100) %>% 
  pivot_wider(., names_from = Recycling, values_from = net_impact) %>% 
  set_colnames(c("Income decile", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%"))


print(
  kable(df_stest_elec, 
        digits = 2,
        caption = "",
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/stest-new-elec.tex")



df_stest_fuel <- fun_stest(scene = "lpg") %>% 
  unite("Recycling", Recycling:tax_rate, sep = "_") %>% 
  mutate(net_impact = net_impact * 100) %>% 
  pivot_wider(., names_from = Recycling, values_from = net_impact) %>% 
  set_colnames(c("Income decile", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%", "0%", "25%", "75%", "100%"))

print(
  kable(df_stest_fuel, 
        digits = 2,
        caption = "",
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "Article/Tables/New/stest-new-fuel.tex")









########################################################

recyc_2 <- e_hh_agg %>% 
  mutate(rev_nat_2 = sum(2 * 14860 * national, na.rm = TRUE),
         rev_int_2 = sum(2 * 14860 * national_import, na.rm = TRUE),
         rev_elec_2 = sum(2 * 14860 * electricity, na.rm = TRUE),
         rev_fuel_2 = sum(2 * 14860 * lpg, na.rm = TRUE),
         nat_2 = (rev_nat_2/nrow(.))/14860,
         int_2 = (rev_int_2/nrow(.))/14860,
         elec_2 = (rev_elec_2/nrow(.))/14860,
         fuel_2 = (rev_fuel_2/nrow(.))/14860)

recyc_40 <- e_hh_agg %>% 
  mutate(rev_nat_40 = sum(40 * 14860 * national, na.rm = TRUE),
         rev_int_40 = sum(40 * 14860 * national_import, na.rm = TRUE),
         rev_elec_40 = sum(40 * 14860 * electricity, na.rm = TRUE),
         rev_fuel_40 = sum(40 * 14860 * lpg, na.rm = TRUE),
         nat_40 = (rev_nat_40/nrow(.))/14860,
         int_40 = (rev_int_40/nrow(.))/14860,
         elec_40 = (rev_elec_40/nrow(.))/14860,
         fuel_40 = (rev_fuel_40/nrow(.))/14860)

recyc_100 <- e_hh_agg %>% 
  mutate(rev_nat_100 = sum(100 * 14860 * national, na.rm = TRUE),
         rev_int_100 = sum(100 * 14860 * national_import, na.rm = TRUE),
         rev_elec_100 = sum(100 * 14860 * electricity, na.rm = TRUE),
         rev_fuel_100 = sum(100 * 14860 * lpg, na.rm = TRUE),
         nat_100 = (rev_nat_100/nrow(.))/14860,
         int_100 = (rev_int_100/nrow(.))/14860,
         elec_100 = (rev_elec_100/nrow(.))/14860,
         fuel_100 = (rev_fuel_100/nrow(.))/14860)

recyc_120 <- e_hh_agg %>% 
  mutate(rev_nat_120 = sum(120 * 14860 * national, na.rm = TRUE),
         rev_int_120 = sum(120 * 14860 * national_import, na.rm = TRUE),
         rev_elec_120 = sum(120 * 14860 * electricity, na.rm = TRUE),
         rev_fuel_120 = sum(120 * 14860 * lpg, na.rm = TRUE),
         nat_120 = (rev_nat_120/nrow(.))/14860,
         int_120 = (rev_int_120/nrow(.))/14860,
         elec_120 = (rev_elec_120/nrow(.))/14860,
         fuel_120 = (rev_fuel_120/nrow(.))/14860)


recyc_all <- bind_cols(
  recyc_2[,9:12],
  recyc_40[,9:12],
  recyc_100[,9:12],
  recyc_120[,9:12]) %>% 
  distinct_all() %>% 
  pivot_longer(1:16, names_to = "scene", values_to = "trf") %>% 
  separate(col = "scene", into = c("scene", "rate"), sep = "_") %>% 
  pivot_wider(., names_from = scene, values_from = trf) 


print(
  kable(recyc_all,
        caption = "",
        digits = 0,
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "")

percent_blt <- bind_cols(
  recyc_2[,9:12],
  recyc_40[,9:12],
  recyc_100[,9:12],
  recyc_120[,9:12]) %>% 
  distinct_all() %>% 
  pivot_longer(1:16, names_to = "scene", values_to = "percent") %>% 
  mutate(percent = 100/percent*100) %>% 
  separate(col = "scene", into = c("scene", "rate"), sep = "_") %>% 
  mutate(percent = case_when(percent >100 ~ 100, percent <= 100 ~ percent)) %>% 
  pivot_wider(., names_from = scene, values_from = percent) 

print(
  kable(percent_blt,
        caption = "",
        digits = 0,
        format="latex", 
        booktabs=TRUE) %>%
    column_spec(1, width = "4cm") %>% 
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "")

