


########### download and prepare data ##############
#to set credentials for accessing the ECEMF Scenario Explorer database please run the following script once in a Python console:
#import pyam
#pyam.iiasa.set_config("<username>", "<password>")
#Refer to this [tutorial](https://pyam-iamc.readthedocs.io/en/stable/tutorials/iiasa_dbs.html) for more information!

dir.create("./data", recursive = TRUE, showWarnings = FALSE)

if(downloadData || length(list.dirs(path = "./data", full.names = TRUE, recursive = FALSE)) == 0){
  
  time <- format(Sys.time(), "%Y_%m_%d_%H.%M.%S")
  dataPath <- paste0("./data/", time)
  dir.create(dataPath, recursive = TRUE, showWarnings = FALSE)
  
  py_require("pyam-iamc") # make reticulate provision a Python environment with pyam if none is configured
  source_python("./download_iiasa_db.py")
  
  # Downloading metadata
  print(paste0("Downloading metadata."))
  download_iiasa_meta_py(fileName = paste0(dataPath, "/metadata_",time), db="ecemf_internal", default_only = TRUE)
  
  # Downloading data
  print(paste0("Downloading data"))  
  scenList <- c("WP1 NPI", "WP1 NetZero", "WP1 NetZero-LimBio", "WP1 NetZero-LimCCS", "WP1 NetZero-LimNuc", "WP1 NetZero-ElecPush", "WP1 NetZero-H2Push", "WP1 NetZero-SynfPush", "WP1 NetZero-HighEfficiency")
  download_iiasa_db_py(fileName = paste0(dataPath, "/WP1_allModels_" ,time), db="ecemf_internal", model="*", 
                       scen=scenList, 
                       region=c("EU27 & UK (*)", "EU27 (*)"))
  
  # Filtering downloaded data to retain only most up to date results 
  
  #auxiliary function to simplify model name
  simplifyModelName <- function(model){
    model <- gsub("v\\d*\\.?\\d*\\.?\\d*", "", model) # remove "v1.0.0"
    model <- gsub(" \\d*\\.?\\d*\\.?\\d*", "", model) # remove "1.0.0"
    model <- gsub(" \\d*\\.?\\d*", "", model) # remove "1.1"
    model <- gsub("\\s$*", "", model) # remove trailing spaces
    model <- gsub("\\_*$", "", model) # remove trailing underscores
    model <- gsub("MESSAGEix-GLOBIOM","MESSAGE",model)
    return(model)
  }
  
  # table to filter most up to date results  
  metaFilter <- openxlsx::read.xlsx(paste0(dataPath, "/metadata_", time,".xlsx")) %>%
    tidyr::fill(model, .direction = "down") %>% # fill NAs with above row values
    arrange(desc(create_date)) %>%
    mutate(cleanModelName = simplifyModelName(model)) %>%
    group_by(cleanModelName,scenario) %>% 
    filter(row_number()==1) %>%
    ungroup()
  
  # cleaning data from NAs, formatting issues, filtering only more recent data, and simplifying model names
  data <- read.xlsx(paste0(dataPath, "/WP1_allModels_" , time ,".xlsx")) %>% 
    as.quitte() %>% 
    filter(!is.na(value)) %>%  
    mutate(region = gsub("&amp;", "&", region)) %>% 
    right_join(metaFilter %>% filter(scenario %in% scenList) %>% select(model,scenario), by = join_by(model, scenario)) %>%
    mutate(model = simplifyModelName(model)) %>%
    na.omit()
  
  #additional model added by hand
  data <- data %>%
    rbind(
      read.xlsx(paste0(dataPath, "/WP1_allModels_" , time ,".xlsx")) %>%
        as.quitte() %>%
        filter(!is.na(value), model == "TIAM-ECN 1.2") %>%
        mutate(region = gsub("&amp;", "&", region)) %>%
        na.omit()
    )
  
  write.xlsx(data %>% pivot_wider(names_from = period, values_from = value) %>% select(where(~!all(is.na(.x)))), file = paste0(dataPath, "/WP1_" , time,".xlsx"))
  
}

# select data file
allDataDirs <- list.dirs(path = "./data", full.names = TRUE, recursive = FALSE)
validDirs <- allDataDirs[sapply(allDataDirs, function(d) length(list.files(d, pattern = "^WP1_.*\\.xlsx$")) > 0)]
dataFolder <- sort(validDirs, decreasing = TRUE)[1]
time <- basename(dataFolder)

dataFile <- paste0("./data/", time, "/WP1_", time, ".xlsx")

df <- suppressWarnings(quitte::read.quitte(dataFile)) %>% 
  filter(!is.na(value))


if(file.exists("./hist/historical.rds")){
  hist <- readRDS("./hist/historical.rds")
} else {
  # data directly from the historical mif
  hist <- piamInterfaces::convertHistoricalData(
    mif = "./hist/historical.mif",
    project = "ECEMF",
    regionMapping = "./hist/regionmapping_historical.csv"
  )
  saveRDS(hist,"./hist/historical.rds")
}

if(file.exists("./hist/historical_origName.rds")){
  histOrigN <- readRDS("./hist/historical_origName.rds")
} else {
  # data directly from the historical mif
  histOrigN <- suppressWarnings(quitte::read.quitte("./hist/historical.mif")) %>% 
    filter(!is.na(value) )
  histOrigN <- filter(histOrigN, period >= 1990)
  saveRDS(histOrigN,"./hist/historical_origName.rds")
}



df_safecopy <- df
df <- df_safecopy
df <- df_safecopy %>%
  filter(period %in% seq(2005,2060,5),
         #           scenario %in% c("WP1 NetZero","WP1 NPI"),
         scenario %in% c("WP1 NetZero","WP1 NPI","WP1 NetZero-LimBio","WP1 NetZero-LimNuc","WP1 NetZero-LimCCS"),
         region == "EU27 & UK (*)" )

histOrigN_safecopy <- histOrigN
histOrigN <- histOrigN_safecopy %>%
  filter(period %in% seq(1990,2030,1),
         region == "EUR" )



## helper lists and numbers -->
  
mappingForAveraging <- quitte::remind_timesteps %>%
  filter(period %in% c(2005, 2010, 2015, 2020)) %>%
  # weight can be dropped in this case, because it is always 1
  select(-"weight")

convertEJ2TWh <- 277.7777778

listForQuartiles <- list(
  Min = min,
  Mean = mean,
  Median = median,
  Max = max,
  Sd = sd,
  Q1=~quantile(., probs = 0.25),
  Q3=~quantile(., probs = 0.75)
)

 # complete missing data for models:  -->

# Euro-Calliope: 
df_woModel   <- filter(df, model != "Euro-Calliope")
df_onlyModel <- filter(df, model == "Euro-Calliope")
df_onlyModel <- calc_addVariable(df_onlyModel, "`Secondary Energy|Liquids|Electricity`" = "`Secondary Energy|Liquids|Hydrogen`", units = "EJ/yr") # Euro-Calliope reports syngas under "hydrogen", this script uses "electricity" 
df_onlyModel <- calc_addVariable(df_onlyModel, "`Secondary Energy|Gases|Electricity`" = "`Secondary Energy|Gases|Hydrogen`", units = "EJ/yr") # Euro-Calliope reports syngas under "hydrogen", this script uses "electricity" 
df <- rbind(df_woModel, df_onlyModel)

#IMAGE
df_woModel   <- filter(df, model != "IMAGE")
df_onlyModel <- filter(df, model == "IMAGE")
# no helper calculations required 
df <- rbind(df_woModel, df_onlyModel)

# # MESSAGE: 
# not part of the paper

#OSEMBE
df_woModel   <- filter(df, model != "OSeMBE")
df_onlyModel <- filter(df, model == "OSeMBE")
# no helper calculations required
df <- rbind(df_woModel, df_onlyModel)

#PRIMES
df_woModel   <- filter(df, model != "PRIMES")
df_onlyModel <- filter(df, model == "PRIMES")
# no helper calculations required
df <- rbind(df_woModel, df_onlyModel)


#PROMETHEUS
df_woModel   <- filter(df, model != "PROMETHEUS")
df_onlyModel <- filter(df, model == "PROMETHEUS")

# adding missing extra-eu bunkers 
df_onlyModel <- df_onlyModel %>%
  filter(variable != "Emissions|CO2|Energy|Demand|Bunkers") %>%
  rbind(
    df_woModel %>%
      filter(variable == "Emissions|CO2|Energy|Demand|Bunkers") %>%
      group_by(period, region, scenario) %>%
      summarize(average_emissions = mean(value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(model = "PROMETHEUS",
             unit = "Mt CO2/yr",
             variable = "Emissions|CO2|Energy|Demand|Bunkers",
             value = 2/3 * average_emissions) %>%
      select(model, scenario, region, variable, unit, period, value)
  )
# add missing GDP
df_onlyModel <- df_onlyModel %>%
  filter(variable != "GDP|MER") %>%
  rbind(
    df_woModel %>%
      filter(variable == "GDP|MER") %>%
      group_by(period, region, scenario) %>%
      summarize(average_GDP = mean(value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(model = "PROMETHEUS",
             unit = "billion EUR_2020/yr",
             variable = "GDP|MER",
             value = average_GDP) %>%
      select(model, scenario, region, variable, unit, period, value)
  )

df_onlyModel <- calc_addVariable(df_onlyModel, "`Emissions|CO2|Energy and Industrial Processes`" = "`Emissions|CO2|Energy and Industrial Processes` + `Emissions|CO2|Energy|Demand|Bunkers`", units = "Mt CO2/yr")
df <- rbind(df_woModel, df_onlyModel)

# REMIND: 
df_woModel   <- filter(df, model != "REMIND")
df_onlyModel <- filter(df, model == "REMIND")
# no helper calculations required 
df <- rbind(df_woModel, df_onlyModel)

# TIAM-ECN: 
df_woModel   <- filter(df, model != "TIAM-ECN")
df_onlyModel <- filter(df, model == "TIAM-ECN")

#df_onlyModel <- df_onlyModel %>% mutate(value = if_else(model == "TIAM-ECN" & scenario == "WP1 NetZero" & region == "EU27 & UK (*)" & variable == "Emissions|CO2|Energy|Supply|Electricity" & period == 2030, 72, value))
df_onlyModel <- calc_addVariable(df_onlyModel, "`Secondary Energy|Gases|Electricity`" = "`Secondary Energy|Gases|Hydrogen`", units = "EJ/yr") # TIAM-ECN reports syngas under "hydrogen", this script uses "electricity" 
df_onlyModel <- calc_addVariable(df_onlyModel, "`Secondary Energy|Liquids|Electricity`" = "`Secondary Energy|Liquids|Hydrogen`", units = "EJ/yr") # TIAM-ECN reports SE synliq under "hydrogen", this script uses "electricity" 

# add missing GDP
df_onlyModel <- df_onlyModel %>%
  filter(variable != "GDP|MER") %>%
  rbind(
    df_woModel %>%
      filter(variable == "GDP|MER") %>%
      group_by(period, region, scenario) %>%
      summarize(average_GDP = mean(value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(model = "TIAM-ECN",
             unit = "billion EUR_2020/yr",
             variable = "GDP|MER",
             value = average_GDP) %>%
      select(model, scenario, region, variable, unit, period, value)
  )

df_onlyModel <- calc_addVariable(df_onlyModel, "`Emissions|CO2|Industrial Processes`" = "`Emissions|CO2|Energy and Industrial Processes` - `Emissions|CO2|Energy` ", units = "Mt CO2/yr")

df <- rbind(df_woModel, df_onlyModel)

# WITCH: 
df_woModel   <- filter(df, model != "WITCH")
df_onlyModel <- filter(df, model == "WITCH")
# no helper calculations required
df <- rbind(df_woModel, df_onlyModel)


# test negative emissions
# quitte error if a formula variable is entirely absent from the data
# WITCH does not report Other/Other Removal/Waste, so add absent variables as explicit zero rows before the check
testVars <- c("Emissions|CO2", "Emissions|CO2|Energy and Industrial Processes", "Emissions|CO2|AFOLU",
              "Emissions|CO2|Other", "Carbon Capture|Storage|Direct Air Capture", "Emissions|CO2|Waste")
testData <- df_onlyModel %>% mutate(variable = as.character(variable))
testData <- bind_rows(testData,
                      testData %>%
                        distinct(model, scenario, region, period) %>%
                        tidyr::crossing(variable = setdiff(testVars, unique(testData$variable))) %>%
                        mutate(unit = "Mt CO2/yr", value = 0))
test <- filter(calc_addVariable(testData, "`Emi|CO2|diffToEnInd`"  = "`Emissions|CO2` - `Emissions|CO2|Energy and Industrial Processes` - `Emissions|CO2|AFOLU` - `Emissions|CO2|Other` + `Carbon Capture|Storage|Direct Air Capture` - `Emissions|CO2|Waste`", units = "MtCO2/yr", only.new = TRUE, completeMissing = TRUE),abs(value) > 0.1)

# complete missing data for historical:  -->

# UNFCCC
histOrigN_woModel   <- filter(histOrigN, model != "UNFCCC")
histOrigN_onlyModel <- filter(histOrigN, model == "UNFCCC")

histOrigN_onlyModel <- calc_addVariable(histOrigN_onlyModel, "`Emi|CO2|Calc Other`" = "`Emi|CO2|Energy and Industrial Processes` - `Emi|CO2|Energy|Supply` - `Emi|CO2|Energy|Demand` - `Emi|CO2|Industrial Processes`", units = "Mt CO2/yr", completeMissing = TRUE) # UNFCCC mapping currently misses some emissions to match the totals 
histOrigN_onlyModel <- calc_addVariable(histOrigN_onlyModel, "`Emi|GHG|Calc Other`" = "`Emi|GHG|Energy and Industrial Processes` - `Emi|GHG|Energy` - `Emi|GHG|Industrial Processes`", units = "Mt CO2e/yr", completeMissing = TRUE) # UNFCCC mapping currently misses some emissions to match the totals 
histOrigN_onlyModel <- calc_addVariable(histOrigN_onlyModel, "`Emi|GHG|w/ intraEU Bunkers|LULUCF national accounting`" = "`Emi|GHG|w/o Bunkers|LULUCF national accounting` + 0.33 * `Emi|GHG|Energy|Demand|Transport|International Bunkers` ", units = "Mt CO2/yr", completeMissing = TRUE) # calculate ex-EU bunkers as 33% of total bunkers
histOrigN_onlyModel <- calc_addVariable(histOrigN_onlyModel,"`Emi|Kyoto Gases|non-CO2 calc`" = "`Emi|GHG|LULUCF national accounting` - `Emi|CO2|LULUCF national accounting`", units = "MtCO2e/yr", completeMissing = TRUE)
histOrigN_onlyModel <- calc_addVariable(histOrigN_onlyModel,"`Emi|Kyoto Gases|non-CO2 calc|Land-Use Change`" = "`Emi|GHG|Land-Use Change|LULUCF national accounting` - `Emi|CO2|Land-Use Change|LULUCF national accounting`", units = "MtCO2e/yr", completeMissing = TRUE)

histOrigN <- rbind(histOrigN_woModel, histOrigN_onlyModel)


# calculate additional variables for all models -->

df <- calc_addVariable(df, "`Emi|Kyoto Gases|non-CO2 calc`" = "`Emissions|Kyoto Gases` - `Emissions|CO2`", units = "MtCO2e/yr")
df <- calc_addVariable(df, "`Emi|Kyoto Gases|non-CO2 DiffRepTocalc`" = "`Emissions|Kyoto Gases|non-CO2` - `Emi|Kyoto Gases|non-CO2 calc`", units = "MtCO2e/yr")

df <- calc_addVariable(df, "`Carbon Removal|Geological Storage|ForPlotting`" = "-1 * `Carbon Removal|Geological Storage`", units = "MtCO2/yr")

#calculate_Elecshares

df <- calc_addVariable(df, "`Final Energy|Electricity Share`" = "`Final Energy|Electricity` / `Final Energy`", units = "%")
df <- calc_addVariable(df, "`Final Energy|Electricity Share woBunkers woNonEnergy`" = "(`Final Energy|Electricity` - `Final Energy|Bunkers|Electricity`) / (`Final Energy` - `Final Energy|Bunkers` - `Final Energy|Non-Energy Use`)", units = "%", completeMissing = TRUE)

histOrigN <- calc_addVariable(histOrigN, "`Final Energy|Electricity Share woBunkers woNonEnergy`" = "(`FE|Electricity`) / (`FE` - `FE|Transport|Bunkers` - `FE|Non-energy Use`)", units = "%", completeMissing = TRUE)
histOrigN <- calc_addVariable(histOrigN, "`Final Energy|Electricity Share`" = "`FE|Electricity` / `FE`", units = "%", completeMissing = TRUE)

df <- calc_addVariable(df, "`Final Energy|Industry|Electricity Share`" = "`Final Energy|Industry|Electricity` / `Final Energy|Industry`", units = "%")
df <- calc_addVariable(df, "`Final Energy|Residential and Commercial|Electricity Share`" = "`Final Energy|Residential and Commercial|Electricity` / `Final Energy|Residential and Commercial`", units = "%")
df <- calc_addVariable(df, "`Final Energy|Transportation|Electricity Share`" = "`Final Energy|Transportation|Electricity` / `Final Energy|Transportation`", units = "%")

df <- calc_addVariable(df, "`Final Energy|Nonelectric`" = "`Final Energy` - `Final Energy|Electricity`", units = "%")
#    df <- calc_addVariable(df, "`UE|Nonelectric`" = "`Final Energy|Nonelectric`", units = "%")     
#    df <- calc_addVariable(df, "`UE|Nonelectric`" = "`Final Energy|Nonelectric`", units = "%") 


#calculate_H2hares

df <- calc_addVariable(df, "`Final Energy|Hydrogen Share`" = "`Final Energy|Hydrogen` / `Final Energy`", units = "%")
df <- calc_addVariable(df, "`Final Energy|Hydrogen Share woBunkers woNonEnergy`" = "(`Final Energy|Hydrogen` - `Final Energy|Bunkers|Hydrogen` - `Final Energy|Non-Energy Use|Hydrogen`) / (`Final Energy` - `Final Energy|Bunkers` - `Final Energy|Non-Energy Use`)", units = "%", completeMissing = TRUE)

df <- calc_addVariable(df, "`Final Energy|Industry|Hydrogen Share`" = "`Final Energy|Industry|Hydrogen` / `Final Energy|Industry`", units = "%")
df <- calc_addVariable(df, "`Final Energy|Residential and Commercial|Hydrogen Share`" = "`Final Energy|Residential and Commercial|Hydrogen` / `Final Energy|Residential and Commercial`", units = "%")
df <- calc_addVariable(df, "`Final Energy|Transportation|Hydrogen Share`" = "`Final Energy|Transportation|Hydrogen` / `Final Energy|Transportation`", units = "%")


#calculate_SEshares

if("Trade|Secondary Energy|Liquids|Electricity|Volume" %in% unique(df$variable))
  df <- calc_addVariable(df, "`SE-Import|Liquids|Synfuel`" = " -`Trade|Secondary Energy|Liquids|Electricity|Volume`", units = "EJ/yr")
df <- calc_addVariable(df, "`SE-Import|Liquids|Fossil`" = " -`Trade|Secondary Energy|Liquids|Oil|Volume` - `Trade|Secondary Energy|Liquids|Gas|Volume`", units = "EJ/yr", completeMissing = TRUE)
#MESSAGE  df <- calc_addVariable(df, "`SE-Import|Liquids|Fossil`" = " -`Trade|Secondary Energy|Liquids|Oil|Volume` - `Trade|Secondary Energy|Liquids|Coal|Volume` - `Trade|Secondary Energy|Liquids|Gas|Volume`", units = "EJ/yr", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE-Import|Liquids|Bio`" = " -`Trade|Secondary Energy|Liquids|Biomass|Volume` ", units = "EJ/yr")

df <- calc_addVariable(df, "`SE|Liquids|Bio Share`" = "`Secondary Energy|Liquids|Biomass` / `Secondary Energy|Liquids`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|Liquids|Synfuel Share`" = "`Secondary Energy|Liquids|Electricity` / `Secondary Energy|Liquids`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|Liquids|Fossil Share`" = "`Secondary Energy|Liquids|Fossil` / `Secondary Energy|Liquids`", units = "%", completeMissing = TRUE)

df <- calc_addVariable(df, "`SE|Liquids|SumOfShares`" = "`SE|Liquids|Fossil Share` + `SE|Liquids|Bio Share` + `SE|Liquids|Synfuel Share`", units = "%", completeMissing = TRUE)

df <- calc_addVariable(df, "`SE|Gases|Bio Share`" = "`Secondary Energy|Gases|Biomass` / `Secondary Energy|Gases`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|Gases|Synfuel Share`" = "`Secondary Energy|Gases|Electricity` / `Secondary Energy|Gases`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|Gases|Fossil Share`" = "`Secondary Energy|Gases|Fossil` / `Secondary Energy|Gases`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|Gases|SumOfShares`" = "`SE|Gases|Fossil Share` + `SE|Gases|Bio Share` + `SE|Gases|Synfuel Share`", units = "%", completeMissing = TRUE)

df <- calc_addVariable(df, "`SE|Solids|Bio Share`" = "`Secondary Energy|Solids|Biomass` / `Secondary Energy|Solids`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|Solids|Coal Share`" = "`Secondary Energy|Solids|Coal` / `Secondary Energy|Solids`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|Solids|Fossil Share`" = "`Secondary Energy|Solids|Fossil` / `Secondary Energy|Solids`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|Solids|SumOfShares`" = "`SE|Solids|Fossil Share` + `SE|Solids|Bio Share`", units = "%", completeMissing = TRUE)

# calculate_SEshares_2 

df <- calc_addVariable(df, "`SE|SolLiqGas`" = "`Secondary Energy|Liquids` + `Secondary Energy|Solids` + `Secondary Energy|Gases`", units = "EJ/yr")
df <- calc_addVariable(df, "`SE|SolLiqGas|Bio`" = "`Secondary Energy|Liquids|Biomass` + `Secondary Energy|Solids|Biomass` + `Secondary Energy|Gases|Biomass`", units = "EJ/yr", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|SolLiqGas|Fossil`" = "`Secondary Energy|Liquids|Fossil` + `Secondary Energy|Solids|Fossil` + `Secondary Energy|Gases|Fossil`", units = "EJ/yr", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|SolLiqGas|Synfuel`" = "`Secondary Energy|Liquids|Electricity` +  `Secondary Energy|Gases|Electricity`", units = "EJ/yr", completeMissing = TRUE)


df <- calc_addVariable(df, "`SE|SolLiqGas|Synfuel Share`" = "`SE|SolLiqGas|Synfuel` / `SE|SolLiqGas`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|SolLiqGas|Fossil Share`" = "`SE|SolLiqGas|Fossil` / `SE|SolLiqGas`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|SolLiqGas|Bio Share`" = "`SE|SolLiqGas|Bio` / `SE|SolLiqGas`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`SE|SolLiqGas|SumOfShares`" = "`SE|SolLiqGas|Fossil Share` + `SE|SolLiqGas|Bio Share` + `SE|SolLiqGas|Synfuel Share`", units = "%", completeMissing = TRUE)

#calculate_FEshares_2


df <- calc_addVariable(df, "`FE|SolLiqGas`" = "`Final Energy|Liquids` + `Final Energy|Solids` + `Final Energy|Gases`", units = "EJ/yr")
df <- calc_addVariable(df, "`FE|SolLiqGas|Bio`" = "`Final Energy|Liquids|Biomass` + `Final Energy|Solids|Biomass` + `Final Energy|Gases|Biomass`", units = "EJ/yr", completeMissing = TRUE)
df <- calc_addVariable(df, "`FE|SolLiqGas|Fossil`" = "`Final Energy|Liquids|Fossil` + `Final Energy|Solids|Fossil` + `Final Energy|Gases|Fossil`", units = "EJ/yr", completeMissing = TRUE)
df <- calc_addVariable(df, "`FE|SolLiqGas|Synfuel`" = "`Final Energy|Liquids|Electricity` +  `Final Energy|Gases|Electricity`", units = "EJ/yr", completeMissing = TRUE)

# add reductions of FE|SolLiqGas vs 2020
tmp <- df %>%
  filter(variable == "FE|SolLiqGas",
         region == "EU27 & UK (*)") %>%
  #         !(model %in% models_without_data)) %>%
  mutate(denominator= 39,
          numerator=value) %>%
  mutate(value = 1-(numerator/denominator),
         variable = "FE|SolLiqGas reduction vs 2020",
         unit = "%") %>%
  na.omit() %>%
  select(model, scenario, region, variable, unit, period, value)

df <- rbind(df, tmp) 

df <- calc_addVariable(df, "`FE|SolLiqGas|Synfuel Share`" = "`FE|SolLiqGas|Synfuel` / `FE|SolLiqGas`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`FE|SolLiqGas|Fossil Share`" = "`FE|SolLiqGas|Fossil` / `FE|SolLiqGas`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`FE|SolLiqGas|Bio Share`" = "`FE|SolLiqGas|Bio` / `FE|SolLiqGas`", units = "%", completeMissing = TRUE)
df <- calc_addVariable(df, "`FE|SolLiqGas|SumOfShares`" = "`FE|SolLiqGas|Fossil Share` + `FE|SolLiqGas|Bio Share` + `FE|SolLiqGas|Synfuel Share`", units = "%", completeMissing = TRUE)

#GHG_emission_reductins

# Emissions|Kyoto Gases reduction vs 1990
models_without_data <- df %>% #PRIMES is not reporting Emissions|Kyoto Gases
  filter(variable == "Emissions|Kyoto Gases",
         region == "EU27 & UK (*)") %>%
  group_by(model) %>%
  filter(sum(value*value) == 0) %>%
  pull(model) %>%
  unique()

if(length(models_without_data) > 0)
  warning(paste0("Models that do not report `Emissions|Kyoto Gases` and that are excluded from the variable is chart: ", models_without_data))

tmp <- df %>%
  filter(variable == "Emissions|Kyoto Gases",
         region == "EU27 & UK (*)", 
         !(model %in% models_without_data)) %>%
  mutate(denominator= histOrigN %>% filter(region == "EUR", model == "UNFCCC", variable == "Emi|GHG|LULUCF national accounting", period == "1990") %>% pull(value),
         numerator=value) %>%
  mutate(value = 1-(numerator/denominator),
         variable = "Emissions|Kyoto Gases reduction vs 1990",
         unit = "%") %>%
  na.omit() %>%
  select(model, scenario, region, variable, unit, period, value)

df <- rbind(df, tmp) 


# add_synthetic_LULUCF

periodSynthetic <- c(2005,2010,2015,2020,2025,2030,2035,2040,2045,2050)

### LULUCF

# It is necessary to check also if hist data covers all years 

hist_lulucf_all <- histOrigN %>% 
  filter(variable == "Emi|CO2|Land-Use Change|LULUCF national accounting",
         region == "EUR",
         model == "UNFCCC")

hist_lulucf_2005 <- hist_lulucf_all %>%
  filter(period >= 2005-2, period <= 2005+2) %>%
  summarize(Mean = mean(value, na.rm=TRUE)) %>%
  pull(Mean)
hist_lulucf_2010 <- hist_lulucf_all %>%
  filter(period >= 2010-2, period <= 2010+2) %>%
  summarize(Mean = mean(value, na.rm=TRUE)) %>%
  pull(Mean)
hist_lulucf_2015 <- hist_lulucf_all %>%
  filter(period >= 2015-2, period <= 2015+2) %>%
  summarize(Mean = mean(value, na.rm=TRUE)) %>%
  pull(Mean)
hist_lulucf_2020 <- hist_lulucf_all %>%
  filter(period >= 2020-2, period <= 2020+2) %>%
  summarize(Mean = mean(value, na.rm=TRUE)) %>%
  pull(Mean)


# hist_lulucf <- histOrigN %>% 
#   filter(variable == "Emi|CO2|Land-Use Change",
#          period >= 2018, period <= 2022,
#          region == "EUR",
#          model == "UNFCCC") %>%
#   summarize(Mean = mean(value, na.rm=TRUE)) %>%
#   pull(Mean)
# 
# hist_lulucf2015 <- histOrigN %>% 
#   filter(variable == "Emi|CO2|Land-Use Change",
#          period >= 2013, period <= 2017,
#          region == "EUR",
#          model == "UNFCCC") %>%
#   summarize(Mean = mean(value, na.rm=TRUE)) %>%
#   pull(Mean)

# Type 1: Model has AFOLU CO2 data reported (equivalent to LULUCF). 
df_type1_orig <- df %>% # reported afolu data models
  filter(variable == "Emissions|CO2|AFOLU",
         period %in% periodSynthetic, #         period %in% c(2020,2030,2040,2050),
         scenario == "WP1 NetZero",
         region == "EU27 & UK (*)") %>%
  group_by(model) %>%
  filter(sum(value*value) != 0) %>%
  ungroup()

model_w_lulucf <- df_type1_orig %>%
  pull(model) %>%
  unique()

df_lulucf_adj <- left_join( # calculating adjustment factors based on 2020 historical values
  df_type1_orig,
  df_type1_orig %>%
    filter(period == 2020) %>%
    mutate(hist2020 = hist_lulucf_2020,
           multFactor = hist_lulucf_2020 / value,
           diffFactor = hist_lulucf_2020 - value) %>%
    select(-period, -value),
  by = join_by(model == model, scenario == scenario, region == region, variable == variable, unit == unit)
) %>%
  mutate(
    valueAdjMult = value * multFactor,
    valueAdjDiff = value + diffFactor
  )

df_lulucf_w_data <- df_lulucf_adj %>% # calculating the adjusted data using the difference between historical values to translate the model values instead of using a multiplicative factor
  mutate(value = valueAdjDiff) %>%
  select(model, scenario, region, variable, unit, period, value)

# Type 2: Model has no CO2 AFOLU data reported
model_wo_lulucf <- df %>% filter(!(model %in% model_w_lulucf)) %>% pull(model) %>% unique() 

df_lulucf_wo_data <- full_join( #assigning synthetic value to Emissions|CO2|AFOLU
  df_lulucf_w_data %>% ungroup() %>% select(-model, -value) %>% unique(),
  data.frame(model=rep(model_wo_lulucf,each=length(periodSynthetic)), period=rep(periodSynthetic,length(model_wo_lulucf)), value = rep(c(hist_lulucf_2005,hist_lulucf_2010,hist_lulucf_2015,hist_lulucf_2020, -280, -310,-310, -310,-310, -310),length(model_wo_lulucf))),
  by = join_by(period == period)
)

# Synthetic CO2 AFOLU data
df_lulucf <- rbind(
  df_lulucf_w_data,
  df_lulucf_wo_data
) %>%
  mutate(variable = "Emissions|CO2|AFOLU synthetic")

# adding synthetic data to main data
df <- rbind(
  df %>% filter(!(variable %in% c("Emissions|CO2|AFOLU synthetic"))),
  df_lulucf
)

# add_synthetic_nonGHG

hist_nonCO2_all <- histOrigN %>% 
  filter(region == "EUR",
         model == "UNFCCC",
         variable == "Emi|Kyoto Gases|non-CO2 calc")

hist_nonCO2_2005 <- hist_nonCO2_all %>%
  filter(period >= 2005-2, period <= 2005+2) %>%
  summarize(Mean = mean(value, na.rm=TRUE)) %>%
  pull(Mean)
hist_nonCO2_2010 <- hist_nonCO2_all %>%
  filter(period >= 2010-2, period <= 2010+2) %>%
  summarize(Mean = mean(value, na.rm=TRUE)) %>%
  pull(Mean)
hist_nonCO2_2015 <- hist_nonCO2_all %>%
  filter(period >= 2015-2, period <= 2015+2) %>%
  summarize(Mean = mean(value, na.rm=TRUE)) %>%
  pull(Mean)
hist_nonCO2_2020 <- hist_nonCO2_all %>%
  filter(period >= 2020-2, period <= 2020+2) %>%
  summarize(Mean = mean(value, na.rm=TRUE)) %>%
  pull(Mean)

# Type A: Models that include non-CO2 emissions
model_w_nonCO2 <- df %>% # reported afolu data models
  filter(variable == "Emissions|Kyoto Gases",
         period %in% c(2015,2020,2025,2030,2035,2040,2045,2050),
         scenario == "WP1 NetZero",
         region == "EU27 & UK (*)") %>%
  group_by(model) %>%
  filter(sum(value*value) != 0) %>%
  ungroup() %>%
  pull(model) %>%
  unique()

df_nonCO2_w_data <- calc_addVariable(df, "`Emi|Kyoto Gases|non-CO2 calc`" = "`Emissions|Kyoto Gases` - `Emissions|CO2`", units = "MtCO2e/yr", only.new = TRUE) %>%
  filter(period %in% periodSynthetic,
         scenario == "WP1 NetZero",
         region == "EU27 & UK (*)",
         model %in% model_w_nonCO2) 

# Type B: Model has no GHG data reported
model_wo_nonCO2 <- df %>% filter(!(model %in% model_w_nonCO2)) %>% pull(model) %>% unique() 

df_nonCO2_wo_data <- full_join( #assigning synthetic value to Emissions|Kyoto Gases|non-CO2 synthetic
  df_nonCO2_w_data %>% ungroup() %>% select(-model, -value) %>% unique(),
  data.frame(model=rep(model_wo_nonCO2,each=length(periodSynthetic)), period=rep(periodSynthetic,length(model_wo_nonCO2)), value = rep(c(hist_nonCO2_2005,hist_nonCO2_2010,hist_nonCO2_2015,hist_nonCO2_2020, 630, 580, 550, 520, 515, 510),length(model_wo_nonCO2))),
  by = join_by(period == period)
)

# Synthetic non-CO2 data
df_nonCO2 <- rbind(
  df_nonCO2_w_data,
  df_nonCO2_wo_data
) %>%
  mutate(variable = "Emissions|Kyoto Gases|non-CO2 synthetic")

# adding synthetic data to main data
df <- rbind(
  df %>% filter(!(variable %in% c("Emissions|Kyoto Gases|non-CO2 synthetic"))),
  df_nonCO2
)

# calculate_addSynthVar

ShareIntraEUAviationInBunkers <- 66.01 /  filter(histOrigN, model == "UNFCCC", variable == "Emi|GHG|Energy|Demand|Transport|International Bunkers", period == 2019)$value # The intra-EU aviation emission in the ETS was 66.01 MtCO2e in 2019, the last year with EU27&UK reporting and before the Covid shock

df_woModel   <- filter(df, !model %in% modelsFullSystem )
df_onlyModel <- filter(df, model %in% modelsFullSystem )

df_onlyModel <- calc_addVariable(df_onlyModel, "`Emi|Kyoto Gases|synthetic`" = "`Emissions|CO2|Energy and Industrial Processes` + `Emissions|CO2|Other` - `Carbon Capture|Storage|Direct Air Capture` + `Emissions|CO2|Waste` +  `Emissions|CO2|AFOLU synthetic` + `Emissions|Kyoto Gases|non-CO2 synthetic` ", units = "MtCO2e/yr", completeMissing = TRUE)
df_onlyModel <- calc_addVariable(df_onlyModel, "`Gross Emi|Kyoto Gases|synthetic`" = "`Gross Emissions|CO2|Energy and Industrial Processes` + `Emissions|CO2|Other` + `Emissions|CO2|Waste` + `Emissions|Kyoto Gases|non-CO2 synthetic` ", units = "MtCO2e/yr", completeMissing = TRUE)
df_onlyModel <- calc_addVariable(df_onlyModel, "`Emi|CO2|synthetic`" = "`Emissions|CO2|Energy and Industrial Processes` + `Emissions|CO2|Other` - `Carbon Capture|Storage|Direct Air Capture` + `Emissions|CO2|Waste` +  `Emissions|CO2|AFOLU synthetic` ", units = "MtCO2/yr", completeMissing = TRUE)
df_onlyModel <- calc_addVariable(df_onlyModel, "`Carbon Removal|Synthetic AFOLU total`" = "-1 * `Emissions|CO2|AFOLU synthetic`", units = "MtCO2e/yr", completeMissing = TRUE)

df_onlyModel <- calc_addVariable(df_onlyModel, "`Emi|Kyoto Gases|synthetic wo Bunkers`" = "`Emi|Kyoto Gases|synthetic` - `Emissions|CO2|Energy|Demand|Bunkers`  ", units = "MtCO2e/yr", completeMissing = TRUE)
df_onlyModel <- calc_addVariable(df_onlyModel, "`Emi|Kyoto Gases|synthetic w/ intraEU Bunkers`" = "`Emi|Kyoto Gases|synthetic` - 0.667 * `Emissions|CO2|Energy|Demand|Bunkers`  ", units = "MtCO2e/yr", completeMissing = TRUE)
df_onlyModel <- calc_addVariable(df_onlyModel, "`Emi|Kyoto Gases|synthetic w/ intraEU Aviation`" = "`Emi|Kyoto Gases|synthetic` - (1 - 0.2091 ) * `Emissions|CO2|Energy|Demand|Bunkers`  ", units = "MtCO2e/yr", completeMissing = TRUE) # inserting the value of ShareIntraEUAviationInBunkers (0.2091) here, because calc_addVariable can't deal with exogenous parameters


  df_onlyModel <- rbind(df_onlyModel, calc_addScenPercentChange(df_onlyModel, "Energy System Cost|Supply", "WP1 NPI", "NPi"))
  df_onlyModel <- rbind(df_onlyModel, calc_addScenIncrease(df_onlyModel, "Energy System Cost|Supply", "WP1 NPI", "NPi"))

df_onlyModel <- rbind(df_onlyModel, mutate(calcCumulatedDiscount(filter(df_onlyModel, scenario == "WP1 NetZero", model == "REMIND",period >= 2025, period <=2050, variable == "Energy System Cost|Supply|Inc over NPi"),
                                                                nameVar="Energy System Cost|Supply|Inc over NPi", discount=0.00),
                                          unit = "billion EUR_2020"))
df_onlyModel <- rbind(df_onlyModel,mutate(calcCumulatedDiscount(filter(df_onlyModel,period >= 2025, period <=2050),"Energy System Cost|Supply", discount=0.00), unit = "billion EUR_2020"))

df_onlyModel <- rbind(df_onlyModel, calc_addScenIncrease(df_onlyModel,"Energy System Cost|Supply|aggregated","WP1 NPI", "NPi") )
df_onlyModel <- rbind(df_onlyModel, calc_addScenPercentChange(df_onlyModel,"Energy System Cost|Supply|aggregated","WP1 NPI", "NPi") )

# consumption
df_onlyModel <- rbind(df_onlyModel,mutate(calcCumulatedDiscount(filter(df_onlyModel,period >= 2025, period <=2050),
                                                                "Consumption", discount=0.00), unit = "billion EUR_2020"))
df_onlyModel <- rbind(df_onlyModel, calc_addScenIncrease(df_onlyModel,"Consumption|aggregated","WP1 NPI", "NPi") )
df_onlyModel <- rbind(df_onlyModel, calc_addScenPercentChange(df_onlyModel,"Consumption|aggregated","WP1 NPI", "NPi") )
df_onlyModel <- rbind(df_onlyModel, calc_addScenIncrease(df_onlyModel,"Consumption","WP1 NPI", "NPi") )
df_onlyModel <- rbind(df_onlyModel, calc_addScenPercentChange(df_onlyModel,"Consumption","WP1 NPI", "NPi") )


# MAC
df_onlyModel <- rbind(df_onlyModel,mutate(calcCumulatedDiscount(filter(df_onlyModel,period >= 2025, period <=2050),
                                                                "Policy Cost|Area under MAC Curve", discount=0.00), unit = "billion EUR_2020"))

# GDP
df_onlyModel <- rbind(df_onlyModel,mutate(calcCumulatedDiscount(filter(df_onlyModel,period >= 2025, period <=2050),
                                                                "GDP|MER", discount=0.00), unit = "billion EUR_2020"))
# Emissions|Kyoto Gases reduction vs 1990

tmp <- df_onlyModel %>%
  filter(variable == "Emi|Kyoto Gases|synthetic",
         region == "EU27 & UK (*)") %>%
  mutate(denominator= histOrigN %>% filter(region == "EUR", model == "UNFCCC", variable == "Emi|GHG|w/ Bunkers|LULUCF national accounting", period == "1990") %>% pull(value),
         numerator=value) %>%
  mutate(value = 1-(numerator/denominator),
         variable = "Emi|Kyoto Gases|synthetic| reduction vs 1990",
         unit = "%") %>%
  na.omit() %>%
  select(model, scenario, region, variable, unit, period, value)

tmp2 <- df_onlyModel %>%
  filter(variable == "Emi|Kyoto Gases|synthetic wo Bunkers",
         region == "EU27 & UK (*)") %>%
  mutate(denominator= histOrigN %>% filter(region == "EUR", model == "UNFCCC", variable == "Emi|GHG|w/o Bunkers|LULUCF national accounting", period == "1990") %>% pull(value),
         numerator=value) %>%
  mutate(value = 1-(numerator/denominator),
         variable = "Emi|Kyoto Gases|synthetic wo Bunkers| reduction vs 1990",
         unit = "%") %>%
  na.omit() %>%
  select(model, scenario, region, variable, unit, period, value)

tmp3 <- df_onlyModel %>%
  filter(variable == "Emi|Kyoto Gases|synthetic w/ intraEU Bunkers",
         region == "EU27 & UK (*)") %>%
  mutate(denominator= histOrigN %>% filter(region == "EUR", model == "UNFCCC", variable == "Emi|GHG|w/ intraEU Bunkers|LULUCF national accounting", period == "1990") %>% pull(value),
         numerator=value) %>%
  mutate(value = (1-(numerator/denominator)),
         variable = "Emi|Kyoto Gases|synthetic w/ intraEU Bunkers| reduction vs 1990",
         unit = "%") %>%
  na.omit() %>%
  select(model, scenario, region, variable, unit, period, value)

df <- rbind(df_woModel, df_onlyModel,tmp,tmp2, tmp3) 


# -----# mitigation costs in absolute values ------------------------------------------

modelPolicyCostConsLoss   <- c("REMIND","WITCH")
modelPolicyCostMAC        <- c("IMAGE")
modelPolicyCostEnSysCost  <- c("PRIMES","PROMETHEUS","TIAM-ECN")

df_modelPolicyCostConsLoss    <- filter(df, model %in% modelPolicyCostConsLoss )
df_modelPolicyCostMAC         <- filter(df, model %in% modelPolicyCostMAC )
df_modelPolicyCostEnSysCost   <- filter(df, model %in% modelPolicyCostEnSysCost )
df_modelsNoPolicyCosts        <- filter(df, !model %in% modelsFullSystem)

df_modelPolicyCostConsLoss <- calc_addVariable(df_modelPolicyCostConsLoss, 
                                               "`Mitigation costs|Absolute|aggregated`" = " - `Consumption|aggregated|Inc over NPi` ", 
                                               units = "billion EUR_2020", completeMissing = TRUE)
df_modelPolicyCostConsLoss <- calc_addVariable(df_modelPolicyCostConsLoss, 
                                               "`Mitigation costs|Absolute`" = " - `Consumption|Inc over NPi` ", 
                                               units = "billion EUR_2020", completeMissing = TRUE)

df_modelPolicyCostMAC <- calc_addVariable(df_modelPolicyCostMAC, 
                                               "`Mitigation costs|Absolute|aggregated`" = "`Policy Cost|Area under MAC Curve|aggregated` ", 
                                               units = "billion EUR_2020", completeMissing = TRUE)
df_modelPolicyCostMAC <- calc_addVariable(df_modelPolicyCostMAC, 
                                          "`Mitigation costs|Absolute`" = "`Policy Cost|Area under MAC Curve` ", 
                                          units = "billion EUR_2020", completeMissing = TRUE)

df_modelPolicyCostEnSysCost <- calc_addVariable(df_modelPolicyCostEnSysCost, 
                                               "`Mitigation costs|Absolute|aggregated`" = "`Energy System Cost|Supply|aggregated|Inc over NPi` ", 
                                               units = "billion EUR_2020", completeMissing = TRUE)
df_modelPolicyCostEnSysCost <- calc_addVariable(df_modelPolicyCostEnSysCost, 
                                                "`Mitigation costs|Absolute`" = "`Energy System Cost|Supply|Inc over NPi` ", 
                                                units = "billion EUR_2020", completeMissing = TRUE)





df <- rbind(df_modelPolicyCostConsLoss, df_modelPolicyCostMAC, df_modelPolicyCostEnSysCost, df_modelsNoPolicyCosts)

df <- calc_addVariable(df, "`Mitigation costs|RelativeToGDP|aggregated`" = "100 * `Mitigation costs|Absolute|aggregated` / `GDP|MER|aggregated`", units = "%")
df <- calc_addVariable(df, "`Mitigation costs|RelativeToGDP`" = "100 * `Mitigation costs|Absolute` / `GDP|MER`", units = "%")


# add_variables_VRE


df <- calc_addVariable(df, "`Secondary Energy|Electricity|WindSolar`" = "`Secondary Energy|Electricity|Wind` + `Secondary Energy|Electricity|Solar`", units = "EJ/yr")
df <- calc_addVariable(df, "`Secondary Energy|Electricity|Sum`" = "`Secondary Energy|Electricity|Ocean` + `Secondary Energy|Electricity|Biomass` + `Secondary Energy|Electricity|Coal` + `Secondary Energy|Electricity|Gas` + `Secondary Energy|Electricity|Geothermal` + `Secondary Energy|Electricity|Hydro` + `Secondary Energy|Electricity|Hydrogen` + `Secondary Energy|Electricity|Nuclear` + `Secondary Energy|Electricity|Oil` + `Secondary Energy|Electricity|WindSolar` ", units = "EJ/yr")
df <- calc_addVariable(df, "`Secondary Energy|Electricity|DiffToSum`" = "`Secondary Energy|Electricity` - `Secondary Energy|Electricity|Sum`", units = "EJ/yr")
df <- calc_addVariable(df, "`Secondary Energy|Electricity|VRE Share`" = "`Secondary Energy|Electricity|WindSolar` / `Secondary Energy|Electricity`", units = "%")


# add_Trade_variables

df <- calc_addVariable(df, "`Trade|Energy`" = " `Trade|Primary Energy|Biomass|Volume` + `Trade|Primary Energy|Coal|Volume` + `Trade|Primary Energy|Gas|Volume`
+ `Trade|Primary Energy|Oil|Volume`
+`Trade|Secondary Energy|Liquids|Oil|Volume`   + `Trade|Secondary Energy|Liquids|Biomass|Volume`   
+ `Trade|Secondary Energy|Solids|Biomass|Volume` 
+ `Trade|Secondary Energy|Electricity|Volume` 
+ `Trade|Secondary Energy|Hydrogen|Volume` 
+ `Trade|Secondary Energy|Liquids|Electricity|Volume` ", units = "EJ/yr", completeMissing = TRUE)

df <- calc_addVariable(df, "`Final Energy|Hydrogen Share woBunkers woNonEnergy`" = "(`Final Energy|Hydrogen` - `Final Energy|Bunkers|Hydrogen` - `Final Energy|Non-Energy Use|Hydrogen`) / (`Final Energy` - `Final Energy|Bunkers` - `Final Energy|Non-Energy Use`)", units = "%", completeMissing = TRUE)

# remove early time steps:  -->

df <- df %>%
  filter(  model != "PRIMES" | period != 2005,
           model != "TIAM-ECN" | ! (period %in% c(2005,2010,2015) )
  )




# remove MESSAGE -->

df <- filter(df,model != "MESSAGE")


df <- calc_addVariable(df, "`Final Energy|Hydrogen Share woBunkers woNonEnergy`" = "(`Final Energy|Hydrogen` - `Final Energy|Bunkers|Hydrogen` - `Final Energy|Non-Energy Use|Hydrogen`) / (`Final Energy` - `Final Energy|Bunkers` - `Final Energy|Non-Energy Use`)", units = "%", completeMissing = TRUE)



## additional variables also for historic -->

# Emissions|Kyoto Gases reduction vs 1990
histOrigN <- rbind(
  histOrigN,
  histOrigN %>% filter(region == "EUR", model == "UNFCCC", variable == "Emi|GHG|LULUCF national accounting") %>%
    mutate(variable = "Emissions|Kyoto Gases reduction vs 1990",
           value = 1 - (value / histOrigN %>% filter(region == "EUR", model == "UNFCCC", variable == "Emi|GHG|LULUCF national accounting", period == "1990") %>% pull(value) ) ),
  # EU27 used to carry a plain `Emi|GHG`; its successor in the newer mifs is the explicit
  # w/o bunkers variant (matches the old values to within the usual vintage drift).
  histOrigN %>% filter(region == "EU27", model == "UNFCCC", variable == "Emi|GHG|w/o Bunkers|LULUCF national accounting") %>%
    mutate(variable = "Emissions|Kyoto Gases|w/o Bunkers reduction vs 1990",
           value = 1 - (value / histOrigN %>% filter(region == "EU27", model == "UNFCCC", variable == "Emi|GHG|LULUCF national accounting", period == "1990") %>% pull(value) ) )
)

histOrigN <- calc_addVariable(histOrigN,"`Emi|Kyoto Gases|non-CO2 calc`" = "`Emi|GHG|LULUCF national accounting` - `Emi|CO2|LULUCF national accounting`", units = "MtCO2e/yr")


histOrigN <- calc_addVariable(histOrigN, "`SE|Electricity|WindSolar`" = "`SE|Electricity|Wind` + `SE|Electricity|Solar`", units = "EJ/yr")
histOrigN <- calc_addVariable(histOrigN, "`SE|Electricity|VRE Share`" = "`SE|Electricity|WindSolar` / `SE|Electricity`", units = "%")


######### filter charts data #############

if (filterChartsData){
  dfFull <- df
  tmp <- dfFull %>% filter(scenario == "WP1 NetZero", region == "EU27 & UK (*)", period >=2010, period <= 2050)
  
  df <- tmp %>%
    filter(
      model %in% modelsFullSystem,
      variable %in% c(
        "Emi|Kyoto Gases|synthetic",
        "Emissions|CO2|Energy and Industrial Processes",
        "Final Energy|Electricity Share woBunkers woNonEnergy",
        "Final Energy|Hydrogen Share woBunkers woNonEnergy",
        "Final Energy|Gases|Electricity",
        "Final Energy|Liquids|Electricity",
        "Final Energy|Gases|Biomass",
        "Final Energy|Liquids|Biomass",
        "Final Energy|Solids|Biomass",
        "Final Energy|Gases|Fossil",   
        "Final Energy|Liquids|Fossil",
        "Final Energy|Solids|Fossil",
        "FE|SolLiqGas",
        "Emissions|CO2|Energy|Demand|Bunkers",
        "Emissions|Kyoto Gases|non-CO2 synthetic",
        "Emissions|CO2|Other",
        "Emissions|CO2|Energy|Demand|AFOFI",
        "Emissions|CO2|Energy|Demand|Residential and Commercial",
        "Gross Emissions|CO2|Energy|Demand|Industry",
        "Gross Emissions|CO2|Industrial Processes",
        "Emissions|CO2|Energy|Demand|Transportation",
        "Gross Emissions|CO2|Energy|Supply|Other",
        "Gross Emissions|CO2|Energy|Supply|H2",
        "Gross Emissions|CO2|Energy|Supply|Solids",
        "Gross Emissions|CO2|Energy|Supply|Liquids",
        "Gross Emissions|CO2|Energy|Supply|Gases",
        "Gross Emissions|CO2|Energy|Supply|Heat",
        "Gross Emissions|CO2|Energy|Supply|Electricity",
        "Emissions|CO2|AFOLU synthetic",
        "Carbon Removal|Geological Storage|ForPlotting",
        "Emi|Kyoto Gases|synthetic",
        "Carbon Capture",
        "Mitigation costs")) %>%
    rbind(
      tmp %>%
        filter(
          model %in% modelsElecall,
          variable %in% c(
            "Emissions|CO2|Energy|Supply|Electricity",
            "Secondary Energy|Electricity|WindSolar",
            "Secondary Energy|Electricity|VRE Share",
            "Trade|Secondary Energy|Electricity|Volume",
            "Trade|Secondary Energy|Hydrogen|Volume",
            "Trade|Secondary Energy|Liquids|Electricity|Volume",
            "Trade|Secondary Energy|Liquids|Hydrogen|Volume",
            "Trade|Secondary Energy|Liquids|Biomass|Volume",
            "Trade|Secondary Energy|Solids|Biomass|Volume",
            "Trade|Primary Energy|Biomass|Volume",
            "Trade|Primary Energy|Gas|Volume",
            "Trade|Secondary Energy|Liquids|Oil|Volume",
            "Trade|Primary Energy|Oil|Volume",
            "Trade|Secondary Energy|Liquids|Coal|Volume",
            "Trade|Primary Energy|Coal|Volume",
            "Price|Carbon")
        )
    )
  
  histDataFull <- histOrigN
  tmp <- histDataFull %>% filter(region == "EUR", period >=1990)
  
  histData <- tmp %>%
    filter(
      model == "UNFCCC",
      variable %in% c(
        "Emi|GHG|w/ Bunkers|LULUCF national accounting", 
        "Emi|CO2|w/ Bunkers|Energy and Industrial Processes",
        "Emi|CO2|Energy|Demand|Transport|International Bunkers",
        "Emi|Kyoto Gases|non-CO2 calc",
        "Emi|CO2|Energy|Demand|Buildings",
        "Emi|CO2|Energy|Demand|Industry",
        "Emi|CO2|Industrial Processes",
        "Emi|CO2|w/o Bunkers|Energy|Demand|Transport",
        "Emi|CO2|Calc Other",
        "Emi|CO2|Energy|Supply|Solids w/ couple prod",
        "Emi|CO2|Energy|Supply|Liquids w/ couple prod",
        "Emi|CO2|Energy|Supply|Electricity and Heat",
        "Emi|CO2|Land-Use Change|LULUCF national accounting")
    ) %>%
    rbind(
      tmp %>%
        filter(
          model == "Ember",
          variable %in% c(
            "Emi|CO2|Energy|Supply|Electricity w/ couple prod",
            "SE|Electricity|WindSolar",
            "SE|Electricity|Nuclear",
            "SE|Electricity|Biomass",
            "SE|Electricity|Hydro",
            "SE|Electricity|VRE Share"
          )
        )
    ) %>%
    rbind(
      tmp %>%
        filter(
          model == "IEA",
          variable %in% c(
            "Final Energy|Electricity Share woBunkers woNonEnergy",
            "Trade|Gas",
            "Trade|Oil",
            "Trade|Coal"
          )
        )
    )
  
  saveRDS(df,"./data/results.rds")
  saveRDS(histData,"./data/historical.rds")
  
}
      

