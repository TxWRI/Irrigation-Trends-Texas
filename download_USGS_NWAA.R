## download and format USGS NWAA data for Texas

## probably need to use arrow for this
library(readr)
library(arrow)
library(httr)
library(dplyr)
library(units)


web_paths <- c(
  "https://api.water.usgs.gov/nwaa-data/data?model=wu-irrigation-wd&variable=irrwdgw,irrwdsw,irrwdtot&timeRes=annualcy&startDate=2000&endDate=2020&location=statecd:tx&format=csv&origin=ssdt&intersection=envelop&skip=0",
  "https://api.water.usgs.gov/nwaa-data/data?model=wu-irrigation-wd&variable=irrwdgw,irrwdsw,irrwdtot&timeRes=annualcy&startDate=2000&endDate=2020&location=statecd:tx&format=csv&origin=ssdt&intersection=envelop&skip=600",
  "https://api.water.usgs.gov/nwaa-data/data?model=wu-irrigation-wd&variable=irrwdgw,irrwdsw,irrwdtot&timeRes=annualcy&startDate=2000&endDate=2020&location=statecd:tx&format=csv&origin=ssdt&intersection=envelop&skip=1200",
  "https://api.water.usgs.gov/nwaa-data/data?model=wu-irrigation-wd&variable=irrwdgw,irrwdsw,irrwdtot&timeRes=annualcy&startDate=2000&endDate=2020&location=statecd:tx&format=csv&origin=ssdt&intersection=envelop&skip=1800",
  "https://api.water.usgs.gov/nwaa-data/data?model=wu-irrigation-wd&variable=irrwdgw,irrwdsw,irrwdtot&timeRes=annualcy&startDate=2000&endDate=2020&location=statecd:tx&format=csv&origin=ssdt&intersection=envelop&skip=2400",
  "https://api.water.usgs.gov/nwaa-data/data?model=wu-irrigation-wd&variable=irrwdgw,irrwdsw,irrwdtot&timeRes=annualcy&startDate=2000&endDate=2020&location=statecd:tx&format=csv&origin=ssdt&intersection=envelop&skip=3000",
  "https://api.water.usgs.gov/nwaa-data/data?model=wu-irrigation-wd&variable=irrwdgw,irrwdsw,irrwdtot&timeRes=annualcy&startDate=2000&endDate=2020&location=statecd:tx&format=csv&origin=ssdt&intersection=envelop&skip=3600",
  "https://api.water.usgs.gov/nwaa-data/data?model=wu-irrigation-wd&variable=irrwdgw,irrwdsw,irrwdtot&timeRes=annualcy&startDate=2000&endDate=2020&location=statecd:tx&format=csv&origin=ssdt&intersection=envelop&skip=4200",
  "https://api.water.usgs.gov/nwaa-data/data?model=wu-irrigation-wd&variable=irrwdgw,irrwdsw,irrwdtot&timeRes=annualcy&startDate=2000&endDate=2020&location=statecd:tx&format=csv&origin=ssdt&intersection=envelop&skip=4800",
  "https://api.water.usgs.gov/nwaa-data/data?model=wu-irrigation-wd&variable=irrwdgw,irrwdsw,irrwdtot&timeRes=annualcy&startDate=2000&endDate=2020&location=statecd:tx&format=csv&origin=ssdt&intersection=envelop&skip=5400"
)


download_nwaa <- function(url, filepath) {
  response <- httr::GET(url)
  con <- file(filepath, "wb")
  writeBin(content(response, "raw"), con)
  close(con)  # Close the file connection
}

for(i in 1:length(web_paths)) {
  tryCatch(
    download_nwaa(url = web_paths[i],
                  filepath = paste0("Data/usgs/nwaa/file_", i, ".csv"))
  )
}




ds <- arrow::open_csv_dataset("Data/usgs/nwaa/")
                      
ds <- ds |> 
  group_by(year) |> 
  summarise(total_irrigative_withdrawals = sum(irrwdtot_mgd),
            gw_irrigative_withdrawals = sum(irrwdgw_mgd),
            sw_irrigative_withdrawals = sum(irrwdsw_mgd)) |> 
  collect()

ds <- ds |> 
  mutate(total_irrigative_withdrawals = as_units(total_irrigative_withdrawals,
                                                 "1000000 gallons/day"),
         gw_irrigative_withdrawals = as_units(gw_irrigative_withdrawals,
                                              "1000000 gallons/day"),
         sw_irrigative_withdrawals = as_units(sw_irrigative_withdrawals,
                                              "1000000 gallons/day")) |> 
  mutate(total_irrigative_withdrawals = set_units(total_irrigative_withdrawals,
                                                  "acre_feet/year"),
         gw_irrigative_withdrawals = set_units(gw_irrigative_withdrawals,
                                               "acre_feet/year"),
         sw_irrigative_withdrawals = set_units(sw_irrigative_withdrawals,
                                               "acre_feet/year")) |> 
  mutate(total_irrigative_withdrawals = as.numeric(total_irrigative_withdrawals),
         gw_irrigative_withdrawals = as.numeric(gw_irrigative_withdrawals),
         sw_irrigative_withdrawals = as.numeric(sw_irrigative_withdrawals))

ds
readr::write_csv(ds, "Data/usgs/cleaned_annual_usgs_withdrawals.csv")
