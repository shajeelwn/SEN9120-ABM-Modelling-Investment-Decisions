extensions
[
  gis
  csv
  matrix
]

;;Global variables which are visible to everyone in the system
globals
[
  ;;Global time variables
  years
  days

  ;;Shape file variables
  germany-dataset
  world-patches

  ;;External data related variables
  market-data
  load-data
  opex-data
  capex-data

  ;;Electricity market data
  electricity-market-price
  effective-electricity-market-price

  ;;Daily demand data
  daily-load-data

  ;;Metrics
  total-carbon-footprint-of-system
]

;;The agents in the model
breed [investors investor]
breed [regulators regulator]

;;We use patches to attribute some geographical features to the model. In this case, it is the states of Germany and related wind-speed of them.
patches-own
[
  state-code
  state-name
  average-windspeed
  world?
]

investors-own
[
  ID                       ;;each investor has a unique ID
  investor-type            ;;investors can have the types such as "Follower", "Traditionalist", "Leader"
  random-value             ;;to generate instances for different structures, we are identifying a random number for each investor, and we are using this to randomize the data values we loaded externally (such as load data).

  invested?                ;;boolean value which shows whether the investor has invested before, or not.
  peer-effect-happened?    ;;boolean value which shows whether the investor has been affected by the peer effect or not.

  production-capacity      ;; The production capacity of an industrial park
  storage-capacity         ;; The storage capacity of an industrial park

  local-wind-speed         ;; Local wind speed based on the geographical position

  windpark-capacity                             ;; The constructed windpark capacity in an industrial park
  storing-capacity                              ;; The constructed storage capacity in an industrial park
  electricity-price-history                     ;; historical electricity price
  effective-electricity-price-history           ;; historical effective electricity price
  electricity-price-history-daily               ;; the daily data for historical electricity price
  effective-electricity-price-history-daily     ;; the daily data for historical effective electicity price
  electricity-price-trend                       ;; the trend of electricity price
  effective-electricity-price-trend             ;; the trend of effective electricity price
  forecasted-price                              ;; the price values which are forecasted by the investors
  effective-forecasted-price                    ;; the effective price values which are forecasted by the investors
  annual-energy-consumption                     ;; annual electricity consumption of the industrial parks
  annual-energy-sold                            ;; the annual amount of electricity which is sold to grid

  capex-cost               ;; capex for wind turbines
  wind-speed-capex         ;; this variables is used to find the related capex data in the csv file
  npv-counter              ;; this variable is used to iterate over the npv calculations
  npv-year                 ;; NPV for each year, it is stored in npv-list
  npv-list                 ;; the list of NPV for each year calculated
  npv                      ;; calculated NPV
  opex-cost                ;; opex for wind turbines

  discount                 ;; the discount factor which is used in the NPV calculations.
  discount-rate            ;; the yearly discount rate for investments
  capacity-ontop-factor    ;; the capacity factor values for wind parks

  investment               ;; the sum of capex and opex
  total-investment-list    ;; the list of investments introduced above
  total-investment         ;; the sum of the list elements introduced above

  generated-electricity                      ;; the amount of generated electricity daily
  demand-responsible                         ;; the amount of power needed to satisfy the daily demand
  demand-responsible-list                    ;; the list which stores the values of demand-responsible
  average-demand-responsible-year            ;; the yearly average value for demand-responsible
  amount-deficit                             ;; the amount of electricity which an industrial park is not able to satisfy by its generation
  sold-electricity                           ;; the amount of electricity which is sold to the grid
  bought-electricity                         ;; the amount of electricity which is bought from the grid
  available-storage                          ;; the available capacity of storage

  ROI                                        ;; Return on investment
  desired-ROI                                ;; The desired roi which changes with respect to the investor type
  sens-to-peers                              ;; The sensitivty of peers investment action on their on investment decision

  threshold-price                            ;; The price which is desired to have by the investors, if this is smaller than the real price, the investors are hesitant to sell their electricity to the grid.
  carbon-footprint                           ;; the amount of co2 emitted per day
  total-carbon-investor                      ;; Accumulated amount of carbon emission over a year.
]

regulators-own
[
  typeRegulator                       ;; identifies the type of regulator, whether green or center.

  first-year-total-load               ;; the sum of first year's load data
  total-emission-in-1990              ;; the amount of emitted co2 in 1990
  target-emission-in-2020             ;; the targeted emission levels for 2020
  target-emission-in-2030             ;; the targeted emission levels for 2030
  target-emission-in-2040             ;; the targeted emission levels for 2040
  target-emission-in-2050             ;; the targeted emission levels for 2050

  emission-reduction-target-2020      ;; the targeted reduction levels until 2020
  emission-reduction-target-2030      ;; the targeted reduction levels until 2030
  emission-reduction-target-2040      ;; the targeted reduction levels until 2040
  emission-reduction-target-2050      ;; the targeted reduction levels until 2050

  subsidy-amount                      ;; base amount of subsidy given by the regulators
  EEG-policy                          ;; the amount of subsidy increase based on regulator evaluations
  emission-forecast                   ;; the forecasted emission levels
  pseudo-emission-forecast-list       ;; the list of forecasted emission levels

  yearly-load-data-for-regulator      ;; the list which holds the yearly load data for regulator's use only
]

to setup
  ;;Starts with resetting the system, and time.
  clear-all
  file-close-all
  reset-ticks

  ;;Fixing the random seed for reproducability, this can be removed for experiments.
  random-seed 10

  ;;Initializing the relevant global variables
  set years 2008
  set days 0
  set total-carbon-footprint-of-system []

  ;;Calling the functions with the appropriate sequence.
  load-read-data
  map-germany
  identify-states
  initialize-regulator
  setting-load-data-for-regulator
  add-investors
  initialize-investors
  calculate-1990-emission-levels
end

to load-read-data
  ;;This function reads the csv files, then fills the lists in the model with the data it reads.
  file-open "time_series_2008_2065.csv"
  file-open "load_data.csv"
  file-open "OPEX.csv"
  file-open "CAPEX.csv"
  set market-data csv:from-file "time_series_2008_2065.csv"
  set load-data csv:from-file "load_data.csv"
  set opex-data csv:from-file "OPEX.csv"
  set capex-data csv:from-file "CAPEX.csv"
end
to map-germany
  ;;This function creates the map of Germany on the user-interface, shp files are used for this purpose.
  ask patches [set pcolor white]

  set germany-dataset gis:load-dataset "2500_NUTS1.shp"

  gis:set-world-envelope gis:envelope-of germany-dataset
  gis:set-transformation gis:world-envelope [-99.5 99.5 -139.5 139.5]

  ask patches gis:intersecting germany-dataset
  [set world? true]

  set world-patches patches with [world? = true]
  gis:apply-coverage germany-dataset "NUTS_CODE" state-code
  gis:set-drawing-color black
  gis:draw germany-dataset  1.0
end
to identify-states
  ;;This function identifies each province in Germany, and appends the average-windspeed respectively.
  ask patches
  [
    if state-code = "DE1"
    [set state-name "Baden-Württemberg"
     set average-windspeed 6.1
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DE2"
    [set state-name "Bayern"
     set average-windspeed 5.8
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DEA"
    [set state-name "Nordrhein-Westfalen"
     set average-windspeed 6.6
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DEB"
    [set state-name "Rheinland-Pfalz"
     set average-windspeed 6.4
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DEC"
    [set state-name "Saarland"
     set average-windspeed 6.3
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DE7"
    [set state-name "Hessen"
     set average-windspeed 6.4
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DE3"
    [set state-name "Berlin"
     set average-windspeed  6.7
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DE4"
    [set state-name "Brandenburg"
     set average-windspeed 6.7
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DED"
    [set state-name "Sachsen"
     set average-windspeed 6.2
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DEE"
    [set state-name "Sachsen-Anhalt"
     set average-windspeed 6.7
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DEG"
    [set state-name "Thüringen"
     set average-windspeed 6.4
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DE5"
    [set state-name "Bremen"
     set average-windspeed 7
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DE6"
    [set state-name "Hamburg"
     set average-windspeed 7
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DE8"
    [set state-name "Mecklenburg-Vorpommern"
     set average-windspeed 7.1
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DE9"
    [set state-name "Niedersachsen"
     set average-windspeed 7
     set pcolor 80 - (average-windspeed / 10 * 48) + 27]
    if state-code = "DEF"
    [set state-name "Schleswig-Holstein"
     set average-windspeed 7.3
     set pcolor 80 - average-windspeed]
  ]
end
to initialize-regulator
  ;;This function creates the regulator in the system, and sets the predefined attributes of it.
  create-regulators 1 [
    set typeRegulator one-of ["green" "center"]
    set xcor -95
    set ycor 135
    set shape "house"
    set size 10
    set color black
    set emission-reduction-target-2020 0.20  ;; By 2020, 20% reduction is aimed based on 1990 emission values.
    set emission-reduction-target-2030 0.40  ;; By 2030, 40% reduction is aimed based on 1990 emission values.
    set emission-reduction-target-2040 0.65  ;; By 2040, 65% reduction is aimed based on 1990 emission values.
    set emission-reduction-target-2050 0.90  ;; By 2050, 90% reduction is aimed based on 1990 emission values.
    set subsidy-amount 68.8   ;; €/MWh
    set emission-forecast []
  ]
end
to setting-load-data-for-regulator
  ;;This function creates the list of load data for the regulator. Then, this list is used by the regulator to calculate the 1990 demand data, and estimate the emission levels for 1990 in the function called "calculate-1990-emission-levels"
  ask regulators [
    set yearly-load-data-for-regulator []
    let iteration 0
    repeat 365 [
      set yearly-load-data-for-regulator lput ((item 0 item iteration load-data) * 24) yearly-load-data-for-regulator
      set iteration iteration + 1
    ]
  ]
end
to add-investors
  ;;This function creates the investors in the identified regions, the number of investors in each region is determined on the user-interface.
  ask n-of south-region patches with [state-code = "DE1" or state-code = "DE2"] [sprout-investors 1 [set shape "factory" set size 15] ]
  ask n-of east-region patches with [state-code = "DE3" or state-code = "DE4" or state-code = "DED" or state-code = "DEE" or state-code = "DEG"] [sprout-investors 1 [set shape "factory" set size 15] ]
  ask n-of west-region patches with [state-code = "DEA" or state-code = "DEB" or state-code = "DEC" or state-code = "DE7"] [sprout-investors 1 [set shape "factory" set size 15] ]
  ask n-of north-region patches with [state-code = "DE5" or state-code = "DE6" or state-code = "DE8" or state-code = "DE9" or state-code = "DEF"] [sprout-investors 1 [set shape "factory" set size 15] ]
end
to initialize-investors
  ;;This function sets the predefined attributes of the investors.
  ask investors
  [
    set random-value (((random 10) + 5 )/ 10)   ;;This random value is used to randomize certain investor attributes. For example, the demand data is for one industrial park, but by multiplying it with this random-value, we get individualized demand data for each industrial park.
    set invested? false
    set peer-effect-happened? false
    set electricity-price-history []
    set effective-electricity-price-history []
    set electricity-price-history-daily []
    set effective-electricity-price-history-daily []
    set demand-responsible-list []
    set electricity-price-trend []
    set effective-electricity-price-trend []
    set npv-list []
    set total-investment-list []
    set npv-counter 0
    set carbon-footprint []
    set total-carbon-investor 0

    ;;Each investor is assigned with their local wind speed which is based on the province it is placed on.
    set local-wind-speed average-windspeed

    ;;Below, 3 lines create a list of investor types with the given frequencies (by adding the same type again and again until the number of the type reaches the identified value on the user-interface).
    ;;Then, the 4th line chooses one of the types randomly, and assigns it to the investor. As a result, the investors in the model have the overall distribution which is identified on the user-interface.
    let investor-type-list (n-values prob-follower ["Follower"])
    set investor-type-list (sentence investor-type-list (n-values prob-traditionalist ["Traditionalist"]))
    set investor-type-list (sentence investor-type-list (n-values prob-leader ["Leader"]))
    set investor-type one-of investor-type-list

    ;;The ROI rates are assigned with respect to the investor types.
    if investor-type = "Leader" [
      set color 15
      set desired-ROI 0.04 + random-float 0.01
      set sens-to-peers 1
    ]
    if investor-type = "Follower" [
      set color 25
      set desired-ROI 0.06 + random-float 0.01
      set sens-to-peers 0.3 + random-float 0.2
    ]
    if investor-type = "Traditionalist" [
      set color 35
      set desired-ROI 0.1 + random-float 0.067
      set sens-to-peers 0.7 + random-float 0.2
    ]
  ]
end
to calculate-1990-emission-levels
  ;;This function has the regulator calculate the 1990 emission levels, then based on that value, it calculates the 2020, 2030, 2040, and 2050 emission targets.
  let total-coef (sum [random-value] of investors)
  ask regulators [
    set first-year-total-load (sum yearly-load-data-for-regulator) * total-coef   ;;in MWh

    ;; 1.1765: the ratio of electricity consumption per capita 1990 - 2017, 778 kg/MWh : the co2 emission in 1990
    set total-emission-in-1990 (first-year-total-load / 1.1765) * co2-per-MWh-1990      ;; in kilograms

    set target-emission-in-2020 total-emission-in-1990 * (1 - emission-reduction-target-2020)   ;; in kilograms
    set target-emission-in-2030 total-emission-in-1990 * (1 - emission-reduction-target-2030)   ;; in kilograms
    set target-emission-in-2040 total-emission-in-1990 * (1 - emission-reduction-target-2040)   ;; in kilograms
    set target-emission-in-2050 total-emission-in-1990 * (1 - emission-reduction-target-2050)   ;; in kilograms
  ]
end

to go
  tick

  ;;In each time tick, the processes start with reading and setting the related data sets
  read-marketprice
  read-load
  set-load-investors

  ;;Then the regulator type is determined if it is election time.
  conduct-election

  ;;Below, the short term operations are handled.
  ;;The investors calculate the desired market price, they generate electricity, satisfy the demand, and decide on
  ;;buying, selling or storing the electricity with respect to their calculations. At the end of the production and
  ;;buying selling process, the daily co2 emission is calculated, and the value is added to the accumulated value.
  calculate-threshold-price
  produce-electricity
  check-demand
  buy-sell-store-electricity-to-satisfy-demand
  calculate-carbon-footprint
  accumulated-carbon-footprint-yearly

  ;;Regulator checks the current performance of the system with respect to emissions, and extrapolates this behavior
  ;;to the future. Checks whether the current direction satisfies the targets in the future or not. If the regulator
  ;;predicts a failure in the future, it increases the subsidy by the amount fixed on the user-interface. If there is
  ;;success, then the regulator does not intervene in the system
  forecast-future-emission-performance
  check-the-emission-performance

  ;;The time updates
  set days days + 1
  if (remainder days 365 = 0)
  [
    set years years + 1
  ]

  ;;At the very beginning of new year, the investors forecast the future market conditions, and calculates the npv for
  ;;possible investment decisions. With respect to the results, they make an investment, or not.
  forecast-market
  decide-to-add-windparks
  decide-to-add-storage
  peer-effect

  ;;Stopping condition for the simulation run.
  if ticks = 15330 [stop]
end

to go-1-year
  ;;The same with "go" function. The only difference is that this "go-1-year" function runs for a year, then stops.
  ;;The user needs to click on the button of this function again and again to make the simulation continue.
  ;;The purpose is to be able to check each year's performance slowly on the user-interface.

  tick
  read-marketprice
  read-load
  set-load-investors

  conduct-election

  calculate-threshold-price
  produce-electricity
  check-demand
  buy-sell-store-electricity-to-satisfy-demand
  calculate-carbon-footprint
  accumulated-carbon-footprint-yearly

  forecast-future-emission-performance
  check-the-emission-performance

  set days days + 1
  if (remainder days 365 = 0)
  [
    set years years + 1
  ]

  forecast-market
  decide-to-add-windparks
  decide-to-add-storage
  peer-effect

  if remainder ticks 365 = 0 [stop]
end

to read-marketprice
  ;;This function reads the market data and sets the market price values within the model.
  if file-at-end? [stop]
  set electricity-market-price item 3 item (ticks + 1) market-data
  set effective-electricity-market-price item 4 item (ticks + 1) market-data
end
to read-load
  ;;This function reads the load data and sets the load values within the model.
  if file-at-end? [stop]
  set daily-load-data item 0 item ticks load-data
  ask investors[
    set demand-responsible (daily-load-data * (random-value))
  ]
end
to set-load-investors
  ;;This function fills the list of load values, and then get the yearly average
  ask investors[
    set demand-responsible-list lput demand-responsible demand-responsible-list
    if ticks = 365[set average-demand-responsible-year mean demand-responsible-list]
  ]
end



;;Short-term operations
to conduct-election
  ;;This function changes the type of the regulator randomly in every 4 years.
  if (remainder years 4 = 2) and (remainder days 365 = 0) [ask regulators [set typeRegulator one-of ["green" "center"]]
    if-else [typeRegulator] of one-of regulators = "green" [ask regulators [set color green]][ask regulators [set color black]]
  ]
end
to calculate-threshold-price
  ;;Investors check the past 3 days' market prices, get the mean value, and expect at least this amount as the threshold price for their selling operations.
  if-else days <= 5
  [
    ask investors [set threshold-price electricity-market-price]
  ]
  [
    let pseudo-price-list []
    set pseudo-price-list lput item 3 item (days + 1) market-data pseudo-price-list
    set pseudo-price-list lput item 3 item (days) market-data pseudo-price-list
    set pseudo-price-list lput item 3 item (days - 1) market-data pseudo-price-list

    ask investors [set threshold-price mean pseudo-price-list]
  ]
end
to produce-electricity
  ;;This function generates daily electricity
  ask investors[
    set generated-electricity (production-capacity) * 24
  ]
end
to check-demand
  ;;This function checks the demand and evaluates whether the deficit exists or not.
  ask investors[
    if-else generated-electricity >= (demand-responsible * 24)
      [set amount-deficit 0]
      [set amount-deficit (demand-responsible * 24) - generated-electricity]
  ]
end
to buy-sell-store-electricity-to-satisfy-demand
  ;;In this function, investors decide on buying electricity from grid, selling electricity to the grid, or storing
  ;;electricity for future use.
  ask investors[
    if (amount-deficit > 0) [
      if-else (available-storage >= amount-deficit)
        [set available-storage available-storage - amount-deficit
         set sold-electricity 0
         set bought-electricity 0]
         [set bought-electricity amount-deficit - available-storage
         set available-storage 0
         set sold-electricity 0]
    ]

    if (amount-deficit = 0) [

      calculate-threshold-price

      if-else (threshold-price >= electricity-market-price)
        [set sold-electricity 0
         set bought-electricity 0
         set available-storage available-storage + (generated-electricity - (demand-responsible * 24))

         if storage-capacity < available-storage
           [set sold-electricity available-storage - storage-capacity
            set available-storage storage-capacity]
        ]
        [set bought-electricity 0
         set sold-electricity available-storage + (generated-electricity - (demand-responsible * 24))
         set available-storage 0]
    ]
  ]
end
to calculate-carbon-footprint
  ;;This function calculates the carbon footprint of investors.
  ask investors
  [
    ;;Currently renewable energy use is around 45%, and 560 kg CO2 is emitted per MWh.
    ;;Currently, it is assumed that this situation is going to be around the same value for the future, but this can be changed via the sliders on the user-interface.
    if (years < 2010) [set carbon-footprint lput ( bought-electricity * co2-per-MWh-1990 ) carbon-footprint]
    if (years >= 2010) and (years < 2020) [set carbon-footprint lput ( bought-electricity * co2-per-MWh-2010 ) carbon-footprint]
    if (years >= 2020) and (years < 2030) [set carbon-footprint lput ( bought-electricity * co2-per-MWh-2020 ) carbon-footprint]
    if (years >= 2030) and (years < 2040) [set carbon-footprint lput ( bought-electricity * co2-per-MWh-2030 ) carbon-footprint]
    if (years >= 2040) [set carbon-footprint lput ( bought-electricity * co2-per-MWh-2040 ) carbon-footprint]
    set total-carbon-investor sum carbon-footprint
  ]
end
to accumulated-carbon-footprint-yearly
  ;;This function calculates the yearly accumulated carbon emission value of the system.
  ;;This value is used by the regulators to determine the system's emission performance.
  if ((remainder days 365) = 0) and (days > 0)
    [
      set total-carbon-footprint-of-system lput (sum [total-carbon-investor] of investors) total-carbon-footprint-of-system
      ask investors [
        set carbon-footprint []
        set total-carbon-investor 0
      ]
    ]
end
to forecast-future-emission-performance
  ;;This function forecasts the emission levels for future years. Later, these forecasted values are used to
  ;;evaluate the system performance.
  if ((remainder years 4) = 0) and (years >= 2012) and (remainder days 365 = 0)
  [
    ask regulators
    [
      set pseudo-emission-forecast-list total-carbon-footprint-of-system

      ;;60years is just a random time limit which is larger than the project's scope for future exploration
      repeat 60
      [
        let next-year-forecast matrix:forecast-linear-growth pseudo-emission-forecast-list
        set pseudo-emission-forecast-list lput item 0 next-year-forecast pseudo-emission-forecast-list
      ]
    ]
  ]
end
to check-the-emission-performance
  ;;This function takes active part in the 1st year of the term of government, and in the 3rd year of the term of government.
  ;;It increases the subsidy value if regulator predicts that the future targets are not satisfied with the current direction
  ;;and so it makes the invesment more advantageous.
  ask regulators
  [
    if (remainder years 4 = 2) and (years > 2018) and (remainder days 365 = 0)
    ;;We assumed that the EEG increase should be at the same moment with governmental change.
    [
      if (years <= 2020) and (item 12 pseudo-emission-forecast-list > target-emission-in-2020)  [increase-EEG]
      if (years > 2020) and (years <= 2030) and (item 22 pseudo-emission-forecast-list > target-emission-in-2030)  [increase-EEG]
      if (years > 2030) and (years <= 2040) and (item 32 pseudo-emission-forecast-list > target-emission-in-2040)  [increase-EEG]
      if (years > 2040) and (years <= 2050) and (item 42 pseudo-emission-forecast-list > target-emission-in-2050)  [increase-EEG]
    ]
  ]
end
to increase-EEG
  ;;This function increases the current subsidy value based on the type of regulator.
  ;;The amount of increase is fixed and it is set on the user-interface.
  ask regulators [
    if-else [typeRegulator] of one-of regulators = "green"
    [set EEG-policy EEG-policy + green-EEG-increase]
    [set EEG-policy EEG-policy + center-EEG-increase]
  ]
end



;;Long-term operations
to forecast-market
  ;;This function includes shortly everything related to investor's long-term operations.
  ;;The investors check the historical market prices, forecasts the future, calculates the related npv based on forecasts
  ;;and then decides whether investing is viable or not.
  ask investors[

    ;;This condition ensures that the investment is done only once.
    if invested? = false [

    set electricity-price-history-daily lput electricity-market-price electricity-price-history-daily
    set effective-electricity-price-history-daily lput effective-electricity-market-price effective-electricity-price-history-daily

    if ( remainder ticks 365 = 0 )[
      let annual-electricity-price-history (mean electricity-price-history-daily)
      let annual-effective-electricity-price-history (mean effective-electricity-price-history-daily)

      set electricity-price-history lput annual-electricity-price-history electricity-price-history
      set effective-electricity-price-history lput annual-effective-electricity-price-history effective-electricity-price-history
      set electricity-price-history-daily []
      set effective-electricity-price-history-daily []
    ]
    set electricity-price-trend electricity-price-history
    set effective-electricity-price-trend effective-electricity-price-history
		

    ;;Investment decision is below
    ifelse without-storage[
        if ticks > 2919 [
          let i 0
          if ( remainder ticks 365 = 0 ) [
            while [i < 16][
              set forecasted-price item 0 matrix:forecast-linear-growth electricity-price-trend
              set electricity-price-trend  lput forecasted-price electricity-price-trend
              set effective-forecasted-price item 0 matrix:forecast-linear-growth effective-electricity-price-trend
              set effective-electricity-price-trend  lput effective-forecasted-price effective-electricity-price-trend
              set i i + 1]

            if-else (remainder average-demand-responsible-year onshore-wind-size > 0)
            [set windpark-capacity average-demand-responsible-year + onshore-wind-size]
            [set windpark-capacity average-demand-responsible-year]
            set annual-energy-consumption average-demand-responsible-year * 24 * 365 * (mean effective-electricity-price-trend)
            set annual-energy-sold (windpark-capacity - average-demand-responsible-year) * 24 * 365 * ((mean electricity-price-trend) + ([subsidy-amount] of one-of regulators + ([EEG-policy] of one-of regulators)))

            ;;CAPEX
            if (local-wind-speed >= 8.7) [set wind-speed-capex 1 set capacity-ontop-factor 0.474]
            if (local-wind-speed >= 8.4) and (local-wind-speed < 8.7) [set wind-speed-capex 2 set capacity-ontop-factor 0.462]
            if (local-wind-speed >= 8.2) and (local-wind-speed < 8.4) [set wind-speed-capex 3 set capacity-ontop-factor 0.45]
            if (local-wind-speed >= 7.9) and (local-wind-speed < 8.2) [set wind-speed-capex 4 set capacity-ontop-factor 0.43]
            if (local-wind-speed >= 7.5) and (local-wind-speed < 7.9) [set wind-speed-capex 5 set capacity-ontop-factor 0.407]
            if (local-wind-speed >= 6.9) and (local-wind-speed < 7.5) [set wind-speed-capex 6 set capacity-ontop-factor 0.364]
            if (local-wind-speed >= 6.2) and (local-wind-speed < 6.9) [set wind-speed-capex 7 set capacity-ontop-factor 0.308]
            if (local-wind-speed >= 5.5) and (local-wind-speed < 6.2) [set wind-speed-capex 8 set capacity-ontop-factor 0.246]
            if (local-wind-speed >= 4.8) and (local-wind-speed < 5.5) [set wind-speed-capex 9 set capacity-ontop-factor 0.183]
            if (local-wind-speed >= 4.0) and (local-wind-speed < 4.8) [set wind-speed-capex 10 set capacity-ontop-factor 0.111]

            repeat 15[
              let capex-iteration (0 + npv-counter)
              set capex-cost ((item capex-iteration item wind-speed-capex CAPEX-data) / 15) * 1000 * round(windpark-capacity / capacity-ontop-factor)

              ;;OPEX
              let opex-iteration (0 + npv-counter)
              set opex-cost (item opex-iteration item 1 OPEX-data) * 1000 * windpark-capacity / capacity-ontop-factor
              set opex-iteration opex-iteration + 1

              ;;discount-rate
              let dr-iteration 1
              set discount ((1 + discount-rate) ^ dr-iteration)
              set dr-iteration dr-iteration + 1

              set npv-year (annual-energy-consumption - capex-cost - opex-cost + annual-energy-sold) / discount
              set investment (capex-cost + opex-cost)

              set total-investment-list lput investment total-investment-list
              set npv-list lput npv-year npv-list
            ]
            set npv sum npv-list
            set total-investment sum total-investment-list

            set ROI npv / total-investment

            set npv-list []
            set total-investment-list []
            set electricity-price-trend []
            set effective-electricity-price-trend []
            set npv-counter npv-counter + 1
          ]
        ]
    ]

    ;;The same logic with the above, but here the storage option is included.
    [
      if ticks > 2919 [
      let i 0
      if ( remainder ticks 365 = 0 ) [
        while [i < 16][
          set forecasted-price item 0 matrix:forecast-linear-growth electricity-price-trend
          set electricity-price-trend  lput forecasted-price electricity-price-trend
          set effective-forecasted-price item 0 matrix:forecast-linear-growth effective-electricity-price-trend
          set effective-electricity-price-trend  lput effective-forecasted-price effective-electricity-price-trend
          set i i + 1]

        if-else (remainder average-demand-responsible-year onshore-wind-size > 0)
        [set windpark-capacity average-demand-responsible-year + onshore-wind-size
         set storing-capacity 750]
        [set windpark-capacity average-demand-responsible-year
         set storing-capacity 750]
        set annual-energy-consumption average-demand-responsible-year * 24 * 365 * (mean effective-electricity-price-trend)
            set annual-energy-sold (windpark-capacity - average-demand-responsible-year) * 24 * 365 * ((mean electricity-price-trend + 10) + ([subsidy-amount] of one-of regulators + ([EEG-policy] of one-of regulators)))

        ;;CAPEX
        if (local-wind-speed >= 8.7) [set wind-speed-capex 1 set capacity-ontop-factor 0.474]
        if (local-wind-speed >= 8.4) and (local-wind-speed < 8.7) [set wind-speed-capex 2 set capacity-ontop-factor 0.462]
        if (local-wind-speed >= 8.2) and (local-wind-speed < 8.4) [set wind-speed-capex 3 set capacity-ontop-factor 0.45]
        if (local-wind-speed >= 7.9) and (local-wind-speed < 8.2) [set wind-speed-capex 4 set capacity-ontop-factor 0.43]
        if (local-wind-speed >= 7.5) and (local-wind-speed < 7.9) [set wind-speed-capex 5 set capacity-ontop-factor 0.407]
        if (local-wind-speed >= 6.9) and (local-wind-speed < 7.5) [set wind-speed-capex 6 set capacity-ontop-factor 0.364]
        if (local-wind-speed >= 6.2) and (local-wind-speed < 6.9) [set wind-speed-capex 7 set capacity-ontop-factor 0.308]
        if (local-wind-speed >= 5.5) and (local-wind-speed < 6.2) [set wind-speed-capex 8 set capacity-ontop-factor 0.246]
        if (local-wind-speed >= 4.8) and (local-wind-speed < 5.5) [set wind-speed-capex 9 set capacity-ontop-factor 0.183]
        if (local-wind-speed >= 4.0) and (local-wind-speed < 4.8) [set wind-speed-capex 10 set capacity-ontop-factor 0.111]

        repeat 15[
          let capex-iteration (0 + npv-counter)
          set capex-cost (((item capex-iteration item wind-speed-capex CAPEX-data) / 15) * 1000 * round(windpark-capacity / capacity-ontop-factor)) + ((1200000 / 15) * storing-capacity)

          ;;OPEX
          let opex-iteration (0 + npv-counter)
          set opex-cost ((item opex-iteration item 1 OPEX-data) * 1000 * windpark-capacity / capacity-ontop-factor) + (36000 * storing-capacity)
          set opex-iteration opex-iteration + 1

          ;;discount-rate
          let dr-iteration 1
          set discount ((1 + discount-rate) ^ dr-iteration)
          set dr-iteration dr-iteration + 1

          set npv-year (annual-energy-consumption - capex-cost - opex-cost + annual-energy-sold) / discount
          set investment (capex-cost + opex-cost)

          set total-investment-list lput investment total-investment-list
          set npv-list lput npv-year npv-list
        ]
        set npv sum npv-list
        set total-investment sum total-investment-list

        set ROI npv / total-investment

        set npv-list []
        set total-investment-list []
        set electricity-price-trend []
        set effective-electricity-price-trend []
        set npv-counter npv-counter + 1
    ]
   ]
  ]
 ]
]
end
to decide-to-add-windparks
  ;;When the decision is made to add windparks, the related production capacity of an industrial park is updated.
  ask investors[
    if ROI >= desired-ROI[
      set production-capacity windpark-capacity
      set invested? true
      set color yellow
    ]
  ]
end
to decide-to-add-storage
  ;;When the decision is made to add storages, the related storage capacity of an industrial park is updated.
  ask investors[
    if ROI >= desired-ROI[
      set storage-capacity storing-capacity
      set invested? true
      set color yellow
    ]
  ]
end
to peer-effect
  ;;Peer effect is activated by this function. Expected ROI of investors is updated with respect to other investors' actions.
  ask investors[
    if peer-effect-happened? = false[
      if ( count (investors with [invested? = true]) / count investors ) >= sens-to-peers[
        set desired-ROI desired-ROI * sens-to-peers
        ;if investor-type = "Follower" [set desired-ROI desired-ROI * sens-to-peers]
        ;if investor-type = "Traditionalist" [set desired-ROI desired-ROI * sens-to-peers]
        set peer-effect-happened? true
      ]
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
274
55
781
762
-1
-1
2.484
1
10
1
1
1
0
0
0
1
-100
100
-140
140
0
0
1
ticks
100.0

BUTTON
4
10
74
43
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
6
688
133
748
west-region
10.0
1
0
Number

INPUTBOX
135
627
262
687
east-region
10.0
1
0
Number

INPUTBOX
135
688
262
748
south-region
10.0
1
0
Number

INPUTBOX
6
627
133
687
north-region
10.0
1
0
Number

BUTTON
80
10
143
43
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
788
10
1208
285
Load of Industrialparks
hour
load
0.0
30.0
0.0
30.0
true
true
"" "ask investors[\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy ticks demand-responsible\n]"
PENS

PLOT
788
285
1208
560
npv
years
electricity-price-trend
2008.0
2050.0
0.0
10.0
true
true
"" "ask investors[\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy years npv\n]"
PENS

MONITOR
274
10
781
55
NIL
years
17
1
11

SLIDER
5
251
266
284
onshore-wind-size
onshore-wind-size
2.5
4.5
3.5
1
1
MW
HORIZONTAL

PLOT
1210
10
1608
285
ROI
years
ROI-rate
2008.0
2050.0
0.0
1.0
true
true
"" "ask investors[\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy years ROI\n]"
PENS

SLIDER
5
467
266
500
prob-follower
prob-follower
0
100
70.0
5
1
%
HORIZONTAL

SLIDER
5
503
266
536
prob-traditionalist
prob-traditionalist
0
100
15.0
5
1
%
HORIZONTAL

SLIDER
5
539
266
572
prob-leader
prob-leader
0
100
15.0
5
1
%
HORIZONTAL

SLIDER
5
287
266
320
green-EEG-increase
green-EEG-increase
0
50
40.0
1
1
€/MWh
HORIZONTAL

SLIDER
5
323
266
356
center-EEG-increase
center-EEG-increase
0
50
10.0
1
1
€/MWh
HORIZONTAL

SWITCH
5
431
266
464
without-storage
without-storage
0
1
-1000

PLOT
1210
285
1608
560
EEG
NIL
NIL
0.0
10.0
0.0
0.5
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? regulators [plot [EEG-policy] of one-of regulators]"

BUTTON
149
10
236
43
NIL
go-1-year\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
5
47
267
80
co2-per-MWh-1990
co2-per-MWh-1990
0
1000
777.0
1
1
kg of CO2 / MWh
HORIZONTAL

SLIDER
5
80
267
113
co2-per-MWh-2010
co2-per-MWh-2010
0
1000
560.0
1
1
kg of CO2 / MWh
HORIZONTAL

SLIDER
5
113
267
146
co2-per-MWh-2020
co2-per-MWh-2020
0
1000
560.0
1
1
kg of CO2 / MWh
HORIZONTAL

SLIDER
5
146
267
179
co2-per-MWh-2030
co2-per-MWh-2030
0
1000
560.0
1
1
kg of CO2 / MWh
HORIZONTAL

SLIDER
5
179
267
212
co2-per-MWh-2040
co2-per-MWh-2040
0
1000
560.0
1
1
kg of CO2 / MWh
HORIZONTAL

PLOT
788
560
1208
827
Yearly Total Carbon Emission of the System
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Emission" 1.0 0 -16777216 true "" "plot sum [total-carbon-investor] of investors"
"Target2020" 1.0 0 -13840069 true "" "if any? regulators [plot [target-emission-in-2020] of one-of regulators]"
"Target2030" 1.0 0 -1184463 true "" "if any? regulators [plot [target-emission-in-2030] of one-of regulators]"
"Target2040" 1.0 0 -11221820 true "" "if any? regulators [plot [target-emission-in-2040] of one-of regulators]"
"Target2050" 1.0 0 -2674135 true "" "if any? regulators [plot [target-emission-in-2050] of one-of regulators]"

TEXTBOX
7
586
157
628
Number of industrial parks in each region:
14
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
