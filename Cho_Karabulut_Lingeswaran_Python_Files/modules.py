import random, time
import numpy as np
import pandas as pd
        
class Calculation():
    """Calculation class provides functions to calculate NPV
    
    extrapolate: forecast future price given the historical data
    
    calculate: calculate net gains over years
    
    forecast: estimate NPV given all the yearly operation
    
    """
    def __init__(self, model, current_year, capacity, windspeed, subsidy, time_series_data):
        self.model = model
        self.grid_charge = .25
        self.current_year = current_year
        self.capacity = capacity
        self.windspeed = windspeed
        self.subsidy = subsidy
        self.time_series_data = time_series_data
        
        self.df = self.calculate3()
        self.df_summation = self.forecast2()
        
    # polynomial extrapolation
    def extrapolate(self, obj, current_tick):
        degree, ticks_in_year, year_ahead = 1, 365, 15
        # load historical data
        data = self.time_series_data[self.time_series_data["tick"] <= current_tick][["year", "tick", obj]].values
        # fit curve
        fit = np.polyfit(data[:,1], data[:,2] , degree)
        line = np.poly1d(fit)
        # future ticks to be estimated
        new_xs = np.arange(current_tick+1, current_tick+(year_ahead*ticks_in_year)+1)
        # extrapolate
        new_ys = line(new_xs)

        data = np.zeros((len(new_xs), 3))
        data[:, 1:3] = np.array([new_xs, new_ys]).T
        data[:, 0] = np.array(2019 + (data[:, 1]-1)/ticks_in_year, dtype=int)

        return pd.DataFrame(data, columns=["year", "tick", obj])

    def calculate3(self):
        current_tick = self.time_series_data[self.time_series_data["year"] == self.current_year].loc[0, "tick"] - 1
        df = self.extrapolate("market price", current_tick).to_dict()
        df["market price eff"] = {k: df["market price"][k] + 0.25 for k in range(5475)}
        df["demand"] = dict(zip(range(5475),np.tile(self.model.demand * self.capacity / 35, 15)))
        df["capacity"] = self.capacity
        df["surplus"] = {k: max(0, df["capacity"] - df["demand"][k]) for k in range(5475)}
        df["consumption from windpower"] = {k: min(self.capacity, df["demand"][k]) for k in range(5475)}
        df["saved money"] = {k: df["consumption from windpower"][k] * df["market price eff"][k] * 24 for k in range(5475)}
        df["sold"] = {k: df["surplus"][k] * df["market price"][k] * 24 for k in range(5475)}
        df["added value"] = {k: df["sold"][k] + df["saved money"][k] for k in range(5475)}
        df["subsidy"] = {k: df["surplus"][k] * self.model.subsidy * 240 for k in range(5475)}
        df["net gain"] = {k: df["added value"][k] + df["subsidy"][k] for k in range(5475)}

        return df

    def forecast2(self):
        tech1 = self.model.tech
        exchange_rate = self.model.exchange_rate
        discount_rate = self.model.discount_rate
        
        df_summ = {}

        vals = np.zeros(shape=(15, 3))

        # net gain
        vals[:, 0] = np.array(list(self.df["net gain"].values())).reshape(-1, 365).sum(axis=1)

        # capex
        tech1_windspeed = tech1[["CF", "wind speed"]]
        tech1_windspeed.columns = tech1_windspeed.columns.droplevel(1)
        windspeed = np.max([self.windspeed, tech1_windspeed["wind speed"].min()])
        tech1_windspeed["wind speed"] <= windspeed
        cap_factor = tech1_windspeed[tech1_windspeed["wind speed"] <= windspeed].iloc[0]["CF"]
        ind1 = tech1_windspeed[tech1_windspeed["wind speed"] <= windspeed].index[0]
        ind2 = tech1["Weighted Average CAPEX ($/kW)"].loc[ind1].index < self.current_year
        capex = tech1["Weighted Average CAPEX ($/kW)"].loc[ind1][ind2].iloc[-1]
        total_capex = capex / cap_factor
        yearly_capex = - self.capacity * 1000 * total_capex / 15
        capex = yearly_capex * exchange_rate
        vals[:, 1] = yearly_capex * exchange_rate

        #opex
        opex = tech1["Weighted Average OPEX ($/kW/yr)"].loc[ind1] * exchange_rate
        d = dict(zip(range(2016, 2065), np.zeros(49)))
        d.update(opex.to_dict())
        for k in d:
            if d[k] == 0:
                d[k] = d[k-1]
            else:
                d[k] = d[k]
        opex = [v * self.capacity * -1000 for k, v in d.items() if k >= self.current_year and k < self.current_year+15]
        vals[:, 2] = opex

        vals = vals.T / np.power(discount_rate, range(1, 16))
        vals.sum()/1000000

        self.npv = vals.sum(axis=0).sum() / 1000000
        self.invest = - vals[1:3].sum() / 1000000
        self.revenue = vals[0].sum() / 1000000

        df_summ["net gain"] = dict(zip(range(self.current_year), vals[0]))
        df_summ["capex"] = dict(zip(range(self.current_year), vals[1]))
        df_summ["opex"] = dict(zip(range(self.current_year), vals[2]))
        
        return df_summ

class Model():
    def __init__(self, subsidy=6.88):
        self.tick = 0
        self.current_year = 2019
        self.unit_tick = 365
        self.name = "Model"
        self.exchange_rate = 0.88
        self.n_investor = 50
        self.discount_rate = 1.05

        self.subsidy = subsidy
        self.investors_list = []
        self.investors_profile = pd.DataFrame()
                
        self.invested_proportion = {}
        self.subsidy_overtime = {}
        
        self.log = []
        
        logging(self, "is initialized.")
        self.tech = pd.read_excel("data.xlsx", sheet_name="technology1", header=(0, 1))

        self.investor_typedata = pd.read_excel("data.xlsx", sheet_name="investors")
        self.investor_state = pd.read_excel("data.xlsx", sheet_name="state", index_col=0)

        self.time_series_data = pd.read_csv("time_series_2008_2065.csv", index_col=0)
        self.demand = self.time_series_data[self.time_series_data["year"] == 2019]["demand"].values
        self.unit_demand = self.time_series_data[self.time_series_data["year"] == 2019]["demand"].mean()

    def setup(self, seed=0):
        self.populate()

        logging(self, "is setup.")

        
    def populate(self):
        logging(self, "populates %s Investors."%self.n_investor)

        for i in range(self.n_investor):
            investor = Investor(index=i, model=self)
            self.investors_list.append(investor)
        
        self.investors_profile["name"] = [i.name for i in self.investors_list]
        self.investors_profile["type"] = [i.type for i in self.investors_list]
        self.investors_profile["capacity"] = [i.capacity for i in self.investors_list]        
        self.investors_profile["thresh"] = [i.thresh for i in self.investors_list]
        self.investors_profile["sen-to-peers"] = [i.sen_to_peers for i in self.investors_list]
        
        self.investors_profile["state"] = [i.state for i in self.investors_list]
        self.investors_profile["windspeed_avg"] = [i.windspeed_avg for i in self.investors_list]
        self.investors_profile["windspeed"] = [i.windspeed for i in self.investors_list]
        self.investors_profile["yearly_demand(MWh)"] = [i.demand * 24 * 365 for i in self.investors_list]
        self.investors_profile["invested"] = False
        self.investors_profile["invest_year"] = 0
        self.investors_profile["subsidy at invest year"] = 0
        
        self.elec_bought_log = pd.DataFrame(index=self.investors_profile["name"])
        self.investors_profile.set_index("name", inplace=True)
        
        logging(self, "populates a Regulator.")
        
        r = Regulator(self)
        self.regulator = r
        
    def update(self):
        self.log.append(["===="]*3)
        logging(self, "starts updates")
        
        # update all investors in the system to proceed operation at the year
        for i in self.investors_list:
            i.operate_update()

            bought = np.sum([v["to be bought"] for v in i.operation_book_one.values()])            
            self.elec_bought_log.loc[i.name, self.current_year] = bought
        
        # update regulator
        self.regulator.update()
        self.subsidy_overtime[self.current_year] = self.subsidy

        # update all investors in the system to estimate NPV
        investors_list = self.investors_list.copy()
        random.shuffle(self.investors_list)
        for i in self.investors_list:
            # update only if did not invest yet
            if not i.invested:
                i.invest_update()
                
        # archive the result of the year
        self.investors_list = investors_list
        self.investors_profile["estimation_%s"%self.current_year] = [i.estimation for i in self.investors_list]
        self.investors_profile["estimation_%s"%self.current_year] = self.investors_profile["estimation_%s"%self.current_year].apply(lambda x: {"npv": x[0], "ROI": x[1], "invest":x[2]})
        
        logging(self, "is updated.")

        self.invested_list = [i for i in self.investors_list if i.invested]
        self.n_invested = len(self.invested_list)
        self.invested_proportion[self.current_year] = self.n_invested / self.n_investor

        # proceed time    
        self.tick += self.unit_tick
        self.current_year += self.unit_tick//365

class Investor():
    def __init__(self, index, model):
        self.model = model
        self.index = index
        self.type = random.choice(["L", "F", "T"])
        
        self.type = dict(zip([0, 1, 2], ["L", "F", "T"]))
        self.type = self.type[index%3]

        self.name = "Investor%s (%s)"%(self.type, self.index)
        
        thresh_min = self.model.investor_typedata[self.type]["thresh-min"]
        thresh_max = self.model.investor_typedata[self.type]["thresh-max"]
        self.thresh = random.uniform(thresh_min, thresh_max)
        sen_min = self.model.investor_typedata[self.type]["sen-peers-min"]
        sen_max = self.model.investor_typedata[self.type]["sen-peers-max"]
        self.sen_to_peers = random.uniform(sen_min, sen_max)
        self.year_ahead = int(self.model.investor_typedata[self.type]["year-ahead"])
        
        self.wind_capacity = 0
        self.storage = 0
        
        self.operation_book = pd.DataFrame()
        
        self.capacity = 3.5 * random.randint(5, 15)
        self.demand = self.model.unit_demand / 35 * self.capacity
        
        mix_table = (self.model.investor_state["mix"]/self.model.investor_state["mix"].sum()).cumsum()
        dice = random.random()
        self.state = mix_table[mix_table > dice].index[0]
        self.windspeed_avg = self.model.investor_state.loc[self.state, "windspeed_avg"]
        windspeed_param = np.array([self.windspeed_avg-.5, self.windspeed_avg+.5, self.windspeed_avg])
        self.windspeed = random.triangular(*(windspeed_param))
        
        self.invested = False
        self.new_operate = True
        
    def invest_update(self):
        # update investor regarding to NPV calculation and investment
        self.tick = self.model.tick
        logging(self, "starts the update")
        self.calculate()
        self.decide_invest()

    def calculate(self):
        logging(self, "forecasts %s years"%self.year_ahead)
        self.cal = Calculation(model = self.model, current_year=self.model.current_year, capacity=self.capacity,
                               windspeed=self.windspeed, subsidy=self.model.subsidy,
                               time_series_data=self.model.time_series_data)
        self.cal.df["storage"] = self.storage

    def decide_invest(self):
        self.ROI = self.cal.npv / self.cal.invest
        try:
            if self.model.invested_proportion[self.model.current_year-1] > self.sen_to_peers:
                thresh = self.thresh * self.sen_to_peers
            else:
                thresh = self.thresh
        except:
            thresh = self.thresh
        self.invest = self.ROI > thresh
        self.estimation = self.cal.npv, self.ROI, self.invest

        if self.invest:
            logging(self, "decides to invest")
            self.invested = True
            self.model.investors_profile.loc[self.name, "invested"] = True
            self.model.investors_profile.loc[self.name, "invest_year"] = self.model.current_year
            self.model.investors_profile.loc[self.name, "subsidy at invest year"] = self.model.subsidy
            self.new_operate = True
            self.wind_capacity = self.capacity
        else:
            logging(self, "decides not to invest")
        
        logging(self, "estimates npv: %s, ROI: %s, invest:%s"%(self.estimation))
        
    def operate_update(self):
        # update investor regarding to yearly operation
        if self.new_operate:
            self.operation_book_one = self.operate_yearly2()

    def operate_yearly2(self):
        # operational activity within one year - buy, sell, produce 
        self.new_operate = False

        logging(self, "operates")

        df = self.model.time_series_data[self.model.time_series_data["year"] == self.model.current_year]
        price_expect = df["market price"].rolling(3, 0).mean().to_dict()
        df = df.to_dict()
        df["wind_capacity"] = self.wind_capacity * 24
        df["demand"].update({k:v*self.capacity / 35 * 24 for k, v in df["demand"].items()})
        df["net"] = {k: df["wind_capacity"] - df["demand"][k] for k in df["demand"]}
        df["surplus"] = {k: df["wind_capacity"] > df["demand"][k] for k in df["demand"]}
        df["deficit"] = {k: df["wind_capacity"] < df["demand"][k] for k in df["demand"]}
        df["store_max"] = self.storage
        df["sell profit?"] = {k: df["market price"][k] > price_expect[k] for k in df["market price"]}
        df["no profit?"] = {k: df["market price"][k] < price_expect[k] for k in df["market price"]}

        new_keys = ["store balance before", "net with store", "to be bought", "to be sold", "can be stored", "cannot be stored", "store balance after"]
        df.update({k: 0 for k in new_keys})
        df = pd.DataFrame(df).T.to_dict()

        for ind in df:
            row = df[ind]
            try:
                row["store balance before"] = df[ind-1]["store balance after"]
            except:
                pass
            row["net with store"] = row["net"] + row["store balance before"]
            row["to be sold"] = row["sell profit?"] * row["net with store"] * row["surplus"]
            row["to be bought"] = (row["net with store"] < 0) * -row["net with store"]
            row["can be stored"] = row["surplus"] * (row["no profit?"]) * min(self.storage, row["net with store"])
            row["can be stored"] += ((row["net with store"] > 0) and (row["deficit"])) * min(self.storage, row["net with store"])
            row["store balance after"] += row["can be stored"]
            row["cannot be stored"] = row["surplus"] * (row["no profit?"]) * (row["net with store"] - row["store_max"]) * (row["net with store"] > row["store_max"])
            df[ind] = row
        return df
    
class Regulator():
    def __init__(self, model):
        self.model = model
        self.name = "Regulator"
        self.type = random.choice(["green", "conservative"])
        self.subsidy = dict(zip(["green", "conservative"], [4, 1]))
        self.target_oriented = "ambitious"
        self.sen_price = 3
        self.sen_emission = 3
        
        self.set_target()
        
    def set_target(self):
        # regulator set the emission target
        data = pd.Series(dict(zip(np.linspace(2020, 2050, 4), (622.22, 466.67, 233.33, 77.78))))
        s = pd.Series(index=np.linspace(2020, 2050, 31))
        for ind, value in data.items():
            s.loc[ind] = value
        s.interpolate(inplace=True)
        self.unit_target = s
                
        yearly_demand = self.model.investors_profile["yearly_demand(MWh)"].sum()
        comparison = pd.DataFrame(self.unit_target * yearly_demand, columns=["target"])
        comparison["achieve"] = 0
        comparison["achieved?"] = False
        
        self.comparison = comparison
        
    def update(self):
        self.tick = self.model.tick
        
        # party changes every 4 years
        if self.model.current_year%4 == 0:
            self.type = random.choice(["green", "conservative"])
            logging(self, "election, new party: %s"%self.type)
        
        # evaluate the current emission
        achieve = self.model.elec_bought_log.sum().loc[self.model.current_year] * 560
        self.comparison.loc[self.model.current_year, "achieve"] = achieve        
        self.achieved = self.comparison.loc[self.model.current_year, "target"] > self.comparison.loc[self.model.current_year, "achieve"]
        self.comparison.loc[self.model.current_year, "achieved?"] = self.achieved
        
        # decide subsidy
        self.decide_subsidy()
            
    def decide_subsidy(self):
        if self.model.current_year != 2019:
            if not self.achieved:
                self.model.subsidy += self.subsidy[self.type]
                logging(self, "increases subsidy by %i (%s)"%(self.subsidy[self.type], self.type))
                self.achieved = False
            else:
                logging(self, "holds subsidy")
    
def logging(self, string):
    if isinstance(self, Model):
        self.log.append([self.tick, self.name, string])
    else:
        self.model.log.append([self.model.tick, self.name, string])