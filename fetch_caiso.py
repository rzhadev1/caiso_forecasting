import io
import zipfile
import requests 
import pandas as pd
import time
import pytz
from datetime import datetime, timedelta 


base_url = "http://oasis.caiso.com/oasisapi/SingleZip?"
# seems like CAISO only archives around 3 years worth of wind/solar forecasts
start_date = datetime(2019, 9, 1)
end_date = datetime(2022, 11, 1)

# convert datetime object to UTC  
def get_UTC_string(dt, local_tz="America/Los_Angeles", fmt="%Y%m%dT%H:%M-0000"):
	tz_ = pytz.timezone(local_tz)
	return tz_.localize(dt).astimezone(pytz.UTC).strftime(fmt)

# convert returned zip file from api to dataframe
def resp_to_df(response):
	with io.BytesIO() as buffer:
		try:
			buffer.write(response.content)
			buffer.seek(0)
			z = zipfile.ZipFile(buffer)
		except zipfile.BadZipFile as e: 
			print("bad zip", e)
		else: 
			csv = z.open(z.namelist()[0])
			df = pd.read_csv(csv)
			return df
		
def format_lmp_df(df, mapping):
	def in_thub(pnode):
		if pnode in mapping.keys():
			return mapping[pnode]
		return None
	# we query one day at a time so no need for the interval 
	df.drop('OPR_INTERVAL', axis=1, inplace=True)
	df.drop('POS', axis=1, inplace=True)
	df.drop('GROUP', axis=1, inplace=True)
	# some pnodes are not in any of the trading hubs, so 
	# filter these out with the groupby
	# (trading hubs appear to just be a specific type of aggregate pricing node)
	df['THUB'] = df.apply(lambda row: in_thub(row.NODE), axis=1)
	df = df.groupby(by=['OPR_DT', 'THUB', 'OPR_HR']).mean().rename({'MW':'LMP'}, axis=1).reset_index()
	return df

def load_pnode_to_thub(date):
	# query CAISO atlas database for   
	# the existing pnode mappings to trading hub on a given date
	params = {
		"queryname": "ATL_PNODE_MAP", 
		"startdatetime": get_UTC_string(date), 
		"enddatetime": get_UTC_string(date+timedelta(1)), 
		"version": 1,
		"resultformat": 6
	}
	response = requests.get(base_url, params=params, timeout=15)
	response.raise_for_status() 
	df = resp_to_df(response)
	pairs = dict(zip(df['PNODE_ID'], df['APNODE_ID']))
	return pairs

# get day ahead market locational marginal pricing between start and end date
# gives lmp of all pnodes in the dam, which we aggregate into 
# trading hub prices by using the mapping given by oasis. 
# Note that 
# http://www.caiso.com/Pages/glossary.aspx?Paged=TRUE&p_SortBehavior=0&p_Term=Existing%20Zone&p_ID=480&PageFirstRow=401&&View=%7B8034109D-E87A-4203-90DC-41FF59CA116E%7D
# http://www.caiso.com/Documents/Conformed-Tariff-as-of-Feb23-2023.pdf
# both indicate that the three trading hubs are classified as Existing Zone Generation Trading Hubs, 
# meaning that their LMPs are a simple average of the pnode that comprise the trading hub.
def get_lmps(path='data/dam_lmps1.csv'):
	# map pricing node to trading hub, then average LMPs by trading hub
	date_list = pd.date_range(start=start_date, end=end_date, freq='D')
	#ret = pd.read_csv(path, index_col=0)
	ret = pd.DataFrame()
	for date in date_list: 
		mapping = load_pnode_to_thub(date)
		params = {
			"queryname": "PRC_LMP", 
			"market_run_id": "DAM",
			"startdatetime": get_UTC_string(date),
			"enddatetime": get_UTC_string(date+timedelta(1)), 
			"version": 1,
			"resultformat": 6, 
			"grp_type": "ALL",
		}
		response = requests.get(base_url, params=params, timeout=None)
		response.raise_for_status()
		df = resp_to_df(response)
		print(df)
		df = format_lmp_df(df, mapping)
		ret = pd.concat([ret, df], ignore_index=True)
		ret.to_csv(path)
		print(f"completed day {date} saving to {path}")
		time.sleep(10)
 
	
	return ret

# use load_pnode_to_thub_map and get all mappings between pnode -> thub 
# by trading hub, get all pnodes and map those to TAC areas, which 
# are used to disaggregate import and export data as well as load data 
# the following mappings are given: 
# SP15 -> TAC_ECNTR, TAC_NCNTR, TAC_SOUTH
# ZP26 -> TAC_NORTH
# NP15 -> TAC_NORTH
def get_tac_thub_map(path='data/tac_thub_map.csv'):

	def reverse_mapping(map1):
		thubs = set(map1.values())
		thub_pnd_map = {}

		for thub in thubs: 
			thub_pnd_map[thub] = []

		for pnode,thub in map1.items():
			thub_pnd_map[thub].append(pnode)

		return thub_pnd_map
	date_list = pd.date_range(start=start_date, end=end_date, freq='D')
	ret = pd.DataFrame()
	for date in date_list: 
		pnd_thub_map = load_pnode_to_thub(date)
		# thub -> list of pnodes in that trading hub
		thub_pnd_map = reverse_mapping(pnd_thub_map)
		
		# get the TAC area mapping for the current date
		# map pnode -> TAC area and aggregate by trading hub 
		# this gives a mapping of thub -> TAC area		
		params = {
			"queryname": "ATL_TAC_AREA_MAP", 
			"startdatetime": get_UTC_string(date),
			"enddatetime": get_UTC_string(date+timedelta(1)), 
			"version": 1,
			"resultformat": 6, 
		}
		response = requests.get(base_url, params=params, timeout=None)
		response.raise_for_status() 
		df = resp_to_df(response)
		pnode_tac_area_map = dict(zip(df['PNODE_ID'], df['TAC_AREA_ID']))
		thub_tac_map = {}
		for thub,pnds in thub_pnd_map.items():
			tac_areas = [] 
			for pnode in pnds: 
				if pnode in pnode_tac_area_map.keys():
					tac_areas.append(pnode_tac_area_map[pnode])
			thub_tac_map[thub] = str(set(tac_areas))

		df = pd.DataFrame(thub_tac_map, index=[date])
		ret = pd.concat([ret, df], ignore_index=False)
		print(ret)
		ret.to_csv(path)
		print(f"completed day {date} saving to {path}")
		time.sleep(15)

	
	return ret



def get_load_fcst(path='data/load_fcst.csv'):
	date_list = pd.date_range(start=start_date, end=end_date, freq='D')
	#ret = pd.read_csv(path, index_col=0)
	ret = pd.DataFrame()
	for date in date_list: 
		params = {
			"queryname": "SLD_FCST", 
			"market_run_id": "DAM",
			"startdatetime": get_UTC_string(date),
			"enddatetime": get_UTC_string(date+timedelta(1)), 
			"version": 1,
			"resultformat": 6, 
		}
		response = requests.get(base_url, params=params, timeout=None)
		response.raise_for_status() 
		df = resp_to_df(response).sort_values(by=['OPR_HR'])
		ret = pd.concat([ret, df], ignore_index=True)
		ret.to_csv(path)
		print(f"completed day {date} saving to {path}")
		time.sleep(15)
	
	return ret

def get_ren_fcst(path='data/ren_fcst.csv'):
	date_list = pd.date_range(start=start_date, end=end_date, freq='D')
	#ret = pd.read_csv(path, index_col=0)
	ret = pd.DataFrame()
	for date in date_list: 
		params = {
			"queryname": "SLD_REN_FCST", 
			"market_run_id": "DAM",
			"startdatetime": get_UTC_string(date),
			"enddatetime": get_UTC_string(date+timedelta(1)), 
			"version": 1,
			"resultformat": 6, 
		}
		response = requests.get(base_url, params=params, timeout=None)
		response.raise_for_status() 
		df = resp_to_df(response).sort_values(by=['OPR_HR'])
		ret = pd.concat([ret, df], ignore_index=True)
		ret.to_csv(path)
		print(f"completed day {date} saving to {path}")
		time.sleep(15)
	
	return ret

# get gas flow price by fuel region, then average 
# by all regions
def get_gas_flow_price(path='data/gas_flow.csv'):
	date_list = pd.date_range(start=start_date, end=end_date, freq='D')
	ret = pd.read_csv(path, index_col=0)
	#ret = pd.DataFrame()
	for date in date_list: 
		params = {
			"queryname": "PRC_FUEL", 
			"startdatetime": get_UTC_string(date),
			"enddatetime": get_UTC_string(date+timedelta(1)), 
			"version": 1,
			"resultformat": 6, 
		}
		response = requests.get(base_url, params=params, timeout=None)
		response.raise_for_status() 
		df = resp_to_df(response).sort_values(by=['OPR_HR'])
		df.drop('GROUP', axis=1, inplace=True)
		df = df.groupby(by=['OPR_DT', 'OPR_HR']).mean().rename({'PRC':'GAS_AVG_PRC'}, axis=1).reset_index()
		ret = pd.concat([ret, df], ignore_index=True)
		ret.to_csv(path)
		print(f"completed day {date} saving to {path}")
		time.sleep(15)
	
	return ret

# get greenhouse gas allowance price 
def get_ghg(path='data/ghg.csv'):
	date_list = pd.date_range(start=start_date, end=end_date, freq='D')
	ret = pd.read_csv(path, index_col=0)
	#ret = pd.DataFrame()
	for date in date_list: 
		params = {
			"queryname": "PRC_GHG_ALLOWANCE", 
			"startdatetime": get_UTC_string(date),
			"enddatetime": get_UTC_string(date+timedelta(1)), 
			"version": 1,
			"resultformat": 6, 
		}
		response = requests.get(base_url, params=params, timeout=None)
		response.raise_for_status() 
		df = resp_to_df(response)
		ret = pd.concat([ret, df], ignore_index=True)
		ret.to_csv(path)
		print(f"completed day {date} saving to {path}")
		time.sleep(15)
	
	return ret



def get_imp_exp(path='data/import_export_data.csv'):
	date_list = pd.date_range(start=start_date, end=end_date, freq='D')
	#ret = pd.read_csv(path, index_col=0)
	ret = pd.DataFrame()
	for date in date_list: 
		params = {
			"queryname": "ENE_SLRS", 
			"market_run_id": "DAM",
			"startdatetime": get_UTC_string(date),
			"enddatetime": get_UTC_string(date+timedelta(1)), 
			"version": 1,
			"resultformat": 6, 
		}
		response = requests.get(base_url, params=params, timeout=None)
		response.raise_for_status() 
		df = resp_to_df(response).sort_values(by=['OPR_HR'])
		ret = pd.concat([ret, df], ignore_index=True)
		ret.to_csv(path)
		print(f"completed day {date} saving to {path}")
		time.sleep(15)
	
	return ret
lmps = get_lmps()
print(lmps)
#load_fcst = get_load_fcst()
#ren_fcst = get_ren_fcst()
#gas = get_gas_flow_price()
#ghg = get_ghg()
#get_imp_exp()
#get_tac_thub_map()
