import pandas as pd
import datetime                              # access to %%time, for timing individual notebook cells
import os
import tempfile
import re                                     # regex 
from requests_ntlm import HttpNtlmAuth
import requests
from dotenv import load_dotenv
from sqlalchemy import create_engine

url = "https://iplant.plantandfood.co.nz/project2/I210820/RA5%20%20Research/Fruit%20growth%20on%20FOPS%20and%20TS%202021.xlsx"
# Source env variables
load_dotenv(override=True)
user = os.environ.get('secretUser')
password = os.environ.get('secretKey')
r=requests.get(url, auth=HttpNtlmAuth(user, password))
output = open("/workspace/cflfcl/github/GF_DHS_MHAS_RA5/Data/fruitgrowth.xlsx", 'wb')
output.write(r.content)
output.close()
df = pd.read_excel("/workspace/cflfcl/github/GF_DHS_MHAS_RA5/Data/fruitgrowth.xlsx", 
                    sheet_name="RawData",
                    index_col='Date',
                    parse_dates=True, #tell the function to parse date columns to datetime formats
                    na_values='NAN')

drop_cols = df.filter(regex = "named").columns.values.tolist()
df.drop(columns=drop_cols, inplace=True)
df.drop_duplicates(inplace=True)

# write into postgresql db
auth = "postgresql://" + os.environ.get('Username') + ':' + os.environ.get('Password') + '@' + os.environ.get('Server') + '/' + os.environ.get('dbname') 
engine = create_engine(auth)

df.to_sql('RawData', engine, if_exists='replace')