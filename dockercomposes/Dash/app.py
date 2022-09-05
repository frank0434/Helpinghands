from sqlalchemy import create_engine
import pandas as pd
from sqlalchemy.pool import NullPool
import os
from dotenv import load_dotenv

# functions

def get_data():
    query = 'SELECT * FROM "RawData";'
    # load credentials from env
    load_dotenv(override=True)
    try:
        auth = "postgresql://" + os.environ.get('Username') + ':' + os.environ.get('Password') + '@' + os.environ.get('Server') + '/' + os.environ.get('dbname') 
        engine = create_engine(auth, poolclass=NullPool)
        df = pd.read_sql_query(query, engine)
    finally:
        engine.dispose()
    return df

import dash
from dash import dash_table
from dash import html
from dash import dcc
import plotly.express as px

# the app 
app = dash.Dash()   #initialising dash app

def rawGraph():
    df = get_data()
    # SWD.set_index("date", inplace=True)
    # Function for creating line chart showing Google stock prices over time 
    col_tovis = df.columns[7:].to_list()
    fig = px.scatter_matrix(df,
                            dimensions=col_tovis,
                            color="Fruitheight",  symbol="Treetype", height=2000, width = 2000)
    fig.update_layout(title = 'Pair-wise scatter plot',                     
                      paper_bgcolor="LightSteelBlue")
    return html.Div(id = 'parent', children = [
           html.H1(id = 'H1', children = 'GF_MAHS_RA5', 
           style = {'textAlign':'center','marginTop':40,'marginBottom':40}),        
           dcc.Graph(id = 'line_plot', figure = fig)],
           style = {'Height' : '100%','width' : '100%'})
 
app.layout = rawGraph()


if __name__ == '__main__': 
    app.run_server(host = '0.0.0.0', port = '3007', debug=True)
