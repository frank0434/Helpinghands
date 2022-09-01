# -*- coding: utf-8 -*-

# Run this app with `python app.py` and
# visit http://127.0.0.1:8050/ in your web browser.

import dash
import dash_core_components as dcc
import dash_html_components as html
import plotly.graph_objects as go
from dash.dependencies import Input, Output
from dash.exceptions import PreventUpdate
from plotly.subplots import make_subplots
import plotly.express as px
import pandas as pd
import argparse
import pandas as pd
from influxdb import DataFrameClient
from influxdb import InfluxDBClient
import configparser

# db connection requirements 
config = configparser.ConfigParser()
config.read('credentials.ini')
host = config['influxdb']['host']
port = config['influxdb']['port']
user = config['influxdb']['user']
password = config['influxdb']['password']
dbname = config['influxdb']['dbname']

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

# new df from influxdb
myclient = DataFrameClient(host, port, user, password, dbname)

# defaultDBinfo = {"host":"text", "port":"number","user":"text","password":"password","dbname":"text"}


rs = myclient.query('select * FROM "Ablock"."autogen"."file"')
df = rs['file']

available_options = df.columns.tolist()

# Graphing -base figure if there is a display need 
# fig = px.line(df, y = "RainfallmmTot")
# fig.update_layout(
#     plot_bgcolor=colors['background'],
#     paper_bgcolor=colors['background'],
#     font_color=colors['text']
# )
colors = {
    'background': '#ffffff'
    # 'text': '#7FDBFF'
}

# Reporting 

markdownText = '''
**Placeholder for auto reporting**
'''

# App layout
app.layout = html.Div([
    # Title area
    html.H1(children='IoT for Decision Making - Demo'),
    html.Div(children='''
        An interactive dashboard to view sensor data and download report.
    '''),
    # html.Br()
    # html.Div([
    #     dcc.Input(
    #         id="input_{}".format(variable),
    #         type = _,
    #         placeholder="input {}".format(variable),
    #         debounce = True
    #     )
    #     for variable, _ in defaultDBinfo.items()
    # ]
    ## Show the user input or not 
    # + [html.Div(id="intermediate-value")]),
    html.Br(),
    dcc.Tabs([
        dcc.Tab(label = 'Raw Data Overview', children = [
            # User selection input area
            html.Br(),
            html.Div([
                dcc.Dropdown(
                id = 'Colname',
                options = [{'label': i, 'value': i} for i in available_options],
                value = 'RainfallmmTot', 
                placeholder="Select a column name")
                ],
                style = {'Height' : '50%','width' : '75%',                         
                         'marginLeft': 200, 'marginRight': 25
                        }
            ),
            html.Br(),
            html.P(id = 'col-output',style = {'Height' : '50%','width' : '75%',                          
                         'marginLeft': 200, 'marginRight': 25
                        }),
            html.Br(),
            # Graphing area 
            html.Div([
                dcc.Graph(id='graph-monthly')
                # dcc.Graph(id='graph-daily')
                ],
                style = {'Height' : '50%','width' : '75%', 
                         'marginBottom': 50,
                         'marginLeft': 200, 'marginRight': 25
                        }),
            html.Div('Download graph:'),
            html.Button("as jpg", id="btn-get-jpg"),
            html.Button("as png", id="btn-get-png"),
            html.Button("as svg", id="btn-get-svg")
        ]),
        dcc.Tab(label = 'Distribution of each variable', children = [
            html.Div(children = 'place holder for distribution graphs')
        ]),
        dcc.Tab(label = 'Accumulation'),
        dcc.Tab(label = 'Water Balance'),
        dcc.Tab(label = 'Report', children = [
            # Markdown for reporting
            html.Div([
                dcc.Markdown(children = markdownText)],
                style = {'display': 'inline-block'}),
            html.Div([
                html.A(html.Button('Download Report', id='download-button'), id='download-link-birst-category')
          ])
            ])
    ])
     # Hidden div inside the app that stores the intermediate value
    # html.Div(id='intermediate-value', style={'display': 'none'})
])

## column name 
@app.callback([
    Output('graph-monthly', 'figure'),    
    Output(component_id='col-output', component_property='children')
    ],
    [Input('Colname', 'value')])
def update_figure(selected_col):
    if selected_col is None:
        raise PreventUpdate

    # Make a subplot of 2 by 1
    fig = make_subplots(
        rows=2, cols=1,
        shared_xaxes=True,
        vertical_spacing=0.03,
        specs=[[{"type": "histogram"}],
               [{"type": "scatter"}]]
    )
    # Add a monthly rainfall
    fig.add_trace(
        go.Histogram(x = df.index, y=df[selected_col], histfunc="sum", name ="Monthly Rainfall (mm)"),
        row = 1, col = 1 
    )
    fig.update_traces(xbins_size="M1")
    fig.update_xaxes(showgrid=True, ticklabelmode="period", dtick="M1", tickformat="%b\n%Y")
    fig.update_layout(bargap=0.1)
    # Add a daily rainfall
    fig.add_trace(
        go.Scatter(mode="markers", x=df.index, y=df[selected_col], name="Daily Rainfall (mm)"),
        row = 2, col = 1
    )
    # Adjust the margin 
    fig.update_layout(margin = dict(t = 10, r = 10, b = 10, l = 10))

    # fig.update_layout(transition_duration=500)
    # fig.update_layout(
        # plot_bgcolor=colors['background'],
        # paper_bgcolor=colors['background']
    # font_color=colors['text']
    # )
    # fig.update_xaxes(gridcolor = "black")

    return fig, 'You have selected Column: {}'.format(selected_col)

# callbacks
## db information callbacks
# @app.callback(
#     Output('intermediate-value', 'children'), 
#     # Output("out-all-types", "children"),
#     [Input("input_{}".format(variable), "value") for variable,_ in defaultDBinfo.items()],
#     prevent_initial_call=True
# )
# def cb_render(*vals):
#     if len(vals) == 5: 
#         host, port, user, password, dbname = (str(val) for val in vals if val)
#         return host, port, user, password, dbname
#     else:
#         return print("Please provide influxdb information.")
    # " | ".join((str(val) for val in vals if val))

@app.callback([  
    Output("cytoscape-image-export", "generateImage")
    ],
    [
        Input('graph-monthly', component_property= 'value'),
        Input("btn-get_jpg", "n_clicks"),
        Input("btn-get_png", "n_clicks"),
        Input("btn-get_svg", "n_clicks")   
    ])
def get_image(tab, get_jpg_clicks, get_png_clicks, get_svg_clicks):

    # File type to output of 'svg, 'png', 'jpg', or 'jpeg' (alias of 'jpg')
    ftype = tab

    # 'store': Stores the image data in 'imageData' !only jpg/png are supported
    # 'download'`: Downloads the image as a file with all data handling
    # 'both'`: Stores image data and downloads image as file.
    action = 'store'

    ctx = dash.callback_context
    if ctx.triggered:
        input_id = ctx.triggered[0]["prop_id"].split(".")[0]

        if input_id != "tabs":
            action = "download"
            ftype = input_id.split("-")[-1]

    return {
        'type': ftype,
        'action': action
        }
if __name__ == '__main__':
    app.run_server(host = '0.0.0.0', port = '3003', debug=True)
