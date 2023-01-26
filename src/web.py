from fastapi import FastAPI
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles

from src.haskell import System

app = FastAPI()
system = System()


@app.get("/api/data")
def data():
    return {
        'parsing_result': system.parsed_data,
        'file_path': system.current_file_path,
        'file_content': system.current_file
    }


@app.get('/api/file/{file_path:path}')
def open_file(file_path: str):
    system.open_file(file_path)
    return {
        'parsing_result': system.parsed_data,
        'file_path': system.current_file_path,
        'file_content': system.current_file
    }


app.mount("/static", StaticFiles(directory="./client/output"), name="static")
@app.get("/", response_class=HTMLResponse)
def home():
    return """
    <!DOCTYPE html>
    <html>
        <head>
            <title>Editor</title>
            <link rel="icon" href="https://fav.farm/💩" /> 
            <script src="https://cdn.tailwindcss.com"></script>
            <link rel="stylesheet" href="/static/css/style.css">
        </head>
        <body>
            <div id="react-root"  class="h-full"></div>
            <script src="/static/js/main.js"></script>
        </body>
    </html>
    """

