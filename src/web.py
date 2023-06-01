import base64
from urllib.parse import unquote_plus

from fastapi import FastAPI, Body, Form
from fastapi.responses import HTMLResponse
from pathlib import Path


from src.encoder import str_to_b64
from src.haskell import Diagnosis, check_haskell_project
from pydantic import BaseModel
import shutil
from jinja2 import FileSystemLoader, Environment

app = FastAPI()

file_loader = FileSystemLoader('./src/templates')
env = Environment(loader=file_loader)
project_dir = Path(__file__).parent.parent
source_dir = project_dir / "example"

built_in_files = [
    "Prelude.hs",
    "Data/List.hs",
    "Data/Maybe.hs",
    "Control/Monad/State.hs"
]

def list_files(destination_dir: Path) -> list[str]:
    if not destination_dir.exists():
        destination_dir.mkdir(exist_ok=True, parents=True)
        shutil.copytree(source_dir, destination_dir, dirs_exist_ok=True)

    files = destination_dir.rglob("*.hs")
    return sorted([str(f.relative_to(destination_dir).as_posix()) for f in files])


class ResponseBase(BaseModel):
    user_id: str
    file_list: list[str]
    built_in_files: list[str]


class Response(ResponseBase):
    diagnoses: list[Diagnosis]
    max_line_number: int
    max_col_number: int
    file: str
    text: str
    goanna1: int
    goanna2: int
    goanna3: int
    goanna4: int
    goanna5: int


@app.get("/editor/{user_id}", response_class=HTMLResponse)
async def list_file(user_id: str):
    destination_dir = project_dir / "tmp" / user_id
    file_list = list_files(destination_dir)

    response = ResponseBase(
        user_id=user_id,
        file_list=file_list,
        built_in_files=built_in_files
    )
    return env.get_template('list_files_only.html').render(
        data=response.json())


@app.post("/editor/{user_id}/{file_path:path}", response_class=HTMLResponse)
async def save_file(user_id: str, file_path: str, body: str = Body()):
    text = unquote_plus(body[5:])

    destination_dir = project_dir / "tmp" / user_id
    (destination_dir / file_path).write_text(text)
    file_list = list_files(destination_dir)
    lines = text.splitlines()
    number_of_lines = len(lines)
    number_of_cols =  max([len(line) for line in lines]) if number_of_lines else 0
    diagnoses, response_time = check_haskell_project(file_path, destination_dir)
    goanna1 = set().union(*[{r.rid for r in c.rules} for i, c in enumerate(diagnoses[0].causes) if i < 1])
    goanna2 = set().union(*[{r.rid for r in c.rules} for i, c in enumerate(diagnoses[0].causes) if i < 2])
    goanna3 = set().union(*[{r.rid for r in c.rules} for i, c in enumerate(diagnoses[0].causes) if i < 3])
    goanna4 = set().union(*[{r.rid for r in c.rules} for i, c in enumerate(diagnoses[0].causes) if i < 4])
    goanna5 = set().union(*[{r.rid for r in c.rules} for i, c in enumerate(diagnoses[0].causes) if i < 5])
    response = Response(
        text=base64.b64encode(text.encode()).decode('utf-8'),
        user_id=user_id,
        diagnoses=diagnoses,
        max_line_number=number_of_lines,
        max_col_number=number_of_cols,
        file=file_path,
        file_list=file_list,
        built_in_files=built_in_files,
        goanna1=len(goanna1),
        goanna2=len(goanna2),
        goanna3=len(goanna3),
        goanna4=len(goanna4),
        goanna5=len(goanna5)
    )
    return env.get_template('start.html').render(
        data=response.json())

@app.get("/editor/{user_id}/{file_path:path}", response_class=HTMLResponse)
async def editor(user_id: str, file_path: str):
    destination_dir = project_dir / "tmp" / user_id
    file_list = list_files(destination_dir)

    text = (destination_dir / file_path).read_text()
    lines = text.splitlines()
    number_of_lines = len(lines)
    number_of_cols =  max([len(line) for line in lines]) if number_of_lines else 0
    diagnoses, response_time = check_haskell_project(file_path, destination_dir)
    goanna1 = set().union(*[{r.rid for r in c.rules} for i, c in enumerate(diagnoses[0].causes) if i < 1])
    goanna2 = set().union(*[{r.rid for r in c.rules} for i, c in enumerate(diagnoses[0].causes) if i < 2])
    goanna3 = set().union(*[{r.rid for r in c.rules} for i, c in enumerate(diagnoses[0].causes) if i < 3])
    goanna4 = set().union(*[{r.rid for r in c.rules} for i, c in enumerate(diagnoses[0].causes) if i < 4])
    goanna5 = set().union(*[{r.rid for r in c.rules} for i, c in enumerate(diagnoses[0].causes) if i < 5])
    response = Response(
        text=str_to_b64(text),
        user_id=user_id,
        diagnoses=diagnoses,
        max_line_number=number_of_lines,
        max_col_number=number_of_cols,
        file=file_path,
        file_list=file_list,
        built_in_files=built_in_files,
        goanna1=len(goanna1),
        goanna2=len(goanna2),
        goanna3=len(goanna3),
        goanna4=len(goanna4),
        goanna5=len(goanna5)
    )
    return env.get_template('start.html').render(data=response.json())
