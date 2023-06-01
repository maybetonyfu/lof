import re
from pathlib import Path
import csv

project_dir = Path(__file__).parent.parent
example_dir = project_dir / 'dataset'
data_dir = project_dir / 'data'
hs_modules = [f for f in example_dir.rglob('*.hs')]



def number_of_files():
    print("There are", len(hs_modules), "files in total")

def get_themes():
    all_themes = set()
    for f in hs_modules:
        rel_name = f.relative_to(example_dir).as_posix()
        # if rel_name not in builtin:
        print(rel_name)
        text = f.read_text()
        m = re.search(r'--\s*theme:\s*.*', text)
        themes = m.group(0).split(':')[1].strip().split(',')
        # print(themes)
        for theme in themes:
            all_themes.add(theme.strip())
    print(all_themes)

# -- theme: adt
# -- goanna results: 4
# -- oracle: true
# -- intended fix: 4
# -- response time: 0.49406981468200684
# -- mus size: 5
# -- ghc loc: 0
# -- ghc diagnosis: 0
# -- ghc fix: 0


def get_data():
    with open(data_dir / 'data.csv', 'w', newline='') as f:
        # Create the csv writer
        writer = csv.writer(f)

        # Write a row to the csv file
        writer.writerow([
            "filename", "goanna_result", "intended_fix", "chameleon", "ghc_loc", "ghc_diagnosis", "ghc_fix",
            "goanna_acc_1",
            "goanna_acc_2",
            "goanna_acc_3",
            "goanna_acc_4",
            "goanna_acc_5",
            "goanna_locs_1",
            "goanna_locs_2",
            "goanna_locs_3",
            "goanna_locs_4",
            "goanna_locs_5",
            "response_time"

        ])

        for f in hs_modules:
            rel_name = f.relative_to(example_dir).as_posix()
            text = f.read_text()
            goanna_results = int(re.search(r'--\s*goanna results:\s*.*', text).group(0).split(':')[1].strip())
            intended_fix = int(re.search(r'--\s*intended fix:\s*.*', text).group(0).split(':')[1].strip())
            response_time = float(re.search(r'--\s*response time:\s*.*', text).group(0).split(':')[1].strip())
            chameleon_results = int(re.search(r'--\s*mus size:\s*.*', text).group(0).split(':')[1].strip())
            ghc_loc = int(re.search(r'--\s*ghc loc:\s*.*', text).group(0).split(':')[1].strip()) == 1
            ghc_diagnosis = int(re.search(r'--\s*ghc diagnosis:\s*.*', text).group(0).split(':')[1].strip()) == 1
            goanna_acc1 = intended_fix <= 1
            goanna_acc2 = intended_fix <= 2
            goanna_acc3 = intended_fix <= 3
            goanna_acc4 = intended_fix <= 4
            goanna_acc5 = intended_fix <= 5
            goanna1 = int(re.search(r'--\s*goanna1:\s*.*', text).group(0).split(':')[1].strip())
            goanna2 = int(re.search(r'--\s*goanna2:\s*.*', text).group(0).split(':')[1].strip())
            goanna3 = int(re.search(r'--\s*goanna3:\s*.*', text).group(0).split(':')[1].strip())
            goanna4 = int(re.search(r'--\s*goanna4:\s*.*', text).group(0).split(':')[1].strip())
            goanna5 = int(re.search(r'--\s*goanna5:\s*.*', text).group(0).split(':')[1].strip())
            data = [rel_name,
                    goanna_results,
                    intended_fix,
                    chameleon_results,
                    ghc_loc,
                    ghc_diagnosis,
                    False,
                    goanna_acc1,
                    goanna_acc2,
                    goanna_acc3,
                    goanna_acc4,
                    goanna_acc5,
                    goanna1,
                    goanna2,
                    goanna3,
                    goanna4,
                    goanna5,
                    response_time]
            writer.writerow(data)



if __name__ == "__main__":
    # number_of_files()
    get_data()