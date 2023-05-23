import base64

symbol_to_name = {
    '+': 'SYMpl',
    '-': 'SYMmn',
    '*': 'SYMas',
    '!': 'SYMbg',
    '#': 'SYMpd',
    '$': 'SYMdl',
    '.': 'SYMdt',
    '=': 'SYMeq',
    "'": 'SYMpr',
    "%": 'SYMpc',
    "|": 'SYMpp',
    "~": 'SYMtl',
    ":": 'SYMcn',
    "&": 'SYMam',
    "/": 'SYMsl',
    "\\": 'SYMbs',
    "<": 'SYMlt',
    ">": 'SYMgt',
    "@": 'SYMat',
    "?": 'SYMqm',
    '^': 'SYMht'
}

name_to_symbol = {
    "SYMpl": "+",
    "SYMmn": "-",
    "SYMas": "*",
    "SYMbg": "!",
    "SYMpd": "#",
    "SYMdl": "$",
    "SYMdt": ".",
    "SYMeq": "=",
    "SYMpr": "'",
    "SYMpc": "%",
    "SYMpp": "|",
    "SYMtl": "~",
    "SYMcn": ":",
    "SYMam": "&",
    "SYMsl": "/",
    "SYMbs": "\\",
    "SYMlt": "<",
    "SYMgt": ">",
    "SYMat": "@",
    'SYMqm': '?',
    'SYMht': '^'

}


def encode_char(char: str) -> str:
    if char == '_':
        return char
    elif char.isalnum():
        return char
    elif char in "+-*!#$.='%|~:&/\<>@?^":
        return symbol_to_name[char]
    else:
        raise ValueError(f"Symbol {char} is not allowed")


def encode(text: str) -> str:
    return ''.join([encode_char(c) for c in text])

def de_module(name:  str) -> str:
    if name.startswith('_hsmd_'):
        return name.split('_hsmd_')[-1]
    else:
        return name
def de_location(name: str) -> str:
    parts = name.split('_')
    if len(parts) < 3:
        return name
    else:
        return '_'.join(name.split('_')[:-2])

def decode(text: str) -> str:
    input = text
    output = ''
    while len(input) != 0:
        if len(input) >= 5 and input.startswith('SYM'):
            output = output + name_to_symbol[input[:5]]
            input = input[5:]
        else:
            output = output + input[0]
            input = input[1:]
    return output


def str_to_b64 (input : str) -> str:
    return base64.b64encode(input.encode()).decode('utf-8')

def b64_to_str (input: str) -> str:
    return base64.b64decode(input.encode()).decode('utf-8')

if __name__ == '__main__':
    print(
        decode(encode('uuvsj<*?>xyz')) == 'uuvsj<*?>xyz',
    )
