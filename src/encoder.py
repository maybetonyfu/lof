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
    "?": 'SYMqm'
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
    'SYMqm': '?'

}


def encode_char(char: str) -> str:
    if char == '_':
        return char
    elif char.isalnum():
        return char
    elif char in "+-*!#$.='%|~:&/\<>@?":
        return symbol_to_name[char]
    else:
        raise ValueError(f"Symbol {char} is not allowed")


def encode(text: str) -> str:
    return ''.join([encode_char(c) for c in text])


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

if __name__ == '__main__':
    print(
        decode(encode('uuvsj<*?>xyz')) == 'uuvsj<*?>xyz'
    )
