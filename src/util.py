#  A function in python named merge_dict
# merge_dict will merge two dicts with same key by combining simple values into list
# e.g. merge_dict({'a': 3}, {'a': 4}) will return {'a': [3,4]}
# merge_dict will combine values even when they are the same
# e.g.merge_dict({'a': 3}, {'a': 3}) will return {'a': [3]}
# merge_dict will append if one of the corresponding value is a list
# e.g.merge_dict({'a': [3]}, {'a': 4}) will return {'a': [3,4]}
# merge_dict will use the value as-is if the key is missing in the other dict
# e.g.merge_dict({'a': 3}, {'b': 4}) will return {'a': 3, 'b': 4}
# merge_dict will merge two dicts with same key by concatenating if the corresponding value is already a list
# e.g.merge_dict({'a': [3]}, {'a': [4, 5]}) will return {'a': [3,4, 5]}
# merge_dict will merge recursively go through the dictionary
# e.g.merge_dict({'a': {'b': 3}}, {'a': {'b': 4}}) will return {'a': {'b': [3,4]}}
def merge_dict(dict1, dict2):
    new_dict = dict1.copy()
    for key, value1 in dict1.items():
        if key in dict2:
            value2 = dict2[key]
            if isinstance(value1, dict) and isinstance(value2, dict):
                new_dict[key] = merge_dict(value1, value2)
            elif isinstance(value1, list) and isinstance(value2, list):
                new_dict[key] = value1 + value2
            elif isinstance(value1, list):
                new_dict[key] = value1 + [value2]
            elif isinstance(value2, list):
                new_dict[key] = [value1] + value2
            else:
                new_dict[key] = [value1, value2]
        else:
            new_dict[key] = value1
    return new_dict

if __name__ == "__main__":
    result = merge_dict({'a': {'b': 3}}, {'a': {'b': 4}})
    print(result)