def get_list_from_list_of_dicts(list_of_dicts, key):
    key = str(key)

    ret = []
    for dct in list_of_dicts:
        assert isinstance(dct, dict)
        ret.append(dct[key])
    return ret
