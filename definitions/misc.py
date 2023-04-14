from HzzProlog.HzzProlog import HzzProlog


def define_tokenizer_regex(hzz_prolog: HzzProlog):  # special cases like time_point
    hzz_prolog.add_new_regex(150, "\d+:\d+:\d+")
    return hzz_prolog
