import os

def get_abs_path_from_relative_path(relative_path_from_this_script):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    return os.path.normpath(os.path.join(script_dir, relative_path_from_this_script))


MAIN_PROLOG_PATH = get_abs_path_from_relative_path("../../main.pl")
FIND_JADWAL_PATH = get_abs_path_from_relative_path("../../find_jadwal.pl")
TIME_PROCESSING_UTILITIES_PL_PATH = get_abs_path_from_relative_path("../../time_processing_utilities.pl")