import os
import subprocess, collections
from multiprocessing.pool import ThreadPool
from random import randint
from typing import IO

import os, sys; sys.path.append(os.path.dirname(os.path.realpath(__file__)))
from PrologOutputProcessor import PrologOutputProcessor


class PrologException(Exception):
    def __init__(self, *args):
        super().__init__(*args)


class HzzProlog:
    def __init__(self, script_file_io: IO, delete_temp_files=True):
        self.temp_folders = "temp/"
        self.script_file_io = script_file_io
        self.facts = {}
        self.custom_regex_tokenizer: list[tuple[int, str]] = []
        self.last_stdout = None
        self.created_temp_files = []
        self.delete_temp_files = delete_temp_files

    def add_new_regex(self, priority_rank, regex):
        self.custom_regex_tokenizer.append((priority_rank, regex))
        self.custom_regex_tokenizer.sort(key=lambda x: x[0])

    def add_facts(self, template_variable_name: str, fact_definitions: list[str]):
        for fact_definition in fact_definitions:
            self.add_fact(template_variable_name=template_variable_name, fact_definition=fact_definition)

    def add_fact(self, template_variable_name: str, fact_definition: str):
        if template_variable_name not in self.facts:
            self.facts[template_variable_name] = []
        if not fact_definition.endswith("."):
            fact_definition += "."
        self.facts[template_variable_name].append(fact_definition)

    def get_injected_script(self):
        print(os.getcwd())
        create_dir_if_not_exist(self.temp_folders)
        file_name = self.get_temporary_file_name()
        self.script_file_io.seek(0)

        file_content = self.script_file_io.read()
        file_content = self.remove_ignored(file_content)
        file_content = self.inject_facts(file_content)

        with open(file_name, "w") as f:
            f.write(file_content)
        return file_name

    def get_temporary_file_name(self):
        file_name = [chr(randint(65, 65 + 26 - 1)) for _ in range(10)]
        file_name = "".join(file_name) + ".pl"
        ret = os.path.join(self.temp_folders, file_name)
        self.created_temp_files.append(ret)
        return ret

    def __del__(self):
        if not self.delete_temp_files:
            return 
        for file in self.created_temp_files:
            os.remove(file)

    def inject_facts(self, template_prolog_content: str) -> str:
        for key, facts in self.facts.items():
            facts = "\n" + "\n".join(facts) + "\n"
            template_prolog_content = template_prolog_content.replace("{{"+key+"}}", facts)
        return template_prolog_content

    def remove_ignored(self, template_prolog_content: str):
        lines = template_prolog_content.split("\n")
        ret = []
        lines_iterator = iter(lines)
        for line in lines_iterator:
            if "{%ignored%}" in line:
                continue
            if "{%begin ignore%}" in line:
                while True:
                    curr_line = next(lines_iterator)
                    if "{%end ignore%}" in curr_line:
                        break
                continue
            ret.append(line)
        return "\n".join(ret)

    def reset(self):
        args = ["swipl", '--quiet']
        if self.script_file_io is not None:
            script_file_name = self.get_injected_script()
            args.append(script_file_name)

        self.p = subprocess.Popen(
            args,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        self.start_token = "START_TOKEN"

    def prefix(self):
        return "\n".join([
            "set_prolog_flag(answer_write_options,[max_depth(0)]).",
        ] + [self.start_of_output_marker()])

    def start_of_output_marker(self) -> str:
        return f"write('{self.start_token}').\n"

    def remove_start_of_output_mark(self, prolog_output_processor: PrologOutputProcessor):
        prolog_output_processor.pop_until(self.start_token)
        assert next(prolog_output_processor.iter_tokens) == "true"
        assert next(prolog_output_processor.iter_tokens) == "."

    def suffix(self, approx_number_of_output):
        return ", write('BACKTRACK').\n" + ";\n" * (approx_number_of_output - 1)

    def query_raw(self, query, approx_number_of_output=1):
        self.reset()
        assert not query.endswith("."), "query should NOT be ended with punctuation"

        query = self.prefix() + query + self.suffix(approx_number_of_output)
        stdout, stderr = self.p.communicate(input=query)
        return stdout, stderr

    def query(self, query, approx_number_of_output=100):
        stdout, stderr = self.query_raw(query, approx_number_of_output=approx_number_of_output)
        self.last_stdout = stdout

        if stderr:
            if self._error_is_because_too_few_semi_colon(stderr):
                stdout += "BACKTRACK false."
            elif not self._error_is_because_too_much_semicolon(stderr):
                raise PrologException(stderr)

        output_processor = PrologOutputProcessor(stdout, additional_regex=self.custom_regex_tokenizer)
        self.remove_start_of_output_mark(output_processor)
        ret = output_processor.process_token()
        return ret

    def _error_is_because_too_few_semi_colon(self, err_msg):
        if err_msg is None:
            return False
        return "Type error: `character_code' expected, found `-1' (an integer)" in err_msg

    def _error_is_because_too_much_semicolon(self, err_msg):
        if err_msg is None:
            return False
        cond1 = "Stream user_input" in err_msg
        cond2 = "Syntax error: Unexpected end of file" in err_msg
        return cond1 and cond2


def for_multithread(func, args: list[tuple], pool_num=8, timeout=99999):
    ret = []
    with ThreadPool(pool_num) as pool:
        ongoing_results = []
        for grouped_args in group_iter(args, pool_num*5):
            for arg in grouped_args:
                ongoing_results.append(
                    pool.apply_async(func, arg)
                )
        for result in ongoing_results:
            ret.append(result.get(timeout=timeout))
    return ret

def group_iter(iterable, group_size):
    assert group_size >= 1
    if not isinstance(iterable, collections.abc.Iterator):
        iterable = iter(iterable)
    stop = False
    while not stop:
        ret = []
        try:
            for _i in range(group_size):
                ret.append(next(iterable))
        except StopIteration:
            stop = True
        if len(ret) >= 1:
            yield ret

def create_dir_if_not_exist(path):
    does_exist = os.path.exists(path)
    if not does_exist:
        os.makedirs(path)