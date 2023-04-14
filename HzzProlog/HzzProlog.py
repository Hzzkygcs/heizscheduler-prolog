from __future__ import annotations

import os
import re
import subprocess, collections
from multiprocessing.pool import ThreadPool
from random import randint
from typing import IO

import os, sys; sys.path.append(os.path.dirname(os.path.realpath(__file__)))
from PrologOutputProcessor import PrologOutputProcessor


class PrologException(Exception):
    def __init__(self, msg, file_content, *args):
        msg += "\n\n\n"
        msg += "================= COMPILED SCRIPT ================= \n"
        msg += str(file_content)
        super().__init__(msg, *args)


class HzzProlog:
    def __init__(self, script_file_abs_path: str, file_content=None, delete_temp_files=True):
        self.last_processed_file_content = None
        self.prolog_script_context = None
        if script_file_abs_path is not None:
            self.main_prolog_script = PrologTemplatePreprocessor(
                script_file_abs_path, file_content=file_content, delete_temp_files=delete_temp_files
            )
            self.prolog_script_context = PrologTemplateProcessingContext(self.main_prolog_script,
                                                                         delete_temp_files=delete_temp_files)
        self.custom_regex_tokenizer: list[tuple[int, str]] = []
        self.last_stdout = None

    def add_new_regex(self, priority_rank, regex):
        self.custom_regex_tokenizer.append((priority_rank, regex))
        self.custom_regex_tokenizer.sort(key=lambda x: x[0])

    def add_facts(self, template_variable_name: str, fact_definitions: list[str]):
        return self.main_prolog_script.add_facts(template_variable_name, fact_definitions)

    def add_fact(self, template_variable_name: str, fact_definition: str):
        return self.main_prolog_script.add_fact(template_variable_name, fact_definition)

    def reset(self):
        args = ["swipl", '--quiet']
        if self.prolog_script_context is not None:
            self.prolog_script_context.reset()
            script_file_name = self.prolog_script_context.save_injected_script_to_file()
            with open(script_file_name, "r") as f:
                self.last_processed_file_content = f.read()
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
        output, err_output = self.query_raw(query, approx_number_of_output=approx_number_of_output)
        self.last_stdout = output

        if err_output:
            if self._error_is_because_too_few_semi_colon(err_output):
                output += "BACKTRACK false."
            elif self._invalid_script_error(err_output):
                raise PrologException(err_output, self.last_processed_file_content)
            elif not self._error_is_because_too_much_semicolon(err_output):
                raise PrologException(err_output, self.last_processed_file_content)

        output_processor = PrologOutputProcessor(output, additional_regex=self.custom_regex_tokenizer)
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
    def _invalid_script_error(self, err_msg):
        file_name = self.main_prolog_script.temp_file_name
        return file_name.lower() in err_msg.lower()


class PrologTemplateProcessingContext:
    def __init__(self, main_template: PrologTemplatePreprocessor, delete_temp_files=True):
        self.main_template = main_template
        self.templates: list[PrologTemplatePreprocessor] = [main_template]
        self.delete_temp_files = delete_temp_files

    def contains(self, other_template: PrologTemplatePreprocessor):
        for template in self.templates:
            if template.script_file_abs_path == other_template.script_file_abs_path:
                return True
            if hash(template) == hash(other_template):
                return True
        return False

    def add_template(self, template: PrologTemplatePreprocessor):
        self.templates.append(template)

    def reset(self):
        self.main_template.reset()

    def save_injected_script_to_file(self, template: PrologTemplatePreprocessor=None, included_templates=None):
        if included_templates is None:
            included_templates = {}  # key: hash of the original template, value: temporary name for it
        if template is None:
            template = self.main_template
        if hash(template) in included_templates:
            return included_templates[hash(template)]

        included_templates[hash(template)] = template.temp_file_path

        generator = template.get_and_replace_imports()  # SIDE EFFECT
        while True:
            try:
                import_file_name = next(generator)
            except StopIteration:
                break
            directory_name = os.path.dirname(template.script_file_abs_path)
            abs_imported_file_path = os.path.join(directory_name, import_file_name)

            imported_template = PrologTemplatePreprocessor(
                f"{abs_imported_file_path}.pl", delete_temp_files=self.delete_temp_files)
            self.add_template(imported_template)  # to prevent imported_template.__del__ from being called.
            processed_file_name = self.save_injected_script_to_file(imported_template)
            processed_file_name = os.path.basename(processed_file_name)
            processed_file_name = os.path.splitext(processed_file_name)[0]
            try:
                generator.send(processed_file_name)
            except StopIteration:
                break
        template.save_injected_script_to_file()  # SIDE EFFECT
        return template.temp_file_path



class PrologTemplatePreprocessor:
    def __init__(self, script_file_abs_path: str, file_content=None, delete_temp_files=True):
        self.facts = {}
        assert os.path.isabs(script_file_abs_path)
        self.script_file_abs_path = script_file_abs_path
        self.temp_folders = os.path.join(self.script_folder_abs_path, "temp")

        self.original_file_content = file_content
        if file_content is None:
            with open(script_file_abs_path) as f:
                self.original_file_content = f.read()
        self.created_temp_files = set()
        self.delete_temp_files = delete_temp_files

        self.temp_file_path = None  # see self.reset() for its initialization
        self.script_file_content = None
        self.reset()
        self.before_preprocessed_script_hash = hash(self.script_file_content)

    def __repr__(self):
        return f"<<{self.temp_file_path}>>"

    @property
    def script_folder_abs_path(self):
        return os.path.dirname(self.script_file_abs_path)

    @property
    def temp_file_name(self):
        return os.path.basename(self.temp_file_path)

    def reset(self):
        self.script_file_content = self.original_file_content
        self.load_temporary_file_name()

    def get_and_replace_imports(self):
        patterns = [
            (":-\\s*\\[([a-zA-Z0-9]+)\\]\\s*\\.", lambda x: f"[{x}]"),
            (":-\\s*consult\\(([a-zA-Z0-9]+)\\)\\s*\\.", lambda x: f"consult({x})"),
             (":-\\s*ensure_loaded\\(([a-zA-Z0-9]+)\\)\\s*\\.", lambda x: f"ensure_loaded({x})"),
        ]
        file_content = self.script_file_content
        for pattern, import_transformer in patterns:
            temp_file_content = []

            tokens = tokenize_using_regex(re.compile(pattern), file_content)
            for token in tokens:
                match = re.match(pattern, token)
                if not match:
                    temp_file_content.append(token)
                    continue
                original_imported_file_name = match.group(1)
                modified_imported_file_name = yield original_imported_file_name

                import_syntax = import_transformer(modified_imported_file_name)
                assert len({".", "/", "\\"} & set(import_syntax)) == 0
                import_syntax = f":- {import_syntax}."
                temp_file_content.append(import_syntax)
            file_content = "".join(temp_file_content)
        self.script_file_content = file_content

    def save_injected_script_to_file(self):
        print(os.getcwd())
        create_dir_if_not_exist(self.temp_folders)

        file_content = self.script_file_content
        file_content = self.remove_ignored(file_content)
        file_content = self.inject_facts(file_content)

        with open(self.temp_file_path, "w") as f:
            f.write(file_content)
        self.created_temp_files.add(self.temp_file_path)

    def load_temporary_file_name(self):
        file_name = [chr(randint(65, 65 + 26 - 1)) for _ in range(10)]
        file_name = "".join(file_name)
        original_file_name = os.path.basename(self.script_file_abs_path).replace(" ", "_")
        original_file_name = os.path.splitext(original_file_name)[0]
        file_name = f"{original_file_name}_{file_name}.pl"

        ret = os.path.join(self.temp_folders, file_name)
        self.temp_file_path = ret

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

    def add_facts(self, template_variable_name: str, fact_definitions: list[str]):
        for fact_definition in fact_definitions:
            self.add_fact(template_variable_name=template_variable_name, fact_definition=fact_definition)

    def add_fact(self, template_variable_name: str, fact_definition: str):
        if template_variable_name not in self.facts:
            self.facts[template_variable_name] = []
        if not fact_definition.endswith("."):
            fact_definition += "."
        self.facts[template_variable_name].append(fact_definition)

    def inject_facts(self, template_prolog_content: str) -> str:
        for key, facts in self.facts.items():
            facts = "\n" + "\n".join(facts) + "\n"
            template_prolog_content = template_prolog_content.replace("{{"+key+"}}", facts)
        return template_prolog_content

    def __del__(self):
        if not self.delete_temp_files:
            return
        for file in self.created_temp_files:
            os.remove(file)

    def __hash__(self):
        return self.before_preprocessed_script_hash


def tokenize_using_regex(regex_pattern: re.Pattern, string):
    matches_obj = list(regex_pattern.finditer(string))
    ret = []
    curr_index = 0
    for match in matches_obj:
        start_nonmatching = curr_index
        start_matching = match.span()[0]
        end_matching = match.span()[1]
        ret.append(string[start_nonmatching: start_matching])  # non-matching string
        ret.append(string[start_matching: end_matching])  # non-matching string

        curr_index = end_matching
    ret.append(string[curr_index: ])  # remaining
    return ret




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