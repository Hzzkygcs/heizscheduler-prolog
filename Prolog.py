import subprocess

from PrologOutputProcessor import PrologOutputProcessor


class PrologException(Exception):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


class Prolog:
    def __init__(self, script_file_name):
        self.script_file_name = script_file_name

    def reset(self):
        args = ["swipl", '--quiet']
        if self.script_file_name is not None:
            args.append(self.script_file_name)

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
            f"write('{self.start_token}').",
        ])

    def suffix(self, approx_number_of_output):
        return ", write('BACKTRACK').\n" + ";\n" * (approx_number_of_output - 1)

    def query_raw(self, query, approx_number_of_output=1):
        self.reset()
        assert not query.endswith("."), "query should NOT be ended with punctuation"

        query = query + self.suffix(approx_number_of_output)
        stdout, stderr = self.p.communicate(input=query)
        return stdout, stderr

    def query(self, query, approx_number_of_output=100):
        stdout, stderr = self.query_raw(query, approx_number_of_output=approx_number_of_output)

        if stderr:
            if self._error_is_because_too_few_semi_colon(stderr):
                stdout += "BACKTRACK false."
            elif not self._error_is_because_too_much_semicolon(stderr):
                raise PrologException(stderr)

        output_processor = PrologOutputProcessor(stdout)
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

