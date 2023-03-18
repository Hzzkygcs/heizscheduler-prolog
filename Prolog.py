import subprocess

from PrologOutputProcessor import PrologOutputProcessor


class PrologException(Exception):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


class Prolog:
    def __init__(self, script_file_name):
        self.p = subprocess.Popen(
            ["swipl", '--quiet', script_file_name],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )

    def query_raw(self, query, approx_number_of_output=1):
        prefix = ", write('BACKTRACK').\n" + ";\n" * (approx_number_of_output - 1)
        stdout, stderr = self.p.communicate(input=query+prefix)
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



prolog = Prolog("inner.pl")
ret = prolog.query("append(A, B, [1, 2])")
print(ret)

