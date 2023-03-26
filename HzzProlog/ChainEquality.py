from typing import Optional


class ChainedEquality:
    def __init__(self, variable_names, constant_value: Optional = None):
        self.variable_names = list(variable_names)
        self.constant_value = constant_value

    def __eq__(self, other):
        if not isinstance(other, ChainedEquality):
            return False
        same_constant_value = self.constant_value == other.constant_value
        same_variables = set(self.variable_names) == set(other.variable_names)
        return same_constant_value and same_variables

    def __repr__(self):
        operands = self.variable_names
        if self.constant_value is not None:
            operands = operands + [self.constant_value]
        return " = ".join(operands)
