from __future__ import annotations
from typing import Optional, Union


class ChainedEquality:
    def __init__(self, variable_names: list[str], constant_value: Optional = None):
        assert all(map(lambda x: isinstance(x, str), variable_names))
        assert not isinstance(constant_value, ChainedEquality)
        self.__variable_names = list(variable_names)
        self.__constant_value = constant_value

    @property
    def constant_value(self):
        return self.__constant_value
    @constant_value.setter
    def constant_value(self, value):
        assert not isinstance(value, ChainedEquality)
        self.__constant_value = value
    @property
    def variable_names(self):
        return list(self.__variable_names)
    @variable_names.setter
    def variable_names(self, value):
        assert all(map(lambda x: isinstance(x, str), value))
        self.__variable_names = value

    def __eq__(self, other):
        if not isinstance(other, ChainedEquality):
            return False
        same_constant_value = self.constant_value == other.constant_value
        same_variables = set(self.variable_names) == set(other.variable_names)
        return same_constant_value and same_variables

    def __repr__(self):
        operands = self.variable_names
        if self.constant_value is not None:
            operands = operands + [str(self.constant_value)]
        return " = ".join(operands)

    @classmethod
    def unify(cls, variable_value_mapping: dict[str, Union[int, str, ChainedEquality]],
              first, second):
        first_is_an_equation = isinstance(first, ChainedEquality)
        second_is_an_equation = isinstance(second, ChainedEquality)
        assert first_is_an_equation or second_is_an_equation

        either_is_an_instantiation = not (first_is_an_equation and second_is_an_equation)
        if either_is_an_instantiation:
            if not first_is_an_equation:
                first, second = second, first
            assert isinstance(first, ChainedEquality)
            assert first.constant_value is None or first.constant_value == second
            first.constant_value = second
            apply_to_defined_variables(variable_value_mapping, first.variable_names, first)
            return

        unified_equality = cls.unify_both_equality(first, second)
        apply_to_defined_variables(variable_value_mapping, unified_equality.variable_names, unified_equality)

    @staticmethod
    def unify_both_equality(first, second):
        first_is_an_equation = not isinstance(first, ChainedEquality)
        second_is_an_equation = not isinstance(second, ChainedEquality)
        assert first_is_an_equation and second_is_an_equation
        first: ChainedEquality
        second: ChainedEquality

        theyre_equal = first.constant_value == second.constant_value
        any_of_them_is_instantiated = (first.constant_value is None) or (second.constant_value is None)
        assert theyre_equal or any_of_them_is_instantiated

        first_variables = first.variable_names
        second_variables = second.variable_names
        return ChainedEquality(first_variables + second_variables,
                               first.constant_value or second.constant_value)

    @staticmethod
    def define_equality(variable_value_mapping, variable_names: list[str], value):
        assert not isinstance(value, ChainedEquality)
        equality = ChainedEquality(variable_names, value)
        for variable_name in variable_names:
            variable_value_mapping[variable_name] = equality
        return variable_value_mapping


def apply_to_defined_variables(defined_variables: dict[str, Union[int, str, ChainedEquality]],
                               new_variables: list[str], new_value):
    for new_variable in new_variables:
        defined_variables[new_variable] = new_value
    return defined_variables
