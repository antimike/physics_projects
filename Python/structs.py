import collections

Data_With_Labels = collections.namedtuple('Data_With_Labels', ['x', 'y', 'labels'])
Data_With_Errors = collections.namedtuple('Data_With_Errors', ['x', 'y', 'ebars'])
Regression_Result = collections.namedtuple('Regression_Result', ['params', 'std_devs'])
Linear_Regression = collections.namedtuple(
    'Linear_Regression', ['slope', 'intercept', 'r_value', 'p_value', 'std_err']
)
