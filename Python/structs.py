import collections

Data_With_Labels = collections.namedtuple('Data_With_Labels', ['x', 'y', 'labels'])
Data_With_Errors = collections.namedtuple('Data_With_Errors', ['x', 'y', 'ebars'])
Regression_Result = collections.namedtuple('Regression_Result', ['params', 'std_devs'])
Linear_Regression = collections.namedtuple(
    'Linear_Regression', ['slope', 'intercept', 'r_value', 'p_value', 'std_err']
)
Text_Attributes = collections.namedtuple(
    'Text_Attributes',
    ['size', 'weight', 'ha', 'va'],
    defaults=[Text_Size.MEDIUM, Text_Weight.NORMAL, Text_Align.CENTER, Text_Align.CENTER]
)
Plot_Attributes = collections.namedtuple(
    'Plot_Attributes',
    ['marker', 'color', 'ebar_color', 'label', 'linestyle']
    defaults=[Markers.POINT, Colors.BLUE, Colors.RED, Linestyles.SOLID]
)

class Colors(Enum):
    RED: 'r'
    BLUE: 'b'
    CYAN: 'c'
    MAGENTA: 'm'
    YELLOW: 'y'
    BLACK: 'b'
    WHITE: 'w'

class Text_Weight(Enum):
    NORMAL: 'normal'
    BOLD: 'bold'
    ULTRALIGHT: 'ultralight'
    LIGHT: 'light'
    REGULAR: 'regular'
    BOOK: 'book'
    MEDIUM: 'medium'
    ROMAN: 'roman'
    SEMIBOLD: 'semibold'
    DEMIBOLD: 'demibold'
    DEMI: 'demi'
    HEAVY: 'heavy'
    EXTRA_BOLD: 'extra bold'
    BLACK: 'black'

class Text_Size(Enum):
    XX_SMALL: 'xx-small'
    X_SMALL: 'x-small'
    SMALL: 'small'
    MEDIUM: 'medium'
    LARGE: 'large'
    X_LARGE: 'x-large'
    XX_LARGE: 'xx-large'

class Text_Align(Enum):
    CENTER: 'center'
    LEFT: 'left'
    RIGHT: 'right'
    TOP: 'top'
    BOTTOM: 'bottom'
    BASELINE: 'baseline'

class Markers(Enum):
    POINT: '.'
    PIXEL: ','
    CIRCLE: 'o'
    TRIANGLE_UP: '^'
    TRIANGLE_DOWN: 'V'
    TRI_DOWN: '1'
    TRI_UP:'2'
    TRI_LEFT: '3'
    TRI_RIGHT: '4'
    OCTAGON: '8'
    SQUARE: 's'
    PENTAGON: 'p'
    FILLED_PLUS: 'P'
    PLUS: '+'
    HEX_SIDE: 'H'
    HEX_UP: 'h'
    STAR: '*'
    X: 'x'
    X_FILLED: 'X'
    DIAMOND_THICK: 'D'
    DIAMOND_THIN: 'd'
    VLINE: '|'
    HLINE: '-'
    NONE: None

class Linestyles(Enum):
    SOLID: '-'
    DASHED: '--'
    DOTTED: ':'
    DASHDOT: '-.'
