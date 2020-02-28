import matplotlib.pyplot as plot
import numpy as np

def initialize_figure(title, xlabel, ylabel, *comments):
    fig, grid = plot.subplots()
    plot.title(title, fontweight='bold', fontsize=14)
    grid.set_xlabel(xlabel)
    grid.set_ylabel(ylabel)
    caption = '\n'.join(comments)
    fig.subplots_adjust(bottom=.8)
    plot.figtext(0.5, 0.01, caption, wrap=True, ha='center', va='top', fontsize=12)
    return fig, grid

def show_figure(figure, grid, legend_pos):
    grid.legend(loc=legend_pos)
    figure.tight_layout()
    plot.show()

def plot_points_with_errorbars(grid, data, marker, color, ebar_color, label, linestyle):
    grid.errorbar(x=data.x, y=data.y, xerr=None, yerr=data.ebars, ecolor=ebar_color,
        color=color, label=label, marker=marker, linestyle=linestyle)
    pass

def plot_function(grid, fn, color, label, linestyle):
    min, max = plot.xlim()
    fn_x = np.linspace(min, max, 100)
    grid.plot(fn_x, fn(fn_x), color=color, label=label, linestyle=linestyle)
    pass

def annotate_points(grid, points, marker, color):
    for x, y, label in zip(points.x, points.y, points.labels):
        plot.annotate(label, (x, y), textcoords='offset points', xytext=(0,10), ha='center')
    pass

def annotate_point(grid, point, marker, color):
    plot.plot(point.x, point.y, color=color, marker=marker)
    plot.annotate(point.labels, (point.x, point.y), textcoords='offset points', xytext=(0,10), ha='center')
    pass

def initialize_new_figure(title, num_subplot_rows=1, num_subplot_cols=1):
    pass

def add_subplot_to_figure(fig):
    pass

def add_comments_to_subplot(sp):
    pass

def add_twin_to_subplot(sp):
    pass

def create_subplot(figure):
    pass

def show_all_figures(tight_layout=True):
    pass
