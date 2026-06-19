# Create and modify argument sets, then make an argument grid for batch simulation

An argument set contains the descriptions of the relevant variables in a
batch simulation. Use `new_arg_set()` to create an `arg_set` object, and
use `add_arg_ele()` to add an element to the `arg_set`. After adding all
elements in the argument set, use `make_arg_grid()` to make an argument
grid that can be used directly for running batch simulation.

## Usage

``` r
new_arg_set()

add_arg_ele(arg_set, arg_name, ele_name, start, end, by)

nele(arg_set)

narg(arg_set)

# S3 method for class 'arg_set'
print(x, detail = FALSE, ...)

make_arg_grid(arg_set)

# S3 method for class 'arg_grid'
print(x, detail = FALSE, ...)
```

## Arguments

- arg_set:

  An `arg_set` object.

- arg_name, ele_name:

  The name of the argument and its element in the simulation function

- start, end, by:

  The data points where you want to test the variables. Passed to `seq`.

- x:

  An `arg_set` object

- detail:

  Do you want to print the object details as a full list?

- ...:

  Not in use.

## Value

`new_arg_set()` returns an `arg_set` object.

`add_arg_ele()` returns an `arg_set` object.

`nele()` returns an integer.

`narg()` returns an integer.

`make_arg_gird()` returns an `arg_grid` object.

## Functions

- `new_arg_set()`: Create an `arg_set`.

- `add_arg_ele()`: Add an element to an `arg_set`.

- `nele()`: The number of elements in an `arg_set`.

- `narg()`: The number of arguments in an `arg_set`.

- `print(arg_set)`: Print an `arg_set` object.

- `make_arg_grid()`: Make an argument grid from an argument set.

- `print(arg_grid)`: Print an `arg_grid` object

## See also

[`batch_simulation()`](https://sciurus365.github.io/simlandr/reference/batch_simulation.md)
for running batch simulation and a concrete example.
