# mazes

```
> cabal exec mazes -- --help
Usage: mazes [-c|--columns ARG] [-r|--rows ARG] [-s|--cellShape ARG] 
             [-w|--width ARG] [-h|--height ARG] [-a|--animate] [-n|--name ARG]

  Generate mazes

Available options:
  -c,--columns ARG         The number of columns of cells in the maze.
  -r,--rows ARG            The number of rows of cells in the maze.
  -s,--cellShape ARG       The type of maze grid. Values are 'square' and 'hex'.
  -w,--width ARG           The desired width of the output.
  -h,--height ARG          The desired height of the output.
  -a,--animate             When set the output will be an animated gif
                           demonstrating the construction of the maze.
  -n,--name ARG            A name for the image. Used to name the output file.
  -h,--help                Show this help text
```

## examples

### square grid

> cabal exec mazes -- -c 40 -r 40 -w 800

![square maze example](https://github.com/jgraydus/mazes/blob/main/examples/square.svg)

### hex grid

> cabal exec mazes -- -c 35 -r 35 -w 800 -s hex

![hex maze example](https://github.com/jgraydus/mazes/blob/main/examples/hex.svg)

### animation

> cabal exec mazes -- -c 8 -r 8 -w 300 -a

![animation example](https://github.com/jgraydus/mazes/blob/main/examples/animation.gif)

