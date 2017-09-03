# Tetris

## Tetris clone written in Elm

### Building 

Compile into a single html file:

```
elm-make --output out/index.html src/Tetris.elm # same as build script included
```

Run tests:

```
elm test
```

### Playing
* A - Slide piece left
* D - Slide piece right
* Q - Rotate
* Any Other - Move piece down

### TODO
* Detect game finished