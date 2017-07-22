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
* S - Rotate (Not implemented yet)
* Any Other - Move piece down

### TODO
* Piece Rotation
* Detect game finished
* Clean up row once filled
* Keep Score

### Known Issues
* When piece slides sideways twice in a row into a wall or another piece it will stop moving down