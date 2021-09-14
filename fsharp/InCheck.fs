module kingincheck.InCheck

type Movement = {RowMovement: int; ColumnMovement: int}
type Location = {Row: int; Column: int}
let downLeft = {RowMovement = 1; ColumnMovement = -1}
let downRight = {RowMovement = 1; ColumnMovement = 1}

let makeLocation row column = {Row = row; Column = column}

let makeSquares rowIndex row =
    let makeSquare columnIndex piece = (makeLocation rowIndex columnIndex, piece)
    row |> Seq.mapi makeSquare

let hasPiece (_, piece) = piece <> '.'

let moveFrom location movement = makeLocation (location.Row + movement.RowMovement) (location.Column + movement.ColumnMovement) 
let singleMove location movement = seq {moveFrom location movement}
let move movement = fun location -> singleMove location movement

let linesOfAttack =
    ['P', [move(downLeft); move(downRight)];]
    |> Map.ofList

let makePieces rows =
    rows |> Seq.mapi makeSquares
         |> Seq.collect (fun x -> x)
         |> Seq.filter hasPiece
         |> Map.ofSeq

//let isKingInCheck rows =
//    let pieces = makePieces rows
//    let pieceAtLocation location = if pieces.ContainsKey location then pieces.[location] else ' '
//    let attackingLocation 
