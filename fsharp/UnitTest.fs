module kingincheck.UnitTest

open NUnit.Framework
open kingincheck.InCheck

[<Test>]
let makePiecesWithPawnAndKing() =
    let rows =
            ["........";
            "........";
            "........";
            "........";
            "....P...";
            "...K....";
            "........";
            "........"]

    let piecesByLocation = makePieces rows

    let expectedPiecesByLocation = Map.empty.Add(makeLocation 4 4, 'P').Add(makeLocation 5 3, 'K')
    Assert.AreEqual(expectedPiecesByLocation, piecesByLocation)

[<Test>]
let shouldGenerateCorrectAttacksForPawn() =
    let generateAttacks attackGenerator = attackGenerator {Row = 3; Column = 3;}
    let attacks = linesOfAttack.['P'] |> Seq.map generateAttacks |> Seq.collect (fun x -> x)
    let expectedAttacks = seq {makeLocation 4 2; makeLocation 4 4}
    Assert.That(attacks, Is.EquivalentTo(expectedAttacks))


[<Test>]
let isPieceAttackingTheKing() =
    let rows =
            ["........";
            "........";
            "........";
            "........";
            "....P...";
            "...K....";
            "........";
            "........"]
    let piecesByLocation = makePieces rows
    
    let generateAttacks attackGenerator = attackGenerator {Row = 4; Column = 4;}
    let attacks = linesOfAttack.['P'] |> Seq.map generateAttacks |> Seq.collect (fun x -> x)
    
    let hasKingAtLocation location = piecesByLocation.ContainsKey location && piecesByLocation.Item location = 'K'
    let isAttackKing = attacks |> Seq.exists hasKingAtLocation 
    
    Assert.AreEqual(isAttackKing, true);


[<Test>]
let isPieceNotAttackingTheKing() =
    let rows =
            ["........";
            "........";
            "........";
            "........";
            "....P...";
            "........";
            "....K...";
            "........"]
    let piecesByLocation = makePieces rows
    
    let generateAttacks attackGenerator = attackGenerator {Row = 4; Column = 4;}
    let attacks = linesOfAttack.['P'] |> Seq.map generateAttacks |> Seq.collect (fun x -> x)
    
    let hasKingAtLocation location = piecesByLocation.ContainsKey location && piecesByLocation.Item location = 'K'
    let isAttackKing = attacks |> Seq.exists hasKingAtLocation 
    
    Assert.AreEqual(isAttackKing, false);
