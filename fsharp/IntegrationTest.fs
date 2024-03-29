module kingincheck.IntegrationTest

open NUnit.Framework
open kingincheck.InCheck

[<Test>]
let toBeImplemented() = Assert.AreEqual(0, square(2))

//checkByPawn
//           ["........";
//            "........";
//            "........";
//            "........";
//            "....P...";
//            "...K....";
//            "........";
//            "........"]

//checkByKnight
//           ["........";
//            "........";
//            "........";
//            "........";
//            "........";
//            ".K......";
//            "PP......";
//            "N......."]

//checkByBishop
//           [".......B";
//            "........";
//            "........";
//            "........";
//            "........";
//            "........";
//            "........";
//            "K......."]

//checkByRook
//           ["........";
//            "........";
//            "........";
//            "........";
//            "..K..R..";
//            "........";
//            "........";
//            "........"]

//checkByQueen
//           ["........";
//            "........";
//            "........";
//            ".Q.....K";
//            "........";
//            "........";
//            "........";
//            "........"]

//KingAlone
//           ["........";
//            "........";
//            "........";
//            "....K...";
//            "........";
//            "........";
//            "........";
//            "........"]

//noChecks
//           ["........";
//            "........";
//            "Q....N.Q";
//            "....K...";
//            "........";
//            "........";
//            "........";
//            "........"]

//blockedByPiece
//           ["........";
//            "........";
//            "R....P.K";
//            "........";
//            "........";
//            "........";
//            "........";
//            "........"]
