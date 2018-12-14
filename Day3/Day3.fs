module Day3

open CommonLib.Files

// X - Visited once
// D - Visited at least twice
// N - Not visited

type Claim = { 
    Id: string;
    X: int;
    Y: int;
    Height: int;
    Width: int
    }

let parseLineIntoClaim (str:string) =
    let id = str.[str.IndexOf('#') + 1 .. str.IndexOf(" @") - 1]
    let y = str.[str.IndexOf(" @") + 2 .. str.IndexOf(',') - 1] |> int
    let x = str.[str.IndexOf(',') + 1 .. str.IndexOf(':') - 1] |> int
    let width = str.[str.IndexOf(": ") + 2 .. str.IndexOf('x') - 1] |> int
    let height = str.[str.IndexOf('x') + 1 .. str.Length - 1] |> int
    { Id = id; Y = y; X = x; Width = width; Height = height }

let drawRectangleOnMatrix (matrix:char[,]) claim =
    for i = claim.X to (claim.X + claim.Height - 1) do
        for j = claim.Y to (claim.Y + claim.Width - 1) do
            if (matrix.[i,j] = 'X') then
                Array2D.set matrix i j 'D'
            else if (matrix.[i,j] = 'N') then
                Array2D.set matrix i j 'X'

let getNumberOfDuplicatedClaims (matrix:char[,]) =
    let mutable duplicatedCount = 0
    let increaseCount chr =
        if chr = 'D' then
            duplicatedCount <- duplicatedCount + 1
    Array2D.iter increaseCount matrix
    duplicatedCount

let getFilledMatrix (claimArr:Claim[]) =
    let matrix = Array2D.init 1000 1000 (fun i j -> 'N')
    Array.iter (drawRectangleOnMatrix matrix) claimArr
    matrix

let getDuplicatesFromClaimArray (claimArr:Claim[]) =
    getFilledMatrix claimArr
    |> getNumberOfDuplicatedClaims

let getNonDuplicateClaim (claimArr:Claim[]) = 
    let mutable freeClaim = "" 
    let isClaimOccupied (matrix:char[,]) claim =
        let mutable flag = true
        for i = claim.X to (claim.X + claim.Height - 1) do
            for j = claim.Y to (claim.Y + claim.Width - 1) do
                if (matrix.[i,j] = 'D') then 
                    flag <- false
        if (flag) then freeClaim <- claim.Id

    Array.iter (isClaimOccupied (getFilledMatrix claimArr)) claimArr
    freeClaim

let getDuplicatesFromFile fileName =
    getLineValuesFromFilePath fileName
    |> Array.map parseLineIntoClaim
    |> getDuplicatesFromClaimArray

let getNonDuplicateClaimFromFile fileName =
    getLineValuesFromFilePath fileName
    |> Array.map parseLineIntoClaim
    |> getNonDuplicateClaim