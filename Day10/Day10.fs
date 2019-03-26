module Day10

open CommonLib.Files
open System

type Star = {
    XVelocity : int
    YVelocity : int
    XPosition : int
    YPosition : int
}

let advanceStar star =
        { star with XPosition = star.XPosition + star.XVelocity
                    YPosition = star.YPosition + star.YVelocity }

let getNewStarMap (currentMap) = 
    Array.map advanceStar currentMap

let printStarMap map =
    let bufferX = (Array.minBy (fun x -> x.XPosition) map).XPosition
    let bufferY = (Array.minBy (fun x -> x.YPosition) map).YPosition
    let maxX = (Array.maxBy (fun x -> x.XPosition) map).XPosition
    let maxY = (Array.maxBy (fun x -> x.YPosition) map).YPosition
    if (maxX - bufferX < 100 && maxY - bufferY < 50 ) then
        let starMap = Array2D.init (maxX - bufferX + 1) (maxY - bufferY + 1) (fun _ _ -> ' ')
        Array.iter (fun x -> Array2D.set starMap (x.XPosition - bufferX) (x.YPosition - bufferY) '*') map

        for r = 0 to Array2D.length2 starMap - 1 do
            printfn "%A" <| String.Concat starMap.[*, r]

let getStarMapWidth map =
    let minY = (Array.minBy (fun x -> x.YPosition) map).YPosition 
    let maxY = (Array.maxBy (fun x -> x.YPosition) map).YPosition
    maxY - minY

let rec getMessage currentSecond minWidth map =
    let currentMap = getNewStarMap map
    let currentWidth = getStarMapWidth currentMap
    if (minWidth = 0 || currentWidth < minWidth) then
        printStarMap currentMap
        getMessage (currentSecond + 1) currentWidth currentMap
    else
        printfn "Second %A" <| currentSecond - 1
        0

let getStarFromStr (str:string) = 
    let posX = str.[10 .. 15] |> int
    let posY = str.[18 .. 23] |> int
    let velX = str.[36 .. 37] |> int
    let velY = str.[40 .. 41] |> int
    { XVelocity = velX; YVelocity = velY; XPosition = posX; YPosition = posY}

let getMessageFromFile fileName =
    getLineValuesFromFilePath fileName
    |> Array.map getStarFromStr
    |> getMessage 1 0
